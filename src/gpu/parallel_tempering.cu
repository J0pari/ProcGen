#include <cuda_runtime.h>
#include <curand_kernel.h>
#include "sheaf_template.cuh"

// Initialize RNG states once, persist across iterations
__global__ void init_rng_kernel(
    curandState_t* states,
    int stateCount,
    unsigned long long baseSeed
) {
    int idx = GridStride::thread_id();
    if (idx < stateCount) {
        curand_init(baseSeed + idx, idx, 0, &states[idx]);
    }
}

__global__ void pt_step_kernel(
    float* positions,
    const int* edges,
    const float* temperatures,
    curandState_t* rngStates,
    int nodeCount,
    int edgeCount,
    int chainIdx
) {
    using Sec = Section1D<256>;
    __shared__ float localPos[256 * 3];

    for (int nodeIdx = GridStride::thread_id(); nodeIdx < nodeCount; nodeIdx += GridStride::stride()) {
        int posOffset = chainIdx * nodeCount * 3 + nodeIdx * 3;

        Sec::restrict_flat(localPos, &positions[chainIdx * nodeCount * 3], nodeCount, 3);
        __syncthreads();

        int stateIdx = GridStride::thread_id();
        curandState_t localState = rngStates[stateIdx];

        float temp = temperatures[chainIdx];
        float scale = sqrtf(temp) * 0.1f;

        // Use curand_normal2 for 2x throughput
        float2 rnd = curand_normal2(&localState);

        positions[posOffset + 0] += rnd.x * scale;
        positions[posOffset + 1] += rnd.y * scale;
        positions[posOffset + 2] += curand_normal(&localState) * scale;

        rngStates[stateIdx] = localState;
    }
}


__global__ void compute_cost_kernel(
    const float* __restrict__ positions,
    cudaTextureObject_t edgeTex,
    float* costs,
    int nodeCount,
    int edgeCount,
    int chainIdx
) {
    float threadCost = 0.0f;
    int offset = chainIdx * nodeCount * 3;

    for (int edgeIdx = GridStride::thread_id(); edgeIdx < edgeCount / 2; edgeIdx += GridStride::stride()) {

        int2 edge = tex1Dfetch<int2>(edgeTex, edgeIdx);

        float3 p1 = Section1D<256>::as_position_offset(positions, offset, edge.x);
        float3 p2 = Section1D<256>::as_position_offset(positions, offset, edge.y);

        threadCost += FastMath::distance_sq(p1, p2);
    }

    threadCost = WarpReduce<32>::sum(threadCost);

    if ((threadIdx.x & 31) == 0) {
        atomicAdd(&costs[chainIdx], threadCost);
    }
}

__global__ void replica_exchange_kernel(
    float* positions,
    float* costs,
    const float* temperatures,
    int nodeCount,
    int tempCount,
    curandState_t* rngStates
) {
    int chainIdx = blockIdx.x * 2;
    if (chainIdx + 1 >= tempCount) return;

    __shared__ bool shouldSwap;

    if (threadIdx.x == 0) {
        float T1 = temperatures[chainIdx];
        float T2 = temperatures[chainIdx + 1];
        float C1 = costs[chainIdx];
        float C2 = costs[chainIdx + 1];

        float delta = (__fdividef(1.0f, T1) - __fdividef(1.0f, T2)) * (C1 - C2);

        int stateIdx = blockIdx.x % (gridDim.x * blockDim.x);
        curandState_t localState = rngStates[stateIdx];

        shouldSwap = (delta < 0.0f) || (curand_uniform(&localState) < __expf(-delta));

        rngStates[stateIdx] = localState;
    }
    __syncthreads();

    if (shouldSwap) {
        for (int i = threadIdx.x; i < nodeCount * 3; i += blockDim.x) {
            int idx1 = chainIdx * nodeCount * 3 + i;
            int idx2 = (chainIdx + 1) * nodeCount * 3 + i;

            float temp = positions[idx1];
            positions[idx1] = positions[idx2];
            positions[idx2] = temp;
        }

        if (threadIdx.x == 0) {
            float tempCost = costs[chainIdx];
            costs[chainIdx] = costs[chainIdx + 1];
            costs[chainIdx + 1] = tempCost;
        }
    }
}

extern "C" {
    __declspec(dllexport) void parallel_tempering_step(
        float* positions,
        int* edges,
        float* costs,
        float* temperatures,
        int nodeCount,
        int edgeCount,
        int tempCount
    ) {
        static float *d_positions = nullptr, *d_costs = nullptr, *d_temperatures = nullptr;
        static int2 *d_edges = nullptr;
        static curandState_t *d_rngStates = nullptr;
        static int cachedNodeCount = 0;
        static int cachedEdgeCount = 0;
        static int cachedTempCount = 0;
        static cudaArray* edgeArray = nullptr;
        static cudaTextureObject_t edgeTex = 0;

        size_t posSize = nodeCount * 3 * tempCount * sizeof(float);
        size_t costSize = tempCount * sizeof(float);
        size_t tempSize = tempCount * sizeof(float);
        size_t edgeSize = (edgeCount / 2) * sizeof(int2);

        // Reallocate only if problem size changed
        if (cachedNodeCount != nodeCount || cachedEdgeCount != edgeCount || cachedTempCount != tempCount) {
            if (d_positions) {
                cudaFree(d_positions);
                cudaFree(d_costs);
                cudaFree(d_temperatures);
                cudaFree(d_rngStates);
                if (edgeArray) {
                    cudaDestroyTextureObject(edgeTex);
                    cudaFreeArray(edgeArray);
                }
            }

            cudaMalloc(&d_positions, posSize);
            cudaMalloc(&d_costs, costSize);
            cudaMalloc(&d_temperatures, tempSize);

            // Setup edge texture for cache-optimized lookups
            cudaChannelFormatDesc channelDesc = cudaCreateChannelDesc<int2>();
            cudaMallocArray(&edgeArray, &channelDesc, edgeCount / 2);

            cudaResourceDesc resDesc;
            memset(&resDesc, 0, sizeof(resDesc));
            resDesc.resType = cudaResourceTypeArray;
            resDesc.res.array.array = edgeArray;

            cudaTextureDesc texDesc;
            memset(&texDesc, 0, sizeof(texDesc));
            texDesc.addressMode[0] = cudaAddressModeClamp;
            texDesc.filterMode = cudaFilterModePoint;
            texDesc.readMode = cudaReadModeElementType;
            texDesc.normalizedCoords = 0;

            cudaCreateTextureObject(&edgeTex, &resDesc, &texDesc, nullptr);

            int blockSize = 256;
            int smCount;
            cudaDeviceGetAttribute(&smCount, cudaDevAttrMultiProcessorCount, 0);
            int gridSize = smCount * 4;
            int rngStateCount = gridSize * blockSize;
            cudaMalloc(&d_rngStates, rngStateCount * sizeof(curandState_t));

            // Initialize RNG states once with deterministic per-thread seeds
            unsigned long long seed = 12345ULL;
            init_rng_kernel<<<gridSize, blockSize>>>(d_rngStates, rngStateCount, seed);

            cachedNodeCount = nodeCount;
            cachedEdgeCount = edgeCount;
            cachedTempCount = tempCount;
        }

        Stream stream;

        // Convert edges to int2 format for texture
        int2* h_edges2 = new int2[edgeCount / 2];
        for (int i = 0; i < edgeCount / 2; i++) {
            h_edges2[i] = make_int2(edges[i * 2], edges[i * 2 + 1]);
        }

        cudaMemcpyAsync(d_positions, positions, posSize, cudaMemcpyHostToDevice, stream);
        cudaMemcpyAsync(d_temperatures, temperatures, tempSize, cudaMemcpyHostToDevice, stream);
        cudaMemcpyToArrayAsync(edgeArray, 0, 0, h_edges2, edgeSize, cudaMemcpyHostToDevice, stream);
        cudaMemsetAsync(d_costs, 0, costSize, stream);

        int blockSize = 256;
        int smCount;
        cudaDeviceGetAttribute(&smCount, cudaDevAttrMultiProcessorCount, 0);
        int gridSize = smCount * 4;

        for (int chain = 0; chain < tempCount; chain++) {
            pt_step_kernel<<<gridSize, blockSize, 0, stream>>>(
                d_positions, nullptr, d_temperatures, d_rngStates,
                nodeCount, edgeCount, chain
            );
        }

        for (int chain = 0; chain < tempCount; chain++) {
            int costBlockSize = 256;
            int costGridSize = smCount * 2;
            compute_cost_kernel<<<costGridSize, costBlockSize, 0, stream>>>(
                d_positions, edgeTex, d_costs,
                nodeCount, edgeCount, chain
            );
        }

        delete[] h_edges2;

        replica_exchange_kernel<<<(tempCount + 1) / 2, blockSize, 0, stream>>>(
            d_positions, d_costs, d_temperatures, nodeCount, tempCount, d_rngStates
        );

        cudaMemcpyAsync(positions, d_positions, posSize, cudaMemcpyDeviceToHost, stream);
        cudaMemcpyAsync(costs, d_costs, costSize, cudaMemcpyDeviceToHost, stream);

        cudaStreamSynchronize(stream);
    }
}
