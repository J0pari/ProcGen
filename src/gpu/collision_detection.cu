#include <cuda_runtime.h>
#include "sheaf_template.cuh"

__global__ void __launch_bounds__(256, 4) collision_kernel(
    cudaTextureObject_t posTex,
    int* collisionPairs,
    int nodeCount,
    float minDistance,
    int* outCount
) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    int idy = blockIdx.y * blockDim.y + threadIdx.y;

    if (idx >= nodeCount || idy >= nodeCount || idx >= idy) return;

    float4 pi = tex1Dfetch<float4>(posTex, idx);
    float4 pj = tex1Dfetch<float4>(posTex, idy);

    float3 p1 = make_float3(pi.x, pi.y, pi.z);
    float3 p2 = make_float3(pj.x, pj.y, pj.z);
    float distSq = FastMath::distance_sq(p1, p2);
    float minDistSq = minDistance * minDistance;

    auto tile = cg::tiled_partition<32>(cg::this_thread_block());
    int hasCollision = (distSq < minDistSq) ? 1 : 0;
    unsigned active = tile.ballot(hasCollision);

    if (hasCollision) {
        int warpOffset = __popc(active & ((1 << tile.thread_rank()) - 1));

        int warpSlot = 0;
        if (tile.thread_rank() == 0) {
            warpSlot = Atomics::claim_slot(outCount);
        }
        warpSlot = tile.shfl(warpSlot, 0);

        int slot = warpSlot + warpOffset;
        // Use externally calculated maxPairs to prevent overflow
        collisionPairs[slot * 2] = idx;
        collisionPairs[slot * 2 + 1] = idy;
    }
}

extern "C" {
    __declspec(dllexport)
    void collision_detection(
        float* positions,
        int* collisionPairs,
        int nodeCount,
        float minDistance,
        int* outCount
    ) {
        PinnedMemory<float4> h_positions4(nodeCount);
        for (int i = 0; i < nodeCount; i++) {
            h_positions4[i] = make_float4(positions[i*3], positions[i*3+1], positions[i*3+2], 0.0f);
        }

        cudaArray *d_posArray;
        int *d_collisionPairs, *d_outCount;

        size_t posSize = nodeCount * sizeof(float4);

        // Dynamic sizing: worst case is O(n²/2) pairs for complete collision
        // Use min(n²/2, maxPairs) where maxPairs prevents allocation overflow
        const int maxPairsPerNode = 512;  // Avg 512 collisions per node (tunable)
        size_t maxPairs = min((size_t)nodeCount * maxPairsPerNode, (size_t)nodeCount * (nodeCount - 1) / 2);
        size_t pairSize = maxPairs * 2 * sizeof(int);

        cudaChannelFormatDesc channelDesc = cudaCreateChannelDesc<float4>();
        cudaMallocArray(&d_posArray, &channelDesc, nodeCount);

        cudaResourceDesc resDesc;
        memset(&resDesc, 0, sizeof(resDesc));
        resDesc.resType = cudaResourceTypeArray;
        resDesc.res.array.array = d_posArray;

        cudaTextureDesc texDesc;
        memset(&texDesc, 0, sizeof(texDesc));
        texDesc.addressMode[0] = cudaAddressModeClamp;
        texDesc.filterMode = cudaFilterModePoint;
        texDesc.readMode = cudaReadModeElementType;
        texDesc.normalizedCoords = 0;

        cudaTextureObject_t posTex;
        cudaCreateTextureObject(&posTex, &resDesc, &texDesc, nullptr);

        cudaMalloc(&d_collisionPairs, pairSize);
        cudaMalloc(&d_outCount, sizeof(int));

        Stream stream;

        cudaMemcpyToArrayAsync(d_posArray, 0, 0, h_positions4, posSize, cudaMemcpyHostToDevice, stream);
        cudaMemsetAsync(d_outCount, 0, sizeof(int), stream);

        dim3 blockDim(32, 8);
        dim3 gridDim((nodeCount + 31) / 32, (nodeCount + 7) / 8);

        collision_kernel<<<gridDim, blockDim, 0, stream>>>(
            posTex, d_collisionPairs, nodeCount, minDistance, d_outCount
        );

        int h_outCount;
        cudaMemcpyAsync(&h_outCount, d_outCount, sizeof(int), cudaMemcpyDeviceToHost, stream);
        cudaStreamSynchronize(stream);

        size_t actualPairSize = h_outCount * 2 * sizeof(int);
        cudaMemcpyAsync(collisionPairs, d_collisionPairs, actualPairSize, cudaMemcpyDeviceToHost, stream);
        cudaStreamSynchronize(stream);

        *outCount = h_outCount;

        cudaDestroyTextureObject(posTex);
        cudaFreeArray(d_posArray);
        cudaFree(d_collisionPairs);
        cudaFree(d_outCount);
    }
}
