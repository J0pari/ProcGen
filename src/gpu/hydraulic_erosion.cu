#include <cuda_runtime.h>
#include "sheaf_template.cuh"

__global__ void __launch_bounds__(256, 2) erosion_kernel(
    float* __restrict__ heightmap,
    float* __restrict__ sediment,
    float2* __restrict__ waterFlow,
    int resolution,
    float deltaTime,
    float erosionRate,
    float depositionRate
) {
    using Sec = Section2D<16, 1, 1>;
    __shared__ float tileHeight[Sec::PADDED][Sec::PADDED];

    int tx = threadIdx.x;
    int tz = threadIdx.y;
    int x = blockIdx.x * 16 + tx;
    int z = blockIdx.y * 16 + tz;

    Sec::restrict_from_global(tileHeight, heightmap, resolution);
    __syncthreads();

    #ifdef DEBUG_SHEAF_GLUING
    if (tx == 0 || tx == 15 || tz == 0 || tz == 15) {
        bool consistent = Gluing::compatible_on_overlap(
            tileHeight[tz + 1], tileHeight[tz + 1],
            Sec::PADDED, 1e-5f
        );
    }
    #endif

    if (x <= 0 || x >= resolution - 1 || z <= 0 || z >= resolution - 1) return;

    int idx = z * resolution + x;

    float vals[3][3];
    float h = Sec::at(tileHeight, tx, tz);
    vals[1][1] = h;
    vals[0][1] = Sec::at(tileHeight, tx, tz, 0, -1);
    vals[2][1] = Sec::at(tileHeight, tx, tz, 0, 1);
    vals[1][2] = Sec::at(tileHeight, tx, tz, 1, 0);
    vals[1][0] = Sec::at(tileHeight, tx, tz, -1, 0);

    float gradX = Cohomology::gradient_x(vals);
    float gradZ = Cohomology::gradient_y(vals);
    float slopeSq = __fmaf_rn(gradX, gradX, gradZ * gradZ);
    float slope = (slopeSq > 1e-6f) ? sqrtf(slopeSq) : 0.0f;

    float invSlope = (slopeSq > 1e-6f) ? rsqrtf(slopeSq) : 0.0f;
    waterFlow[idx] = make_float2(-gradX * invSlope, -gradZ * invSlope);

    float capacity = slope * 4.0f;
    float currentSediment = sediment[idx];

    float delta = (capacity - currentSediment) * erosionRate;
    delta = (delta > 0.0f) ? fminf(delta, slope * 0.1f) : delta * (depositionRate / erosionRate);

    heightmap[idx] = h - delta * deltaTime;
    sediment[idx] = currentSediment + delta * deltaTime;
}

__global__ void __launch_bounds__(256, 2) transport_kernel(
    const float* __restrict__ sediment,
    const float2* __restrict__ waterFlow,
    float* __restrict__ sedimentBuffer,
    int resolution,
    float deltaTime
) {
    int x = blockIdx.x * blockDim.x + threadIdx.x;
    int z = blockIdx.y * blockDim.y + threadIdx.y;

    if (x <= 0 || x >= resolution - 1 || z <= 0 || z >= resolution - 1) return;

    int idx = z * resolution + x;
    float2 flow = waterFlow[idx];

    float speedSq = __fmaf_rn(flow.x, flow.x, flow.y * flow.y);
    if (speedSq > 1e-4f) {
        float speed = sqrtf(speedSq);
        float scale = speed * 2.0f;
        int nextX = x + __float2int_rn(flow.x * scale);
        int nextZ = z + __float2int_rn(flow.y * scale);

        int cx = max(0, min(resolution - 1, nextX));
        int cz = max(0, min(resolution - 1, nextZ));
        int nextIdx = cz * resolution + cx;

        float transfer = sediment[idx] * __fmaf_rn(0.5f, speed, 0.0f) * deltaTime;
        sedimentBuffer[idx] -= transfer;
        Atomics::add_float(&sedimentBuffer[nextIdx], transfer);
    }
}

__global__ void __launch_bounds__(256, 2) erosion_transport_fused(
    float* __restrict__ heightmap,
    float* __restrict__ sediment,
    float* __restrict__ sedimentBuffer,
    float2* __restrict__ waterFlow,
    int resolution,
    float deltaTime,
    float erosionRate,
    float depositionRate
) {
    using Sec = Section2D<16, 1, 1>;
    __shared__ float tileHeight[Sec::PADDED][Sec::PADDED];

    int tx = threadIdx.x;
    int tz = threadIdx.y;
    int x = blockIdx.x * 16 + tx;
    int z = blockIdx.y * 16 + tz;

    Sec::restrict_from_global(tileHeight, heightmap, resolution);
    __syncthreads();

    if (x <= 0 || x >= resolution - 1 || z <= 0 || z >= resolution - 1) return;

    int idx = z * resolution + x;

    float vals[3][3];
    float h = Sec::at(tileHeight, tx, tz);
    vals[1][1] = h;
    vals[0][1] = Sec::at(tileHeight, tx, tz, 0, -1);
    vals[2][1] = Sec::at(tileHeight, tx, tz, 0, 1);
    vals[1][2] = Sec::at(tileHeight, tx, tz, 1, 0);
    vals[1][0] = Sec::at(tileHeight, tx, tz, -1, 0);

    float gradX = Cohomology::gradient_x(vals);
    float gradZ = Cohomology::gradient_y(vals);
    float slopeSq = __fmaf_rn(gradX, gradX, gradZ * gradZ);
    float slope = (slopeSq > 1e-6f) ? sqrtf(slopeSq) : 0.0f;

    float invSlope = (slopeSq > 1e-6f) ? rsqrtf(slopeSq) : 0.0f;
    float2 flow = make_float2(-gradX * invSlope, -gradZ * invSlope);
    waterFlow[idx] = flow;

    float capacity = slope * 4.0f;
    float currentSediment = sediment[idx];

    float delta = (capacity - currentSediment) * erosionRate;
    delta = (delta > 0.0f) ? fminf(delta, slope * 0.1f) : delta * (depositionRate / erosionRate);

    heightmap[idx] = h - delta * deltaTime;
    float newSediment = currentSediment + delta * deltaTime;
    sediment[idx] = newSediment;

    __syncthreads();

    float speedSq = __fmaf_rn(flow.x, flow.x, flow.y * flow.y);
    if (speedSq > 1e-4f) {
        float speed = sqrtf(speedSq);
        float scale = speed * 2.0f;
        int nextX = x + __float2int_rn(flow.x * scale);
        int nextZ = z + __float2int_rn(flow.y * scale);

        int cx = max(0, min(resolution - 1, nextX));
        int cz = max(0, min(resolution - 1, nextZ));
        int nextIdx = cz * resolution + cx;

        float transfer = newSediment * __fmaf_rn(0.5f, speed, 0.0f) * deltaTime;
        sedimentBuffer[idx] = newSediment - transfer;
        Atomics::add_float(&sedimentBuffer[nextIdx], transfer);
    } else {
        sedimentBuffer[idx] = newSediment;
    }
}

extern "C" {
    __declspec(dllexport)
    void hydraulic_erosion(
        float* heightmap,
        float* sediment,
        float* waterFlow,
        int resolution,
        float deltaTime,
        float erosionRate,
        float depositionRate,
        int iterations
    ) {
        float *d_heightmap, *d_sediment, *d_sedimentBuffer;
        float2 *d_waterFlow;

        size_t mapSize = resolution * resolution * sizeof(float);
        size_t flowSize = resolution * resolution * sizeof(float2);

        cudaMalloc(&d_heightmap, mapSize);
        cudaMalloc(&d_sediment, mapSize);
        cudaMalloc(&d_waterFlow, flowSize);
        cudaMalloc(&d_sedimentBuffer, mapSize);

        Stream stream;

        cudaMemcpyAsync(d_heightmap, heightmap, mapSize, cudaMemcpyHostToDevice, stream);
        cudaMemcpyAsync(d_sediment, sediment, mapSize, cudaMemcpyHostToDevice, stream);

        dim3 blockDim(16, 16);
        dim3 gridDim((resolution + 15) / 16, (resolution + 15) / 16);

        struct DoubleBuffer {
            float *front, *back;
            void swap() { float *temp = front; front = back; back = temp; }
        } sedimentBuffer = { d_sediment, d_sedimentBuffer };

        for (int iter = 0; iter < iterations; iter++) {
            erosion_transport_fused<<<gridDim, blockDim, 0, stream>>>(
                d_heightmap, sedimentBuffer.front, sedimentBuffer.back, d_waterFlow,
                resolution, deltaTime, erosionRate, depositionRate
            );

            sedimentBuffer.swap();
        }

        d_sediment = sedimentBuffer.front;

        cudaMemcpyAsync(heightmap, d_heightmap, mapSize, cudaMemcpyDeviceToHost, stream);
        cudaMemcpyAsync(sediment, d_sediment, mapSize, cudaMemcpyDeviceToHost, stream);

        float* tempFlow = (float*)malloc(flowSize);
        cudaMemcpyAsync(tempFlow, d_waterFlow, flowSize, cudaMemcpyDeviceToHost, stream);

        cudaStreamSynchronize(stream);

        float2* tempFlow2 = reinterpret_cast<float2*>(tempFlow);
        for (int i = 0; i < resolution * resolution; i++) {
            waterFlow[i * 2] = tempFlow2[i].x;
            waterFlow[i * 2 + 1] = tempFlow2[i].y;
        }
        free(tempFlow);

        cudaFree(d_heightmap);
        cudaFree(d_sediment);
        cudaFree(d_waterFlow);
        cudaFree(d_sedimentBuffer);
    }
}
