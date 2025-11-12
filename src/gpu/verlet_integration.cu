#include <cuda_runtime.h>
#include "sheaf_template.cuh"

__global__ void __launch_bounds__(512, 2) verlet_kernel(
    float4* __restrict__ positions,
    float4* __restrict__ velocities,
    cudaTextureObject_t forceTex,
    cudaTextureObject_t massTex,
    int nodeCount,
    float timeStep
) {
    float dt = timeStep;
    float halfDt2 = 0.5f * dt * dt;

    for (int idx = GridStride::thread_id(); idx < nodeCount; idx += GridStride::stride()) {
        float4 force = tex1Dfetch<float4>(forceTex, idx);
        float invMass = tex1Dfetch<float>(massTex, idx);

        float4 pos = positions[idx];
        float4 vel = velocities[idx];

        float3 accel = make_float3(force.x * invMass, force.y * invMass, force.z * invMass);

        pos.x = __fmaf_rn(vel.x, dt, __fmaf_rn(accel.x, halfDt2, pos.x));
        pos.y = __fmaf_rn(vel.y, dt, __fmaf_rn(accel.y, halfDt2, pos.y));
        pos.z = __fmaf_rn(vel.z, dt, __fmaf_rn(accel.z, halfDt2, pos.z));

        vel.x = __fmaf_rn(accel.x, dt, vel.x);
        vel.y = __fmaf_rn(accel.y, dt, vel.y);
        vel.z = __fmaf_rn(accel.z, dt, vel.z);

        positions[idx] = pos;
        velocities[idx] = vel;
    }
}

extern "C" {
    __declspec(dllexport)
    void verlet_integrate(
        float* positions,
        float* velocities,
        float* forces,
        int nodeCount,
        float timeStep,
        float* masses
    ) {
        static float4 *d_positions = nullptr, *d_velocities = nullptr;
        static cudaArray *d_forceArray = nullptr, *d_massArray = nullptr;
        static cudaTextureObject_t forceTex = 0, massTex = 0;
        static int cachedNodeCount = 0;

        size_t vec4Size = nodeCount * sizeof(float4);

        if (cachedNodeCount != nodeCount) {
            if (d_positions) {
                cudaFree(d_positions);
                cudaFree(d_velocities);
                cudaDestroyTextureObject(forceTex);
                cudaDestroyTextureObject(massTex);
                cudaFreeArray(d_forceArray);
                cudaFreeArray(d_massArray);
            }

            cudaMalloc(&d_positions, vec4Size);
            cudaMalloc(&d_velocities, vec4Size);

            cudaChannelFormatDesc forceDesc = cudaCreateChannelDesc<float4>();
            cudaChannelFormatDesc massDesc = cudaCreateChannelDesc<float>();
            cudaMallocArray(&d_forceArray, &forceDesc, nodeCount);
            cudaMallocArray(&d_massArray, &massDesc, nodeCount);

            cudaResourceDesc forceRes, massRes;
            memset(&forceRes, 0, sizeof(forceRes));
            memset(&massRes, 0, sizeof(massRes));
            forceRes.resType = cudaResourceTypeArray;
            forceRes.res.array.array = d_forceArray;
            massRes.resType = cudaResourceTypeArray;
            massRes.res.array.array = d_massArray;

            cudaTextureDesc texDesc;
            memset(&texDesc, 0, sizeof(texDesc));
            texDesc.addressMode[0] = cudaAddressModeClamp;
            texDesc.filterMode = cudaFilterModePoint;
            texDesc.readMode = cudaReadModeElementType;
            texDesc.normalizedCoords = 0;

            cudaCreateTextureObject(&forceTex, &forceRes, &texDesc, nullptr);
            cudaCreateTextureObject(&massTex, &massRes, &texDesc, nullptr);

            cachedNodeCount = nodeCount;
        }

        PinnedMemory<float4> h_positions4(nodeCount);
        PinnedMemory<float4> h_velocities4(nodeCount);
        PinnedMemory<float4> h_forces4(nodeCount);
        for (int i = 0; i < nodeCount; i++) {
            h_positions4[i] = make_float4(positions[i*3], positions[i*3+1], positions[i*3+2], 0.0f);
            h_velocities4[i] = make_float4(velocities[i*3], velocities[i*3+1], velocities[i*3+2], 0.0f);
            h_forces4[i] = make_float4(forces[i*3], forces[i*3+1], forces[i*3+2], 0.0f);
        }

        Stream stream;

        cudaMemcpyAsync(d_positions, h_positions4, vec4Size, cudaMemcpyHostToDevice, stream);
        cudaMemcpyAsync(d_velocities, h_velocities4, vec4Size, cudaMemcpyHostToDevice, stream);
        cudaMemcpyToArrayAsync(d_forceArray, 0, 0, h_forces4, vec4Size, cudaMemcpyHostToDevice, stream);
        cudaMemcpyToArrayAsync(d_massArray, 0, 0, masses, nodeCount * sizeof(float), cudaMemcpyHostToDevice, stream);

        int smCount;
        cudaDeviceGetAttribute(&smCount, cudaDevAttrMultiProcessorCount, 0);
        int blockSize = 512;
        int gridSize = smCount * 4;

        verlet_kernel<<<gridSize, blockSize, 0, stream>>>(
            d_positions, d_velocities, forceTex, massTex, nodeCount, timeStep
        );

        cudaMemcpyAsync(h_positions4, d_positions, vec4Size, cudaMemcpyDeviceToHost, stream);
        cudaMemcpyAsync(h_velocities4, d_velocities, vec4Size, cudaMemcpyDeviceToHost, stream);

        cudaStreamSynchronize(stream);

        for (int i = 0; i < nodeCount; i++) {
            positions[i*3] = h_positions4[i].x;
            positions[i*3+1] = h_positions4[i].y;
            positions[i*3+2] = h_positions4[i].z;
            velocities[i*3] = h_velocities4[i].x;
            velocities[i*3+1] = h_velocities4[i].y;
            velocities[i*3+2] = h_velocities4[i].z;
        }

    }
}
