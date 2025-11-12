#include <cuda_runtime.h>
#include "sheaf_template.cuh"

struct Spring {
    int nodeA;
    int nodeB;
    float restLength;
    float stiffness;
    float damping;
};

__global__ void __launch_bounds__(256, 4) compute_spring_forces(
    const float3* __restrict__ positions,
    const float3* __restrict__ velocities,
    const Spring* __restrict__ springs,
    float3* forces,
    int nodeCount,
    int springCount,
    float minDistanceEpsilon
) {
    int springIdx = GridStride::thread_id();

    for (int i = springIdx; i < springCount; i += GridStride::stride()) {
        Spring spring = springs[i];

        float3 posA = positions[spring.nodeA];
        float3 posB = positions[spring.nodeB];

        float3 delta = FastMath::sub(posB, posA);
        float distSq = FastMath::dot(delta, delta);

        if (distSq > minDistanceEpsilon * minDistanceEpsilon) {
            float dist = sqrtf(distSq);
            float invDist = __fdividef(1.0f, dist);

            float3 direction = make_float3(
                delta.x * invDist,
                delta.y * invDist,
                delta.z * invDist
            );

            float displacement = dist - spring.restLength;
            float springMag = displacement * spring.stiffness;

            float3 velA = velocities[spring.nodeA];
            float3 velB = velocities[spring.nodeB];
            float3 relVel = FastMath::sub(velB, velA);
            float dampingMag = FastMath::dot(relVel, direction) * spring.damping;

            float totalMag = springMag + dampingMag;

            float3 force = make_float3(
                direction.x * totalMag,
                direction.y * totalMag,
                direction.z * totalMag
            );

            Atomics::add_float(&forces[spring.nodeA].x, force.x);
            Atomics::add_float(&forces[spring.nodeA].y, force.y);
            Atomics::add_float(&forces[spring.nodeA].z, force.z);

            Atomics::add_float(&forces[spring.nodeB].x, -force.x);
            Atomics::add_float(&forces[spring.nodeB].y, -force.y);
            Atomics::add_float(&forces[spring.nodeB].z, -force.z);
        }
    }
}

__global__ void __launch_bounds__(256, 2) add_external_forces(
    float3* forces,
    const float3* __restrict__ externalForces,
    int nodeCount
) {
    int idx = GridStride::thread_id();

    for (int i = idx; i < nodeCount; i += GridStride::stride()) {
        float3 ext = externalForces[i];
        Atomics::add_float(&forces[i].x, ext.x);
        Atomics::add_float(&forces[i].y, ext.y);
        Atomics::add_float(&forces[i].z, ext.z);
    }
}

extern "C" {
    __declspec(dllexport)
    void compute_forces(
        float* positions,
        float* velocities,
        float* forces,
        int* springNodeA,
        int* springNodeB,
        float* springRestLength,
        float* springStiffness,
        float* springDamping,
        int nodeCount,
        int springCount,
        float minDistanceEpsilon
    ) {
        static float3 *d_positions = nullptr, *d_velocities = nullptr, *d_forces = nullptr;
        static Spring *d_springs = nullptr;
        static int cachedNodeCount = 0, cachedSpringCount = 0;

        size_t nodeSize = nodeCount * sizeof(float3);
        size_t springSize = springCount * sizeof(Spring);

        if (cachedNodeCount != nodeCount) {
            if (d_positions) {
                cudaFree(d_positions);
                cudaFree(d_velocities);
                cudaFree(d_forces);
            }
            cudaMalloc(&d_positions, nodeSize);
            cudaMalloc(&d_velocities, nodeSize);
            cudaMalloc(&d_forces, nodeSize);
            cachedNodeCount = nodeCount;
        }

        if (cachedSpringCount != springCount) {
            if (d_springs) cudaFree(d_springs);
            cudaMalloc(&d_springs, springSize);
            cachedSpringCount = springCount;
        }

        PinnedMemory<float3> h_positions(nodeCount);
        PinnedMemory<float3> h_velocities(nodeCount);
        PinnedMemory<Spring> h_springs(springCount);

        for (int i = 0; i < nodeCount; i++) {
            h_positions[i] = make_float3(positions[i*3], positions[i*3+1], positions[i*3+2]);
            h_velocities[i] = make_float3(velocities[i*3], velocities[i*3+1], velocities[i*3+2]);
        }

        for (int i = 0; i < springCount; i++) {
            h_springs[i].nodeA = springNodeA[i];
            h_springs[i].nodeB = springNodeB[i];
            h_springs[i].restLength = springRestLength[i];
            h_springs[i].stiffness = springStiffness[i];
            h_springs[i].damping = springDamping[i];
        }

        Stream stream;

        cudaMemcpyAsync(d_positions, h_positions.ptr, nodeSize, cudaMemcpyHostToDevice, stream);
        cudaMemcpyAsync(d_velocities, h_velocities.ptr, nodeSize, cudaMemcpyHostToDevice, stream);
        cudaMemcpyAsync(d_springs, h_springs.ptr, springSize, cudaMemcpyHostToDevice, stream);
        cudaMemsetAsync(d_forces, 0, nodeSize, stream);

        int smCount;
        cudaDeviceGetAttribute(&smCount, cudaDevAttrMultiProcessorCount, 0);
        int blockSize = 256;
        int gridSize = smCount * 4;

        compute_spring_forces<<<gridSize, blockSize, 0, stream>>>(
            d_positions, d_velocities, d_springs,
            d_forces, nodeCount, springCount,
            minDistanceEpsilon
        );

        PinnedMemory<float3> h_forces(nodeCount);
        cudaMemcpyAsync(h_forces.ptr, d_forces, nodeSize, cudaMemcpyDeviceToHost, stream);
        cudaStreamSynchronize(stream);

        for (int i = 0; i < nodeCount; i++) {
            forces[i*3] = h_forces[i].x;
            forces[i*3+1] = h_forces[i].y;
            forces[i*3+2] = h_forces[i].z;
        }
    }
}
