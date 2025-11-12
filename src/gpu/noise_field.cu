#include <cuda_runtime.h>
#include "sheaf_template.cuh"

__device__ __forceinline__ uint64_t hash3d(int x, int y, int z, uint64_t seed) {
    uint64_t h = seed;
    h ^= (uint64_t)x * 0xff51afd7ed558ccdULL;
    h ^= h >> 33;
    h ^= (uint64_t)y * 0xc4ceb9fe1a85ec53ULL;
    h ^= h >> 33;
    h ^= (uint64_t)z * 0xff51afd7ed558ccdULL;
    h ^= h >> 33;
    return h;
}

__device__ __forceinline__ float smoothstep(float t) {
    return t * t * (3.0f - 2.0f * t);
}

__device__ __forceinline__ float3 gradient(uint64_t hash) {
    int h = (int)(hash & 15ULL);
    float u = (h < 8) ? 1.0f : -1.0f;
    float v = ((h & 1) == 0) ? 1.0f : -1.0f;
    float w = ((h & 2) == 0) ? 1.0f : -1.0f;
    return make_float3(u, v, w);
}

__device__ float perlin3d(float3 pos, uint64_t seed) {
    int x0 = __float2int_rd(pos.x);
    int y0 = __float2int_rd(pos.y);
    int z0 = __float2int_rd(pos.z);

    int x1 = x0 + 1;
    int y1 = y0 + 1;
    int z1 = z0 + 1;

    float fx = pos.x - floorf(pos.x);
    float fy = pos.y - floorf(pos.y);
    float fz = pos.z - floorf(pos.z);

    float u = smoothstep(fx);
    float v = smoothstep(fy);
    float w = smoothstep(fz);

    float3 g000 = gradient(hash3d(x0, y0, z0, seed));
    float3 g100 = gradient(hash3d(x1, y0, z0, seed));
    float3 g010 = gradient(hash3d(x0, y1, z0, seed));
    float3 g110 = gradient(hash3d(x1, y1, z0, seed));
    float3 g001 = gradient(hash3d(x0, y0, z1, seed));
    float3 g101 = gradient(hash3d(x1, y0, z1, seed));
    float3 g011 = gradient(hash3d(x0, y1, z1, seed));
    float3 g111 = gradient(hash3d(x1, y1, z1, seed));

    float3 d000 = make_float3(fx, fy, fz);
    float3 d100 = make_float3(fx - 1.0f, fy, fz);
    float3 d010 = make_float3(fx, fy - 1.0f, fz);
    float3 d110 = make_float3(fx - 1.0f, fy - 1.0f, fz);
    float3 d001 = make_float3(fx, fy, fz - 1.0f);
    float3 d101 = make_float3(fx - 1.0f, fy, fz - 1.0f);
    float3 d011 = make_float3(fx, fy - 1.0f, fz - 1.0f);
    float3 d111 = make_float3(fx - 1.0f, fy - 1.0f, fz - 1.0f);

    float dot000 = FastMath::dot(g000, d000);
    float dot100 = FastMath::dot(g100, d100);
    float dot010 = FastMath::dot(g010, d010);
    float dot110 = FastMath::dot(g110, d110);
    float dot001 = FastMath::dot(g001, d001);
    float dot101 = FastMath::dot(g101, d101);
    float dot011 = FastMath::dot(g011, d011);
    float dot111 = FastMath::dot(g111, d111);

    float x00 = Fiber::interpolate_linear(dot000, dot100, u);
    float x10 = Fiber::interpolate_linear(dot010, dot110, u);
    float x01 = Fiber::interpolate_linear(dot001, dot101, u);
    float x11 = Fiber::interpolate_linear(dot011, dot111, u);

    float yy0 = Fiber::interpolate_linear(x00, x10, v);
    float yy1 = Fiber::interpolate_linear(x01, x11, v);

    return Fiber::interpolate_linear(yy0, yy1, w);
}

__global__ void __launch_bounds__(512, 2) noise_field_3d(
    float* output,
    int resolutionX,
    int resolutionY,
    int resolutionZ,
    float3 offset,
    float scale,
    uint64_t seed,
    int octaves,
    float lacunarity,
    float persistence
) {
    int x = blockIdx.x * blockDim.x + threadIdx.x;
    int y = blockIdx.y * blockDim.y + threadIdx.y;
    int z = blockIdx.z * blockDim.z + threadIdx.z;

    if (x >= resolutionX || y >= resolutionY || z >= resolutionZ) return;

    float3 pos = make_float3(
        (float)x + offset.x,
        (float)y + offset.y,
        (float)z + offset.z
    );

    float value = 0.0f;
    float amplitude = 1.0f;
    float frequency = scale;
    float maxValue = 0.0f;

    #pragma unroll 4
    for (int oct = 0; oct < octaves; oct++) {
        float3 scaledPos = make_float3(
            pos.x * frequency,
            pos.y * frequency,
            pos.z * frequency
        );

        value = __fmaf_rn(perlin3d(scaledPos, seed), amplitude, value);
        maxValue += amplitude;
        amplitude *= persistence;
        frequency *= lacunarity;
    }

    int idx = z * resolutionY * resolutionX + y * resolutionX + x;
    output[idx] = value / maxValue;
}

__global__ void __launch_bounds__(512, 2) curl_noise_field(
    float3* output,
    int resolutionX,
    int resolutionY,
    int resolutionZ,
    float3 offset,
    float scale,
    uint64_t seed,
    float epsilon
) {
    int x = blockIdx.x * blockDim.x + threadIdx.x;
    int y = blockIdx.y * blockDim.y + threadIdx.y;
    int z = blockIdx.z * blockDim.z + threadIdx.z;

    if (x >= resolutionX || y >= resolutionY || z >= resolutionZ) return;

    float3 pos = make_float3(
        ((float)x + offset.x) * scale,
        ((float)y + offset.y) * scale,
        ((float)z + offset.z) * scale
    );

    float3 ex = make_float3(epsilon, 0.0f, 0.0f);
    float3 ey = make_float3(0.0f, epsilon, 0.0f);
    float3 ez = make_float3(0.0f, 0.0f, epsilon);

    float px_y = perlin3d(make_float3(pos.x, pos.y + epsilon, pos.z), seed);
    float px_z = perlin3d(make_float3(pos.x, pos.y, pos.z + epsilon), seed);
    float py_x = perlin3d(make_float3(pos.x + epsilon, pos.y, pos.z), seed);
    float py_z = perlin3d(make_float3(pos.x, pos.y, pos.z + epsilon), seed + 1ULL);
    float pz_x = perlin3d(make_float3(pos.x + epsilon, pos.y, pos.z), seed + 2ULL);
    float pz_y = perlin3d(make_float3(pos.x, pos.y + epsilon, pos.z), seed + 2ULL);

    float nx_y = perlin3d(make_float3(pos.x, pos.y - epsilon, pos.z), seed);
    float nx_z = perlin3d(make_float3(pos.x, pos.y, pos.z - epsilon), seed);
    float ny_x = perlin3d(make_float3(pos.x - epsilon, pos.y, pos.z), seed);
    float ny_z = perlin3d(make_float3(pos.x, pos.y, pos.z - epsilon), seed + 1ULL);
    float nz_x = perlin3d(make_float3(pos.x - epsilon, pos.y, pos.z), seed + 2ULL);
    float nz_y = perlin3d(make_float3(pos.x, pos.y - epsilon, pos.z), seed + 2ULL);

    float inv2eps = __fdividef(1.0f, 2.0f * epsilon);

    float dx_dy = (px_y - nx_y) * inv2eps;
    float dx_dz = (px_z - nx_z) * inv2eps;
    float dy_dx = (py_x - ny_x) * inv2eps;
    float dy_dz = (py_z - ny_z) * inv2eps;
    float dz_dx = (pz_x - nz_x) * inv2eps;
    float dz_dy = (pz_y - nz_y) * inv2eps;

    float3 curl = make_float3(
        dz_dy - dy_dz,
        dx_dz - dz_dx,
        dy_dx - dx_dy
    );

    float len = sqrtf(FastMath::dot(curl, curl) + 1e-8f);
    float invLen = __fdividef(1.0f, len);

    int idx = z * resolutionY * resolutionX + y * resolutionX + x;
    output[idx] = make_float3(
        curl.x * invLen,
        curl.y * invLen,
        curl.z * invLen
    );
}

extern "C" {
    __declspec(dllexport)
    void generate_noise_field(
        float* output,
        int resolutionX,
        int resolutionY,
        int resolutionZ,
        float offsetX,
        float offsetY,
        float offsetZ,
        float scale,
        unsigned long long seed,
        int octaves,
        float lacunarity,
        float persistence
    ) {
        static float *d_output = nullptr;
        static int cachedX = 0, cachedY = 0, cachedZ = 0;

        size_t size = resolutionX * resolutionY * resolutionZ * sizeof(float);

        if (cachedX != resolutionX || cachedY != resolutionY || cachedZ != resolutionZ) {
            if (d_output) cudaFree(d_output);
            cudaMalloc(&d_output, size);
            cachedX = resolutionX;
            cachedY = resolutionY;
            cachedZ = resolutionZ;
        }

        dim3 blockDim(8, 8, 8);
        dim3 gridDim(
            (resolutionX + 7) / 8,
            (resolutionY + 7) / 8,
            (resolutionZ + 7) / 8
        );

        Stream stream;

        float3 offset = make_float3(offsetX, offsetY, offsetZ);

        noise_field_3d<<<gridDim, blockDim, 0, stream>>>(
            d_output,
            resolutionX, resolutionY, resolutionZ,
            offset, scale, seed,
            octaves, lacunarity, persistence
        );

        PinnedMemory<float> h_output(resolutionX * resolutionY * resolutionZ);
        cudaMemcpyAsync(h_output.ptr, d_output, size, cudaMemcpyDeviceToHost, stream);
        cudaStreamSynchronize(stream);

        memcpy(output, h_output.ptr, size);
    }

    __declspec(dllexport)
    void generate_curl_noise(
        float* output,
        int resolutionX,
        int resolutionY,
        int resolutionZ,
        float offsetX,
        float offsetY,
        float offsetZ,
        float scale,
        unsigned long long seed,
        float epsilon
    ) {
        static float3 *d_output = nullptr;
        static int cachedX = 0, cachedY = 0, cachedZ = 0;

        size_t size = resolutionX * resolutionY * resolutionZ * sizeof(float3);

        if (cachedX != resolutionX || cachedY != resolutionY || cachedZ != resolutionZ) {
            if (d_output) cudaFree(d_output);
            cudaMalloc(&d_output, size);
            cachedX = resolutionX;
            cachedY = resolutionY;
            cachedZ = resolutionZ;
        }

        dim3 blockDim(8, 8, 8);
        dim3 gridDim(
            (resolutionX + 7) / 8,
            (resolutionY + 7) / 8,
            (resolutionZ + 7) / 8
        );

        Stream stream;

        float3 offset = make_float3(offsetX, offsetY, offsetZ);

        curl_noise_field<<<gridDim, blockDim, 0, stream>>>(
            d_output,
            resolutionX, resolutionY, resolutionZ,
            offset, scale, seed, epsilon
        );

        PinnedMemory<float> h_output(resolutionX * resolutionY * resolutionZ * 3);
        cudaMemcpyAsync(h_output.ptr, d_output, size, cudaMemcpyDeviceToHost, stream);
        cudaStreamSynchronize(stream);

        memcpy(output, h_output.ptr, size);
    }
}
