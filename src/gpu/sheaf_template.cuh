#ifndef SHEAF_TEMPLATE_CUH
#define SHEAF_TEMPLATE_CUH

#include <cuda_runtime.h>
#include <cooperative_groups.h>
#if __CUDA_ARCH__ >= 800
#include <cuda_pipeline.h>
#endif

namespace cg = cooperative_groups;

template<typename T, int ALIGN = 16>
struct VectorizedLoad {
    __device__ static void load_float4(
        T* __restrict__ dest,
        const T* __restrict__ src,
        int count
    ) {
        int vec_count = count / 4;
        float4* dest4 = reinterpret_cast<float4*>(dest);
        const float4* src4 = reinterpret_cast<const float4*>(src);

        for (int i = threadIdx.x; i < vec_count; i += blockDim.x) {
            dest4[i] = src4[i];
        }

        int remainder_start = vec_count * 4;
        for (int i = remainder_start + threadIdx.x; i < count; i += blockDim.x) {
            dest[i] = src[i];
        }
    }

    __device__ static void load_float2(
        T* __restrict__ dest,
        const T* __restrict__ src,
        int count
    ) {
        int vec_count = count / 2;
        float2* dest2 = reinterpret_cast<float2*>(dest);
        const float2* src2 = reinterpret_cast<const float2*>(src);

        for (int i = threadIdx.x; i < vec_count; i += blockDim.x) {
            dest2[i] = src2[i];
        }

        int remainder_start = vec_count * 2;
        for (int i = remainder_start + threadIdx.x; i < count; i += blockDim.x) {
            dest[i] = src[i];
        }
    }
};

template<int TILE_DIM, int HALO, int BANK_OFFSET = 0>
struct Section2D {
    static constexpr int PADDED = TILE_DIM + 2 * HALO + BANK_OFFSET;

    template<typename T>
    __device__ static void restrict_from_global(
        T (&section)[PADDED][PADDED],
        const float* __restrict__ global,
        int resolution
    ) {
        int tx = threadIdx.x;
        int ty = threadIdx.y;
        int x = blockIdx.x * TILE_DIM + tx;
        int y = blockIdx.y * TILE_DIM + ty;

        #if __CUDA_ARCH__ >= 800
        if (x < resolution && y < resolution) {
            AsyncCopy<TILE_DIM>::memcpy_async_tile(&section[ty + HALO][tx + HALO], &global[y * resolution + x], 1);
        }
        AsyncCopy<TILE_DIM>::commit_group();
        #else
        if (x < resolution && y < resolution) {
            section[ty + HALO][tx + HALO] = global[y * resolution + x];
        }
        #endif

        if (tx < HALO && x >= HALO) {
            section[ty + HALO][tx] = global[y * resolution + (x - HALO)];
        }
        if (tx >= TILE_DIM - HALO && x < resolution - HALO) {
            section[ty + HALO][tx + 2 * HALO] = global[y * resolution + (x + HALO)];
        }
        if (ty < HALO && y >= HALO) {
            section[ty][tx + HALO] = global[(y - HALO) * resolution + x];
        }
        if (ty >= TILE_DIM - HALO && y < resolution - HALO) {
            section[ty + 2 * HALO][tx + HALO] = global[(y + HALO) * resolution + x];
        }

        #if __CUDA_ARCH__ >= 800
        AsyncCopy<TILE_DIM>::wait_group();
        #endif
    }

    template<typename T>
    __device__ static void extend_to_global(
        float* __restrict__ global,
        const T (&section)[PADDED][PADDED],
        int resolution
    ) {
        int tx = threadIdx.x;
        int ty = threadIdx.y;
        int x = blockIdx.x * TILE_DIM + tx;
        int y = blockIdx.y * TILE_DIM + ty;

        if (x < resolution && y < resolution) {
            global[y * resolution + x] = section[ty + HALO][tx + HALO];
        }
    }

    template<typename T>
    __device__ static T& at(T (&section)[PADDED][PADDED], int tx, int ty, int dx = 0, int dy = 0) {
        return section[ty + HALO + dy][tx + HALO + dx];
    }
};

template<int TILE_DIM, int HALO, int BANK_OFFSET = 0>
struct Section3D {
    static constexpr int PADDED = TILE_DIM + 2 * HALO + BANK_OFFSET;

    template<typename T>
    __device__ static void restrict_from_global(
        T (&section)[PADDED][PADDED][PADDED],
        const float* __restrict__ global,
        int resolution
    ) {
        int tx = threadIdx.x;
        int ty = threadIdx.y;
        int tz = threadIdx.z;
        int x = blockIdx.x * TILE_DIM + tx;
        int y = blockIdx.y * TILE_DIM + ty;
        int z = blockIdx.z * TILE_DIM + tz;

        if (x < resolution && y < resolution && z < resolution) {
            int idx = z * resolution * resolution + y * resolution + x;
            section[tz][ty][tx] = global[idx];
        }

        if (tx == TILE_DIM - 1 && x + 1 < resolution) {
            int idx = z * resolution * resolution + y * resolution + (x + 1);
            section[tz][ty][tx + 1] = global[idx];
        }
        if (ty == TILE_DIM - 1 && y + 1 < resolution) {
            int idx = z * resolution * resolution + (y + 1) * resolution + x;
            section[tz][ty + 1][tx] = global[idx];
        }
        if (tz == TILE_DIM - 1 && z + 1 < resolution) {
            int idx = (z + 1) * resolution * resolution + y * resolution + x;
            section[tz + 1][ty][tx] = global[idx];
        }
    }

    template<typename T, typename U>
    __device__ static void restrict_multiple(
        T (&section1)[PADDED][PADDED][PADDED],
        U (&section2)[PADDED][PADDED][PADDED],
        const float* __restrict__ global1,
        const unsigned char* __restrict__ global2,
        int resolution
    ) {
        int tx = threadIdx.x;
        int ty = threadIdx.y;
        int tz = threadIdx.z;
        int x = blockIdx.x * TILE_DIM + tx;
        int y = blockIdx.y * TILE_DIM + ty;
        int z = blockIdx.z * TILE_DIM + tz;
        int res = resolution;

        if (x < res && y < res && z < res) {
            int idx = z * res * res + y * res + x;
            section1[tz][ty][tx] = global1[idx];
            section2[tz][ty][tx] = global2[idx];
        }

        if (tx == TILE_DIM - 1 && x + 1 < res) {
            int idx = z * res * res + y * res + (x + 1);
            section1[tz][ty][tx + 1] = global1[idx];
        }
        if (ty == TILE_DIM - 1 && y + 1 < res) {
            int idx = z * res * res + (y + 1) * res + x;
            section1[tz][ty + 1][tx] = global1[idx];
        }
        if (tz == TILE_DIM - 1 && z + 1 < res) {
            int idx = (z + 1) * res * res + y * res + x;
            section1[tz + 1][ty][tx] = global1[idx];
        }
    }
};

template<int BLOCK_SIZE>
struct Section1D {
    template<typename T>
    __device__ static float3 as_position(const float* __restrict__ data, int idx, int count) {
        if (idx < count) {
            return make_float3(data[idx * 3], data[idx * 3 + 1], data[idx * 3 + 2]);
        }
        return make_float3(0.0f, 0.0f, 0.0f);
    }

    __device__ static float3 as_position_offset(const float* __restrict__ data, int offset, int idx) {
        return make_float3(data[offset + idx * 3], data[offset + idx * 3 + 1], data[offset + idx * 3 + 2]);
    }

    template<typename T>
    __device__ static void restrict_positions(
        T (&section)[BLOCK_SIZE],
        const float* __restrict__ global,
        int count
    ) {
        int tid = threadIdx.x;
        int idx = blockIdx.x * BLOCK_SIZE + tid;

        if (idx < count && tid < BLOCK_SIZE) {
            section[tid] = as_position<T>(global, idx, count);
        }
    }

    template<typename T>
    __device__ static void restrict_flat(
        T* section,
        const float* __restrict__ global,
        int count,
        int stride
    ) {
        int total = count * stride;
        if (total >= BLOCK_SIZE * 4) {
            VectorizedLoad<float>::load_float4(section, global, total);
        } else {
            int tid = threadIdx.x;
            int idx = blockIdx.x * BLOCK_SIZE + tid;

            if (idx < count) {
                for (int i = 0; i < stride; i++) {
                    section[tid * stride + i] = global[idx * stride + i];
                }
            }
        }
    }

    __device__ static void extend_position(
        float* __restrict__ global,
        int idx,
        int count,
        float3 pos
    ) {
        if (idx < count) {
            global[idx * 3] = pos.x;
            global[idx * 3 + 1] = pos.y;
            global[idx * 3 + 2] = pos.z;
        }
    }
};

struct Gluing {
    template<typename SectionA, typename SectionB>
    __device__ static bool compatible_on_overlap(
        const SectionA& s1,
        const SectionB& s2,
        int overlap_size,
        float tolerance = 1e-6f
    ) {
        for (int i = threadIdx.x; i < overlap_size; i += blockDim.x) {
            if (fabsf(s1[i] - s2[i]) > tolerance) {
                return false;
            }
        }
        return true;
    }
};

template<int TILE_SIZE = 32>
struct WarpReduce {
    __device__ static float sum(float val) {
        auto tile = cg::tiled_partition<TILE_SIZE>(cg::this_thread_block());
        #pragma unroll
        for (int offset = TILE_SIZE / 2; offset > 0; offset >>= 1) {
            val += tile.shfl_down(val, offset);
        }
        return val;
    }

    __device__ static float max(float val) {
        auto tile = cg::tiled_partition<TILE_SIZE>(cg::this_thread_block());
        #pragma unroll
        for (int offset = TILE_SIZE / 2; offset > 0; offset >>= 1) {
            val = fmaxf(val, tile.shfl_down(val, offset));
        }
        return val;
    }

    __device__ static float min(float val) {
        auto tile = cg::tiled_partition<TILE_SIZE>(cg::this_thread_block());
        #pragma unroll
        for (int offset = TILE_SIZE / 2; offset > 0; offset >>= 1) {
            val = fminf(val, tile.shfl_down(val, offset));
        }
        return val;
    }

    __device__ static unsigned ballot(int predicate) {
        auto tile = cg::tiled_partition<TILE_SIZE>(cg::this_thread_block());
        return tile.ballot(predicate);
    }

    __device__ static int any(int predicate) {
        auto tile = cg::tiled_partition<TILE_SIZE>(cg::this_thread_block());
        return tile.any(predicate);
    }

    __device__ static int all(int predicate) {
        auto tile = cg::tiled_partition<TILE_SIZE>(cg::this_thread_block());
        return tile.all(predicate);
    }

    __device__ static int thread_rank() {
        auto tile = cg::tiled_partition<TILE_SIZE>(cg::this_thread_block());
        return tile.thread_rank();
    }
};

template<int BLOCK_SIZE, int WARP_SIZE = 32>
struct BlockReduce {
    __device__ static float sum(float val) {
        __shared__ float shared[BLOCK_SIZE / WARP_SIZE];
        int lane = threadIdx.x % WARP_SIZE;
        int wid = threadIdx.x / WARP_SIZE;

        val = WarpReduce<WARP_SIZE>::sum(val);

        if (lane == 0) shared[wid] = val;
        __syncthreads();

        val = (threadIdx.x < BLOCK_SIZE / WARP_SIZE) ? shared[lane] : 0.0f;
        if (wid == 0) val = WarpReduce<WARP_SIZE>::sum(val);

        return val;
    }
};

struct GridStride {
    __device__ static int thread_id() {
        return blockIdx.x * blockDim.x + threadIdx.x;
    }

    __device__ static int stride() {
        return blockDim.x * gridDim.x;
    }
};

struct Atomics {
    __device__ static int claim_slot(int* counter) {
        return atomicAdd(counter, 1);
    }

    __device__ static void add_float(float* address, float val) {
        atomicAdd(address, val);
    }

    __device__ static float cas_float(float* address, float compare, float val) {
        return atomicCAS((int*)address, __float_as_int(compare), __float_as_int(val));
    }
};

template<int TILE_SIZE>
struct AsyncCopy {
    template<typename T>
    __device__ static void memcpy_async_tile(
        T* __restrict__ dest,
        const T* __restrict__ src,
        int count
    ) {
        #if __CUDA_ARCH__ >= 800
        __pipeline_memcpy_async(dest, src, count * sizeof(T));
        #else
        for (int i = threadIdx.x; i < count; i += blockDim.x) {
            dest[i] = src[i];
        }
        #endif
    }

    __device__ static void commit_group() {
        #if __CUDA_ARCH__ >= 800
        __pipeline_commit();
        #else
        __syncthreads();
        #endif
    }

    __device__ static void wait_group() {
        #if __CUDA_ARCH__ >= 800
        __pipeline_wait_prior(0);
        #else
        __syncthreads();
        #endif
    }
};

struct Stream {
    cudaStream_t s;
    __host__ Stream() { cudaStreamCreate(&s); }
    __host__ ~Stream() { cudaStreamDestroy(s); }
    __host__ operator cudaStream_t() const { return s; }
};

struct Fiber {
    __device__ static float interpolate_linear(float a, float b, float t) {
        return __fmaf_rn(b - a, t, a);
    }

    __device__ static float3 interpolate_linear(float3 a, float3 b, float t) {
        return make_float3(
            __fmaf_rn(b.x - a.x, t, a.x),
            __fmaf_rn(b.y - a.y, t, a.y),
            __fmaf_rn(b.z - a.z, t, a.z)
        );
    }

    __device__ static float interpolate_bilinear(float v00, float v10, float v01, float v11, float tx, float ty) {
        float v0 = interpolate_linear(v00, v10, tx);
        float v1 = interpolate_linear(v01, v11, tx);
        return interpolate_linear(v0, v1, ty);
    }

    __device__ static float interpolate_trilinear(
        float v000, float v100, float v010, float v110,
        float v001, float v101, float v011, float v111,
        float tx, float ty, float tz
    ) {
        float v00 = interpolate_linear(v000, v100, tx);
        float v10 = interpolate_linear(v010, v110, tx);
        float v01 = interpolate_linear(v001, v101, tx);
        float v11 = interpolate_linear(v011, v111, tx);
        float v0 = interpolate_linear(v00, v10, ty);
        float v1 = interpolate_linear(v01, v11, ty);
        return interpolate_linear(v0, v1, tz);
    }
};

struct Cohomology {
    template<int N>
    __device__ static float laplacian_stencil_2d(const float (&vals)[N][N]) {
        static_assert(N >= 3 && N % 2 == 1, "N must be odd and >= 3");
        constexpr int c = N / 2;
        return vals[c-1][c] + vals[c+1][c] + vals[c][c-1] + vals[c][c+1] - 4.0f * vals[c][c];
    }

    template<int N>
    __device__ static float gradient_x(const float (&vals)[N][N]) {
        static_assert(N >= 3 && N % 2 == 1, "N must be odd and >= 3");
        constexpr int c = N / 2;
        return (vals[c][c+1] - vals[c][c-1]) * 0.5f;
    }

    template<int N>
    __device__ static float gradient_y(const float (&vals)[N][N]) {
        static_assert(N >= 3 && N % 2 == 1, "N must be odd and >= 3");
        constexpr int c = N / 2;
        return (vals[c+1][c] - vals[c-1][c]) * 0.5f;
    }

    __device__ static float divergence_2d(float2 field, float2 fieldE, float2 fieldN) {
        return (fieldE.x - field.x) + (fieldN.y - field.y);
    }
};

struct FastMath {
    __device__ __forceinline__ static float distance_sq(float3 a, float3 b) {
        float dx = a.x - b.x;
        float dy = a.y - b.y;
        float dz = a.z - b.z;
        float result;
        asm volatile (
            "mul.f32 %0, %1, %1;\n\t"
            "fma.rn.f32 %0, %2, %2, %0;\n\t"
            "fma.rn.f32 %0, %3, %3, %0;\n\t"
            : "=f"(result)
            : "f"(dx), "f"(dy), "f"(dz)
        );
        return result;
    }

    __device__ __forceinline__ static float3 sub(float3 a, float3 b) {
        return make_float3(a.x - b.x, a.y - b.y, a.z - b.z);
    }

    __device__ __forceinline__ static float dot(float3 a, float3 b) {
        float result;
        asm volatile (
            "mul.f32 %0, %1, %4;\n\t"
            "fma.rn.f32 %0, %2, %5, %0;\n\t"
            "fma.rn.f32 %0, %3, %6, %0;\n\t"
            : "=f"(result)
            : "f"(a.x), "f"(a.y), "f"(a.z), "f"(b.x), "f"(b.y), "f"(b.z)
        );
        return result;
    }
};

struct Occupancy {
    template<typename KernelFunc>
    __host__ static void optimal_launch_config(
        KernelFunc kernel,
        int& blockSize,
        int& minGridSize,
        size_t dynamicSMemSize = 0
    ) {
        cudaOccupancyMaxPotentialBlockSize(
            &minGridSize,
            &blockSize,
            kernel,
            dynamicSMemSize,
            0
        );
    }
};

template<typename T>
struct PinnedMemory {
    T* ptr;
    size_t size;

    __host__ PinnedMemory(size_t count) : size(count * sizeof(T)) {
        cudaHostAlloc(&ptr, size, cudaHostAllocDefault);
    }

    __host__ ~PinnedMemory() {
        if (ptr) cudaFreeHost(ptr);
    }

    __host__ operator T*() { return ptr; }
    __host__ T& operator[](size_t i) { return ptr[i]; }
};

__device__ __forceinline__ float ldg_float(const float* ptr) {
    #if __CUDA_ARCH__ >= 350
    return __ldg(ptr);
    #else
    return *ptr;
    #endif
}

__device__ __forceinline__ float4 ldg_float4(const float4* ptr) {
    #if __CUDA_ARCH__ >= 350
    return __ldg(ptr);
    #else
    return *ptr;
    #endif
}

#endif
