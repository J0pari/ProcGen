#include <cuda_runtime.h>
#include "sheaf_template.cuh"
#include "mc_tables.cu"

// Vertex interpolation along edge
__device__ float3 vertexInterp(
    float isolevel,
    float3 p1, float3 p2,
    float v1, float v2
) {
    if (fabsf(isolevel - v1) < 1e-5f) return p1;
    if (fabsf(isolevel - v2) < 1e-5f) return p2;
    if (fabsf(v1 - v2) < 1e-5f) return p1;

    float t = (isolevel - v1) / (v2 - v1);
    return Fiber::interpolate_linear(p1, p2, t);
}

__global__ void marching_cubes_kernel(
    cudaTextureObject_t densityTex,
    cudaTextureObject_t materialTex,
    float* vertices,
    float* normals,
    int* indices,
    int* materialIds,
    int resolution,
    float isovalue,
    int* vertexCount,
    int* indexCount
) {
    int tx = threadIdx.x;
    int ty = threadIdx.y;
    int tz = threadIdx.z;

    int x = blockIdx.x * 8 + tx;
    int y = blockIdx.y * 8 + ty;
    int z = blockIdx.z * 8 + tz;

    if (x >= resolution - 1 || y >= resolution - 1 || z >= resolution - 1) return;

    // Read 8 corners from 3D texture with hardware caching
    float v[8];
    v[0] = tex3D<float>(densityTex, x, y, z);
    v[1] = tex3D<float>(densityTex, x+1, y, z);
    v[2] = tex3D<float>(densityTex, x+1, y+1, z);
    v[3] = tex3D<float>(densityTex, x, y+1, z);
    v[4] = tex3D<float>(densityTex, x, y, z+1);
    v[5] = tex3D<float>(densityTex, x+1, y, z+1);
    v[6] = tex3D<float>(densityTex, x+1, y+1, z+1);
    v[7] = tex3D<float>(densityTex, x, y+1, z+1);

    // Early rejection: all corners same side of isosurface using warp vote
    float vmin = v[0];
    float vmax = v[0];
    #pragma unroll
    for (int i = 1; i < 8; i++) {
        vmin = fminf(vmin, v[i]);
        vmax = fmaxf(vmax, v[i]);
    }

    if (vmin >= isovalue || vmax < isovalue) return;

    // Determine cube index
    int cubeindex = 0;
    cubeindex |= (v[0] < isovalue) ? 1 : 0;
    cubeindex |= (v[1] < isovalue) ? 2 : 0;
    cubeindex |= (v[2] < isovalue) ? 4 : 0;
    cubeindex |= (v[3] < isovalue) ? 8 : 0;
    cubeindex |= (v[4] < isovalue) ? 16 : 0;
    cubeindex |= (v[5] < isovalue) ? 32 : 0;
    cubeindex |= (v[6] < isovalue) ? 64 : 0;
    cubeindex |= (v[7] < isovalue) ? 128 : 0;

    if (edgeTable[cubeindex] == 0) return;

    // Cube corner positions
    float3 p[8];
    float voxelSize = 1.0f;
    p[0] = make_float3(x * voxelSize, y * voxelSize, z * voxelSize);
    p[1] = make_float3((x + 1) * voxelSize, y * voxelSize, z * voxelSize);
    p[2] = make_float3((x + 1) * voxelSize, (y + 1) * voxelSize, z * voxelSize);
    p[3] = make_float3(x * voxelSize, (y + 1) * voxelSize, z * voxelSize);
    p[4] = make_float3(x * voxelSize, y * voxelSize, (z + 1) * voxelSize);
    p[5] = make_float3((x + 1) * voxelSize, y * voxelSize, (z + 1) * voxelSize);
    p[6] = make_float3((x + 1) * voxelSize, (y + 1) * voxelSize, (z + 1) * voxelSize);
    p[7] = make_float3(x * voxelSize, (y + 1) * voxelSize, (z + 1) * voxelSize);

    // Find vertices where surface intersects edges
    float3 vertlist[12];
    if (edgeTable[cubeindex] & 1) vertlist[0] = vertexInterp(isovalue, p[0], p[1], v[0], v[1]);
    if (edgeTable[cubeindex] & 2) vertlist[1] = vertexInterp(isovalue, p[1], p[2], v[1], v[2]);
    if (edgeTable[cubeindex] & 4) vertlist[2] = vertexInterp(isovalue, p[2], p[3], v[2], v[3]);
    if (edgeTable[cubeindex] & 8) vertlist[3] = vertexInterp(isovalue, p[3], p[0], v[3], v[0]);
    if (edgeTable[cubeindex] & 16) vertlist[4] = vertexInterp(isovalue, p[4], p[5], v[4], v[5]);
    if (edgeTable[cubeindex] & 32) vertlist[5] = vertexInterp(isovalue, p[5], p[6], v[5], v[6]);
    if (edgeTable[cubeindex] & 64) vertlist[6] = vertexInterp(isovalue, p[6], p[7], v[6], v[7]);
    if (edgeTable[cubeindex] & 128) vertlist[7] = vertexInterp(isovalue, p[7], p[4], v[7], v[4]);
    if (edgeTable[cubeindex] & 256) vertlist[8] = vertexInterp(isovalue, p[0], p[4], v[0], v[4]);
    if (edgeTable[cubeindex] & 512) vertlist[9] = vertexInterp(isovalue, p[1], p[5], v[1], v[5]);
    if (edgeTable[cubeindex] & 1024) vertlist[10] = vertexInterp(isovalue, p[2], p[6], v[2], v[6]);
    if (edgeTable[cubeindex] & 2048) vertlist[11] = vertexInterp(isovalue, p[3], p[7], v[3], v[7]);

    for (int i = 0; triTable[cubeindex][i] != -1; i += 3) {
        int vIdx = atomicAdd(vertexCount, 3);
        int iIdx = atomicAdd(indexCount, 3);

        float3 v0 = vertlist[triTable[cubeindex][i]];
        float3 v1 = vertlist[triTable[cubeindex][i + 1]];
        float3 v2 = vertlist[triTable[cubeindex][i + 2]];

        float3 edge1 = FastMath::sub(v1, v0);
        float3 edge2 = FastMath::sub(v2, v0);

        float3 normal = make_float3(
            edge1.y * edge2.z - edge1.z * edge2.y,
            edge1.z * edge2.x - edge1.x * edge2.z,
            edge1.x * edge2.y - edge1.y * edge2.x
        );

        float invLen = rsqrtf(normal.x * normal.x + normal.y * normal.y + normal.z * normal.z + 1e-8f);
        normal.x *= invLen;
        normal.y *= invLen;
        normal.z *= invLen;

        for (int j = 0; j < 3; j++) {
            float3 vert = vertlist[triTable[cubeindex][i + j]];
            int vOffset = (vIdx + j) * 3;
            vertices[vOffset + 0] = vert.x;
            vertices[vOffset + 1] = vert.y;
            vertices[vOffset + 2] = vert.z;
            normals[vOffset + 0] = normal.x;
            normals[vOffset + 1] = normal.y;
            normals[vOffset + 2] = normal.z;
            indices[iIdx + j] = vIdx + j;
        }

        materialIds[iIdx / 3] = tex3D<unsigned char>(materialTex, x, y, z);
    }
}

extern "C" {
    __declspec(dllexport)
    void marching_cubes(
        float* densities,
        unsigned char* materials,
        float* vertices,
        float* normals,
        int* indices,
        int* materialIds,
        int resolution,
        float isovalue,
        int* outVertexCount,
        int* outIndexCount
    ) {
        cudaArray *d_densityArray, *d_materialArray;
        float *d_vertices, *d_normals;
        int *d_indices, *d_materialIds, *d_vertexCount, *d_indexCount;

        int voxelCount = resolution * resolution * resolution;
        int maxTriangles = voxelCount * 5;

        cudaExtent volumeSize = make_cudaExtent(resolution, resolution, resolution);
        cudaChannelFormatDesc densityDesc = cudaCreateChannelDesc<float>();
        cudaChannelFormatDesc materialDesc = cudaCreateChannelDesc<unsigned char>();

        cudaMalloc3DArray(&d_densityArray, &densityDesc, volumeSize);
        cudaMalloc3DArray(&d_materialArray, &materialDesc, volumeSize);

        cudaResourceDesc densityRes, materialRes;
        memset(&densityRes, 0, sizeof(densityRes));
        memset(&materialRes, 0, sizeof(materialRes));
        densityRes.resType = cudaResourceTypeArray;
        densityRes.res.array.array = d_densityArray;
        materialRes.resType = cudaResourceTypeArray;
        materialRes.res.array.array = d_materialArray;

        cudaTextureDesc texDesc;
        memset(&texDesc, 0, sizeof(texDesc));
        texDesc.addressMode[0] = texDesc.addressMode[1] = texDesc.addressMode[2] = cudaAddressModeClamp;
        texDesc.filterMode = cudaFilterModePoint;
        texDesc.readMode = cudaReadModeElementType;
        texDesc.normalizedCoords = 0;

        cudaTextureObject_t densityTex, materialTex;
        cudaCreateTextureObject(&densityTex, &densityRes, &texDesc, nullptr);
        cudaCreateTextureObject(&materialTex, &materialRes, &texDesc, nullptr);

        cudaMalloc(&d_vertices, maxTriangles * 3 * 3 * sizeof(float));
        cudaMalloc(&d_normals, maxTriangles * 3 * 3 * sizeof(float));
        cudaMalloc(&d_indices, maxTriangles * 3 * sizeof(int));
        cudaMalloc(&d_materialIds, maxTriangles * sizeof(int));
        cudaMalloc(&d_vertexCount, sizeof(int));
        cudaMalloc(&d_indexCount, sizeof(int));

        Stream stream;

        cudaMemcpy3DParms densityCopy = {0};
        densityCopy.srcPtr = make_cudaPitchedPtr(densities, resolution * sizeof(float), resolution, resolution);
        densityCopy.dstArray = d_densityArray;
        densityCopy.extent = volumeSize;
        densityCopy.kind = cudaMemcpyHostToDevice;
        cudaMemcpy3DAsync(&densityCopy, stream);

        cudaMemcpy3DParms materialCopy = {0};
        materialCopy.srcPtr = make_cudaPitchedPtr(materials, resolution * sizeof(unsigned char), resolution, resolution);
        materialCopy.dstArray = d_materialArray;
        materialCopy.extent = volumeSize;
        materialCopy.kind = cudaMemcpyHostToDevice;
        cudaMemcpy3DAsync(&materialCopy, stream);
        cudaMemsetAsync(d_vertexCount, 0, sizeof(int), stream);
        cudaMemsetAsync(d_indexCount, 0, sizeof(int), stream);

        dim3 blockDim(8, 8, 8);
        dim3 gridDim(
            (resolution + 7) / 8,
            (resolution + 7) / 8,
            (resolution + 7) / 8
        );

        marching_cubes_kernel<<<gridDim, blockDim, 0, stream>>>(
            densityTex, materialTex, d_vertices, d_normals,
            d_indices, d_materialIds, resolution, isovalue,
            d_vertexCount, d_indexCount
        );

        cudaMemcpyAsync(outVertexCount, d_vertexCount, sizeof(int), cudaMemcpyDeviceToHost, stream);
        cudaMemcpyAsync(outIndexCount, d_indexCount, sizeof(int), cudaMemcpyDeviceToHost, stream);

        cudaStreamSynchronize(stream);

        int actualVertBytes = (*outVertexCount) * 3 * sizeof(float);
        int actualNormBytes = (*outVertexCount) * 3 * sizeof(float);
        int actualIdxBytes = (*outIndexCount) * sizeof(int);
        int actualMatBytes = ((*outIndexCount) / 3) * sizeof(int);

        cudaMemcpyAsync(vertices, d_vertices, actualVertBytes, cudaMemcpyDeviceToHost, stream);
        cudaMemcpyAsync(normals, d_normals, actualNormBytes, cudaMemcpyDeviceToHost, stream);
        cudaMemcpyAsync(indices, d_indices, actualIdxBytes, cudaMemcpyDeviceToHost, stream);
        cudaMemcpyAsync(materialIds, d_materialIds, actualMatBytes, cudaMemcpyDeviceToHost, stream);

        cudaStreamSynchronize(stream);

        cudaDestroyTextureObject(densityTex);
        cudaDestroyTextureObject(materialTex);
        cudaFreeArray(d_densityArray);
        cudaFreeArray(d_materialArray);
        cudaFree(d_vertices);
        cudaFree(d_normals);
        cudaFree(d_indices);
        cudaFree(d_materialIds);
        cudaFree(d_vertexCount);
        cudaFree(d_indexCount);
    }
}
