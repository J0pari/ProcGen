namespace GPU.Compute

open System
open System.IO
open System.Runtime.InteropServices
open System.Reflection

/// CUDA kernel signatures (P/Invoke)
module CUDA =

    [<Literal>]
    let private LibraryPath = __SOURCE_DIRECTORY__ + "/libgpu_compute.so"

    let private resolveLibrary (libraryName: string) (assembly: Assembly) (searchPath: Nullable<DllImportSearchPath>) : nativeint =
        if libraryName = "libgpu_compute.so" then
            let fullPath = Path.GetFullPath(LibraryPath)
            if File.Exists(fullPath) then
                NativeLibrary.Load(fullPath)
            else
                nativeint 0
        else
            nativeint 0

    let mutable private resolverSet = false

    let ensureResolver() =
        if not resolverSet then
            NativeLibrary.SetDllImportResolver(Assembly.GetExecutingAssembly(), DllImportResolver(resolveLibrary))
            resolverSet <- true

    [<DllImport("libgpu_compute.so", CallingConvention = CallingConvention.Cdecl)>]
    extern void parallel_tempering_step(
        float[] positions,      // N*3 floats (x,y,z per node)
        int[] edges,            // E*2 ints (from,to per edge)
        float[] costs,          // T floats (cost per temperature)
        float[] temperatures,   // T floats
        int nodeCount,
        int edgeCount,
        int tempCount
    )

    [<DllImport("libgpu_compute.so", CallingConvention = CallingConvention.Cdecl)>]
    extern void collision_detection(
        float[] positions,      // N*3 floats
        int[] collisionPairs,   // Output: M*2 ints
        int nodeCount,
        float minDistance,
        int[] outCount          // Output: number of collisions
    )

    [<DllImport("libgpu_compute.so", CallingConvention = CallingConvention.Cdecl)>]
    extern void verlet_integrate(
        float[] positions,      // N*3 floats
        float[] velocities,     // N*3 floats
        float[] forces,         // N*3 floats
        int nodeCount,
        float timeStep,
        float[] masses          // N floats
    )

    [<DllImport("libgpu_compute.so", CallingConvention = CallingConvention.Cdecl)>]
    extern void marching_cubes(
        float[] densities,      // N*N*N floats (voxel grid)
        byte[] materials,       // N*N*N bytes (material IDs)
        float[] vertices,       // Output: vertex buffer
        float[] normals,        // Output: normal buffer
        int[] indices,          // Output: index buffer
        int[] materialIds,      // Output: material per triangle
        int resolution,         // Voxels per axis
        float isovalue,         // Surface threshold
        int[] outVertexCount,   // Output: number of vertices
        int[] outIndexCount     // Output: number of indices
    )

    [<DllImport("libgpu_compute.so", CallingConvention = CallingConvention.Cdecl)>]
    extern void hydraulic_erosion(
        float[] heightmap,      // N*N floats (2D height)
        float[] sediment,       // N*N floats (sediment layer)
        float[] waterFlow,      // N*N*2 floats (flow vectors)
        int resolution,         // Grid resolution
        float deltaTime,        // Time step
        float erosionRate,      // Soil removal rate
        float depositionRate,   // Sediment deposition rate
        int iterations          // Simulation steps
    )
