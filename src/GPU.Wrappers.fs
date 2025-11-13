namespace GPU

open System
open System.Numerics
open System.Runtime.InteropServices
open ParallelTempering.Core
open Voxel
open Physics
open GPU.Compute

/// CUDA error types
type CudaError =
    | OutOfMemory of string
    | InvalidArgument of string
    | LaunchFailed of string
    | DeviceNotAvailable of string
    | BufferOverflow of string
    | NativeException of exn

/// Result pipeline operators for GPU operations
module Result =

    /// Bind operator for chaining Results
    let (>>=) result f =
        match result with
        | Ok value -> f value
        | Error e -> Error e

    /// Map operator for transforming success values
    let (<!>) f result =
        match result with
        | Ok value -> Ok (f value)
        | Error e -> Error e

    /// Apply operator for function in Result
    let (<*>) resultF resultX =
        match resultF, resultX with
        | Ok f, Ok x -> Ok (f x)
        | Error e, _ -> Error e
        | _, Error e -> Error e

    /// Traverse array of Results into Result of array
    let traverse (f: 'a -> Result<'b, 'e>) (arr: 'a array) : Result<'b array, 'e> =
        let rec loop acc remaining =
            match remaining with
            | [] -> Ok (Array.ofList (List.rev acc))
            | x :: xs ->
                match f x with
                | Ok value -> loop (value :: acc) xs
                | Error e -> Error e
        loop [] (Array.toList arr)

    /// Sequence array of Results
    let sequence (results: Result<'a, 'e> array) : Result<'a array, 'e> =
        traverse id results

/// Computation expression for CUDA resource management
type CudaBuilder() =

    member _.Bind(result: Result<'T, CudaError>, f: 'T -> Result<'U, CudaError>) : Result<'U, CudaError> =
        match result with
        | Ok value -> f value
        | Error e -> Error e

    member _.Return(value: 'T) : Result<'T, CudaError> =
        Ok value

    member _.ReturnFrom(result: Result<'T, CudaError>) : Result<'T, CudaError> =
        result

    member _.Zero() : Result<unit, CudaError> =
        Ok ()

    member _.Delay(f: unit -> Result<'T, CudaError>) : unit -> Result<'T, CudaError> =
        f

    member _.Run(f: unit -> Result<'T, CudaError>) : Result<'T, CudaError> =
        try f()
        with ex -> Error (NativeException ex)

/// Global cuda computation expression instance
module CudaInstance =
    let cuda = CudaBuilder()

/// F# wrappers for GPU-accelerated algorithms
module Wrappers =

    open System.Diagnostics
    open System.Collections.Concurrent

    /// Performance measurement for adaptive thresholding
    type PerformanceMeasurement = {
        ProblemSize: int
        CPUTimeMs: float
        GPUTimeMs: float
        Timestamp: System.DateTime
    }

    /// Adaptive GPU dispatch threshold
    let mutable private gpuThreshold = 10000

    /// Performance history for auto-calibration (last 100 measurements)
    let private performanceHistory = ConcurrentQueue<PerformanceMeasurement>()

    /// Last calibration time
    let mutable private lastCalibration = System.DateTime.UtcNow

    /// Check if CUDA is available
    let available () : bool =
        try
            CUDA.ensureResolver()
            CUDA.parallel_tempering_step([||], [||], [||], [||], 0, 0, 0)
            true
        with
        | :? System.DllNotFoundException as ex ->
            eprintfn "DLL not found: %s" ex.Message
            false
        | ex ->
            eprintfn "CUDA check failed: %s" ex.Message
            false

    /// Get adaptive threshold for GPU dispatch
    let getGPUThreshold () : int = gpuThreshold

    /// Update GPU threshold based on performance profiling
    let updateGPUThreshold (newThreshold: int) : unit =
        gpuThreshold <- max 1000 (min 1000000 newThreshold)

    /// Record performance measurement
    let recordPerformance (problemSize: int) (cpuTimeMs: float) (gpuTimeMs: float) : unit =
        let measurement = {
            ProblemSize = problemSize
            CPUTimeMs = cpuTimeMs
            GPUTimeMs = gpuTimeMs
            Timestamp = System.DateTime.UtcNow
        }
        performanceHistory.Enqueue(measurement)

        // Keep only last 100 measurements
        while performanceHistory.Count > 100 do
            performanceHistory.TryDequeue() |> ignore

    /// Find optimal threshold from measurements (where GPU becomes faster)
    let findOptimalThreshold () : int option =
        let measurements = performanceHistory.ToArray()

        if measurements.Length < 10 then None
        else
            // Group by problem size and average
            let grouped =
                measurements
                |> Array.groupBy (fun m -> m.ProblemSize)
                |> Array.map (fun (size, ms) ->
                    let avgCpu = ms |> Array.averageBy (fun m -> m.CPUTimeMs)
                    let avgGpu = ms |> Array.averageBy (fun m -> m.GPUTimeMs)
                    (size, avgCpu, avgGpu)
                )
                |> Array.sortBy (fun (size, _, _) -> size)

            // Find crossover where GPU becomes faster (with 20% margin for stability)
            grouped
            |> Array.tryFind (fun (_, cpu, gpu) -> gpu * 1.2 < cpu)
            |> Option.map (fun (size, _, _) -> size)

    /// Auto-calibrate threshold if enough data and 24 hours have passed
    let autoCalibrate () : unit =
        let hoursSinceLastCalibration = (System.DateTime.UtcNow - lastCalibration).TotalHours

        if hoursSinceLastCalibration >= 24.0 && performanceHistory.Count >= 10 then
            match findOptimalThreshold() with
            | Some optimalThreshold ->
                updateGPUThreshold optimalThreshold
                lastCalibration <- System.DateTime.UtcNow
            | None -> ()

    /// Measure execution time of a function
    let inline measureTime (f: unit -> 'T) : 'T * float =
        let sw = Stopwatch.StartNew()
        let result = f()
        sw.Stop()
        (result, sw.Elapsed.TotalMilliseconds)

    /// Decide whether to use GPU based on problem size
    let shouldUseGPU (problemSize: int) : bool =
        // Try auto-calibration
        autoCalibrate()

        problemSize >= gpuThreshold && available()

/// Type-driven marshaling helpers
module Marshaling =

    /// Convert float triplet array to Vector3
    let inline floatTripleToVector3 (xyz: float array) : Vector3 =
        Vector3(float32 xyz.[0], float32 xyz.[1], float32 xyz.[2])

    /// Flatten Vector3 array using type-driven stride
    let inline flattenVec3 (vectors: Vector3 array) : float array =
        vectors |> Array.collect (fun v -> [| float v.X; float v.Y; float v.Z |])

    /// Unflatten to Vector3 array using type-driven stride
    let inline unflattenVec3 (floats: float array) : Vector3 array =
        floats
        |> Array.chunkBySize 3
        |> Array.map floatTripleToVector3

    /// Flatten edge list to int array
    let inline flattenEdges (edges: (int * int * 'T) list) : int array =
        edges |> List.collect (fun (a, b, _) -> [a; b]) |> List.toArray

    /// Pin array during CUDA operation to prevent GC moves
    let inline withPinned (array: 'T array) (f: 'T array -> 'R) : 'R =
        let handle = GCHandle.Alloc(array, GCHandleType.Pinned)
        try
            f array
        finally
            handle.Free()

    /// Pin multiple arrays for CUDA operation
    let inline withPinned2 (arr1: 'T1 array) (arr2: 'T2 array) (f: 'T1 array -> 'T2 array -> 'R) : 'R =
        let h1 = GCHandle.Alloc(arr1, GCHandleType.Pinned)
        let h2 = GCHandle.Alloc(arr2, GCHandleType.Pinned)
        try
            f arr1 arr2
        finally
            h2.Free()
            h1.Free()

    /// Safely convert float array to Vector3 array with validation
    let inline toVector3Array (floats: float array) : Result<Vector3 array, CudaError> =
        if floats.Length % 3 <> 0 then
            Error (InvalidArgument $"Float array length {floats.Length} not divisible by 3")
        else
            try
                let vectors =
                    floats
                    |> Array.chunkBySize 3
                    |> Array.map (fun xyz ->
                        if xyz.Length <> 3 then
                            failwith $"Unexpected chunk size {xyz.Length}"
                        floatTripleToVector3 xyz)
                Ok vectors
            with ex ->
                Error (NativeException ex)

/// GPU-accelerated parallel tempering
module GPUTempering =

    /// Validate graph structure before GPU operations
    let validateGraph (graph: SpatialGraph<'T>) : Result<unit, CudaError> =
        if graph.Positions.Length <> graph.Nodes.Length then
            Error (InvalidArgument $"Position count {graph.Positions.Length} != Node count {graph.Nodes.Length}")
        elif graph.Edges |> List.exists (fun (a, b, _) -> a < 0 || b < 0 || a >= graph.Nodes.Length || b >= graph.Nodes.Length) then
            Error (InvalidArgument "Edge indices out of bounds")
        elif graph.Edges |> List.exists (fun (_, _, w) -> w < 0.0 || System.Double.IsNaN(w) || System.Double.IsInfinity(w)) then
            Error (InvalidArgument "Edge weights must be finite and non-negative")
        else
            Ok ()

    /// Flatten graph for GPU transfer
    let flatten (graph: SpatialGraph<'T>) : float array * int array =
        let positions = Marshaling.flattenVec3 graph.Positions
        let edges = Marshaling.flattenEdges graph.Edges
        (positions, edges)

    /// Unflatten positions back to graph
    let unflatten (positions: float array) (graph: SpatialGraph<'T>) : SpatialGraph<'T> =
        { graph with Positions = Marshaling.unflattenVec3 positions }

    /// Run optimization on GPU
    let optimize
        (temperatures: float array)
        (graphs: SpatialGraph<'T> array)
        (iterations: int) : Result<SpatialGraph<'T> array, CudaError> =

        if not (Wrappers.available()) then
            Error (DeviceNotAvailable "CUDA device not available")
        elif temperatures |> Array.exists (fun t -> t <= 0.0 || System.Double.IsNaN(t) || System.Double.IsInfinity(t)) then
            Error (InvalidArgument "Temperatures must be positive and finite")
        elif iterations <= 0 then
            Error (InvalidArgument $"Iterations must be positive, got {iterations}")
        elif graphs.Length = 0 then
            Error (InvalidArgument "No graphs provided")
        else
            // Validate all graphs
            match graphs |> Array.tryPick (fun g -> match validateGraph g with Error e -> Some e | Ok () -> None) with
            | Some err -> Error err
            | None ->
                try
                    // Flatten all graphs
                    let flatGraphs = graphs |> Array.map flatten
                    let nodeCount = graphs.[0].Nodes.Length
                    let edgeCount = graphs.[0].Edges.Length

                    // Allocate unified buffer
                    let totalPositions = nodeCount * 3 * temperatures.Length
                    if totalPositions > System.Int32.MaxValue / 8 then
                        Error (BufferOverflow "Position buffer would exceed safe size")
                    else
                        let positions = Array.zeroCreate<float> totalPositions

                        // Copy initial positions
                        flatGraphs |> Array.iteri (fun i (pos, _) ->
                            System.Array.Copy(pos, 0, positions, i * nodeCount * 3, nodeCount * 3)
                        )

                        // Use first graph's edges (shared topology)
                        let edges = snd flatGraphs.[0]
                        let costs = Array.zeroCreate<float> temperatures.Length

                        // Pin arrays and run iterations
                        Marshaling.withPinned2 positions edges (fun pinnedPos pinnedEdges ->
                            Marshaling.withPinned2 costs temperatures (fun pinnedCosts pinnedTemps ->
                                for _ in 1 .. iterations do
                                    CUDA.parallel_tempering_step(
                                        pinnedPos,
                                        pinnedEdges,
                                        pinnedCosts,
                                        pinnedTemps,
                                        nodeCount,
                                        edgeCount,
                                        temperatures.Length
                                    )
                            )
                        )

                        // Extract results
                        let results = graphs |> Array.mapi (fun i graph ->
                            let offset = i * nodeCount * 3
                            if offset + nodeCount * 3 > positions.Length then
                                Error (BufferOverflow $"Offset {offset} exceeds buffer size {positions.Length}")
                            else
                                let tempPositions = Array.sub positions offset (nodeCount * 3)
                                Ok (unflatten tempPositions graph)
                        )

                        // Check if any extraction failed
                        match results |> Array.tryPick (function Error e -> Some e | Ok _ -> None) with
                        | Some err -> Error err
                        | None ->
                            let graphs = results |> Array.choose (function Ok g -> Some g | Error _ -> None)
                            Ok graphs
                with
                | ex -> Error (NativeException ex)

/// GPU-accelerated collision detection
module GPUCollision =

    let private detectGPU (minDistance: float) (graph: SpatialGraph<'T>) : Result<(int * int) list, CudaError> =
        CudaInstance.cuda {
            let positions, _ = GPUTempering.flatten graph
            let nodeCount = graph.Nodes.Length
            let maxCollisions = nodeCount * nodeCount / 2

            if maxCollisions > System.Int32.MaxValue / 2 then
                return! Error (BufferOverflow $"Too many potential collisions: {maxCollisions}")
            else
                let collisionPairs = Array.zeroCreate<int> (maxCollisions * 2)
                let count = [| 0 |]

                do! Marshaling.withPinned positions (fun pinnedPos ->
                    Marshaling.withPinned2 collisionPairs count (fun pinnedPairs pinnedCount ->
                        CUDA.collision_detection(
                            pinnedPos,
                            pinnedPairs,
                            nodeCount,
                            float minDistance,
                            pinnedCount
                        )
                        Ok ()
                    )
                )

                if count.[0] < 0 || count.[0] > maxCollisions then
                    return! Error (BufferOverflow $"Invalid collision count: {count.[0]}")
                else
                    return collisionPairs
                        |> Array.chunkBySize 2
                        |> Array.take count.[0]
                        |> Array.map (fun pair -> (pair.[0], pair.[1]))
                        |> Array.toList
        }

    let detect (minDistance: float) (graph: SpatialGraph<'T>) : Result<(int * int) list, CudaError> =
        if minDistance <= 0.0 || System.Double.IsNaN(minDistance) || System.Double.IsInfinity(minDistance) then
            Error (InvalidArgument $"minDistance must be positive and finite, got {minDistance}")
        elif graph.Positions.Length <> graph.Nodes.Length then
            Error (InvalidArgument $"Position count {graph.Positions.Length} != Node count {graph.Nodes.Length}")
        elif not (Wrappers.available()) then
            // Fallback to CPU spatial hash
            Ok (SpatialHash.detectCollisions minDistance graph.Positions)
        else
            let n = graph.Nodes.Length

            // 5% of the time in sweet spot, measure both CPU and GPU
            let shouldMeasureBoth = ThreadSafeRandom.nextInt(100) < 5 && n >= 100 && n <= 10000

            if shouldMeasureBoth then
                let (cpuResult, cpuTime) = Wrappers.measureTime (fun () ->
                    SpatialHash.detectCollisions minDistance graph.Positions
                )

                let (gpuResult, gpuTime) = Wrappers.measureTime (fun () ->
                    detectGPU minDistance graph
                )

                // Record measurement for adaptive threshold
                match gpuResult with
                | Ok _ -> Wrappers.recordPerformance n cpuTime gpuTime
                | Error _ -> ()  // GPU failed, don't record

                Ok cpuResult  // Return CPU result (more reliable)
            elif Wrappers.shouldUseGPU n then
                detectGPU minDistance graph
            else
                Ok (SpatialHash.detectCollisions minDistance graph.Positions)

/// GPU-accelerated Verlet integration
module GPUVerlet =

    let integrate (state: PhysicsState) : Result<PhysicsState, CudaError> =
        // Validate physics state
        if state.Positions.Length <> state.Properties.Length then
            Error (InvalidArgument $"Position count {state.Positions.Length} != Property count {state.Properties.Length}")
        elif state.Positions.Length = 0 then
            Error (InvalidArgument "Cannot integrate empty physics state")
        elif float state.TimeStep <= 0.0 || System.Double.IsNaN(float state.TimeStep) || System.Double.IsInfinity(float state.TimeStep) then
            Error (InvalidArgument $"TimeStep must be positive and finite, got {state.TimeStep}")
        elif state.Properties |> Array.exists (fun p -> float p.Mass <= 0.0 || System.Double.IsNaN(float p.Mass) || System.Double.IsInfinity(float p.Mass)) then
            Error (InvalidArgument "All masses must be positive and finite")
        elif state.Positions.Length > System.Int32.MaxValue / 3 then
            Error (BufferOverflow $"Position count {state.Positions.Length} would overflow marshaling buffer")
        elif not (Wrappers.available()) then
            Ok (Verlet.integrate state)
        else
            CudaInstance.cuda {
                let positions =
                    state.Positions
                    |> Array.collect (fun v -> [| float v.X; float v.Y; float v.Z |])

                let velocities =
                    state.Properties
                    |> Array.collect (fun p -> [| float p.Velocity.X; float p.Velocity.Y; float p.Velocity.Z |])

                let forces = Array.zeroCreate<float> (state.Positions.Length * 3)

                let masses =
                    state.Properties
                    |> Array.map (fun p -> float p.Mass)

                do! Marshaling.withPinned positions (fun pinnedPos ->
                    Marshaling.withPinned velocities (fun pinnedVel ->
                        Marshaling.withPinned forces (fun pinnedForces ->
                            Marshaling.withPinned masses (fun pinnedMasses ->
                                CUDA.verlet_integrate(
                                    pinnedPos,
                                    pinnedVel,
                                    pinnedForces,
                                    state.Positions.Length,
                                    float state.TimeStep,
                                    pinnedMasses
                                )
                                Ok ()
                            )
                        )
                    )
                )

                let! newPositions = Marshaling.toVector3Array positions
                let! newVelocities = Marshaling.toVector3Array velocities

                if newVelocities.Length <> state.Properties.Length then
                    return! Error (InvalidArgument $"Velocity result count {newVelocities.Length} != Property count {state.Properties.Length}")
                else
                    let newProperties =
                        Array.mapi2 (fun i (props: PhysicalProperties) (vel: Vector3) ->
                            { props with Velocity = vel }
                        ) state.Properties newVelocities

                    return { state with Positions = newPositions; Properties = newProperties }
            }

/// GPU-accelerated marching cubes for voxel meshing
module GPUMarchingCubes =

    let generateMesh (chunk: VoxelChunk) (isovalue: float32) : Result<MeshData, CudaError> =
        // Validate inputs
        if System.Single.IsNaN(isovalue) || System.Single.IsInfinity(isovalue) then
            Error (InvalidArgument $"Isovalue must be finite, got {isovalue}")
        elif chunk.Resolution <= 0 then
            Error (InvalidArgument $"Chunk resolution must be positive, got {chunk.Resolution}")
        elif chunk.Graph.Nodes.Length = 0 then
            Error (InvalidArgument "Chunk has no voxel data")
        elif not (Wrappers.available()) then
            Error (DeviceNotAvailable "GPU not available for marching cubes")
        else
            CudaInstance.cuda {
                let res = chunk.Resolution
                let totalVoxels = res * res * res

                if chunk.Graph.Nodes.Length <> totalVoxels then
                    return! Error (InvalidArgument $"Node count {chunk.Graph.Nodes.Length} != expected {totalVoxels}")
                elif totalVoxels > System.Int32.MaxValue / 15 then
                    return! Error (BufferOverflow $"Voxel count {totalVoxels} too large for marching cubes buffer")
                else
                    // Extract density and material arrays
                    let densities =
                        chunk.Graph.Nodes
                        |> Array.map (fun v -> float v.Density)

                    let materials =
                        chunk.Graph.Nodes
                        |> Array.map (fun v -> v.Material)

                    // Allocate output buffers (estimate max size)
                    let maxTriangles = totalVoxels * 5  // Conservative estimate
                    let maxVertices = maxTriangles * 3

                    let vertices = Array.zeroCreate<float> (maxVertices * 3)
                    let normals = Array.zeroCreate<float> (maxVertices * 3)
                    let indices = Array.zeroCreate<int> maxTriangles
                    let materialIds = Array.zeroCreate<int> maxTriangles

                    let vertexCount = Array.zeroCreate<int> 1
                    let indexCount = Array.zeroCreate<int> 1

                    // Run GPU marching cubes with pinned arrays
                    do! Marshaling.withPinned densities (fun pinnedDens ->
                        Marshaling.withPinned materials (fun pinnedMats ->
                            Marshaling.withPinned vertices (fun pinnedVerts ->
                                Marshaling.withPinned normals (fun pinnedNorms ->
                                    Marshaling.withPinned indices (fun pinnedInds ->
                                        Marshaling.withPinned materialIds (fun pinnedMatIds ->
                                            Marshaling.withPinned2 vertexCount indexCount (fun pinnedVCount pinnedICount ->
                                                CUDA.marching_cubes(
                                                    pinnedDens,
                                                    pinnedMats,
                                                    pinnedVerts,
                                                    pinnedNorms,
                                                    pinnedInds,
                                                    pinnedMatIds,
                                                    res,
                                                    float isovalue,
                                                    pinnedVCount,
                                                    pinnedICount
                                                )
                                                Ok ()
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )

                    // Validate output counts
                    if vertexCount.[0] < 0 || vertexCount.[0] > maxVertices then
                        return! Error (BufferOverflow $"Invalid vertex count: {vertexCount.[0]}")
                    elif indexCount.[0] < 0 || indexCount.[0] > maxTriangles then
                        return! Error (BufferOverflow $"Invalid index count: {indexCount.[0]}")
                    elif indexCount.[0] % 3 <> 0 then
                        return! Error (InvalidArgument $"Index count {indexCount.[0]} is not divisible by 3")
                    else
                        // Extract actual data
                        let! actualVertices =
                            vertices
                            |> Array.take (vertexCount.[0] * 3)
                            |> Marshaling.toVector3Array

                        let! actualNormals =
                            normals
                            |> Array.take (vertexCount.[0] * 3)
                            |> Marshaling.toVector3Array

                        let actualIndices = Array.take indexCount.[0] indices
                        let actualMaterialIds = Array.take (indexCount.[0] / 3) materialIds |> Array.map byte

                        // Generate UVs (simple box mapping)
                        let uvs = actualVertices |> Array.map (fun v -> System.Numerics.Vector2(v.X, v.Z))

                        return {
                            Vertices = actualVertices
                            Normals = actualNormals
                            UVs = uvs
                            Indices = actualIndices
                            MaterialIds = actualMaterialIds
                        }
            }

    /// Generate mesh and cache in chunk
    let meshChunk (chunk: VoxelChunk) : Result<VoxelChunk, CudaError> =
        generateMesh chunk 0.5f
        |> Result.map (fun mesh -> { chunk with Mesh = Some mesh })

/// GPU-accelerated hydraulic erosion
module GPUErosion =

    type ErosionParams = {
        TimeStep: float
        ErosionRate: float
        DepositionRate: float
    }

    let defaultErosionParams = {
        TimeStep = 0.016       // 60fps
        ErosionRate = 0.3
        DepositionRate = 0.1
    }

    let erodeHeightmap (heightmap: float32 array array) (iterations: int) (erosionParams: ErosionParams)
        : Result<float32 array array, CudaError> =
        // Validate inputs
        if heightmap.Length = 0 then
            Error (InvalidArgument "Heightmap is empty")
        elif heightmap |> Array.exists (fun row -> row.Length <> heightmap.[0].Length) then
            Error (InvalidArgument "Heightmap rows have inconsistent lengths")
        elif iterations <= 0 then
            Error (InvalidArgument $"Iterations must be positive, got {iterations}")
        elif erosionParams.TimeStep <= 0.0 || System.Double.IsNaN(erosionParams.TimeStep) || System.Double.IsInfinity(erosionParams.TimeStep) then
            Error (InvalidArgument $"TimeStep must be positive and finite, got {erosionParams.TimeStep}")
        elif erosionParams.ErosionRate < 0.0 || System.Double.IsNaN(erosionParams.ErosionRate) || System.Double.IsInfinity(erosionParams.ErosionRate) then
            Error (InvalidArgument $"ErosionRate must be non-negative and finite, got {erosionParams.ErosionRate}")
        elif erosionParams.DepositionRate < 0.0 || System.Double.IsNaN(erosionParams.DepositionRate) || System.Double.IsInfinity(erosionParams.DepositionRate) then
            Error (InvalidArgument $"DepositionRate must be non-negative and finite, got {erosionParams.DepositionRate}")
        elif not (Wrappers.available()) then
            Error (DeviceNotAvailable "GPU not available for erosion")
        else
            CudaInstance.cuda {
                let res = heightmap.Length

                if res * res > System.Int32.MaxValue / 2 then
                    return! Error (BufferOverflow $"Heightmap size {res}x{res} too large for erosion buffer")
                else
                    // Flatten 2D heightmap
                    let flatHeightmap =
                        heightmap
                        |> Array.collect (Array.map float)

                    let sediment = Array.zeroCreate<float> (res * res)
                    let waterFlow = Array.zeroCreate<float> (res * res * 2)

                    // Run GPU erosion simulation with pinned arrays
                    do! Marshaling.withPinned flatHeightmap (fun pinnedHeight ->
                        Marshaling.withPinned sediment (fun pinnedSed ->
                            Marshaling.withPinned waterFlow (fun pinnedFlow ->
                                CUDA.hydraulic_erosion(
                                    pinnedHeight,
                                    pinnedSed,
                                    pinnedFlow,
                                    res,
                                    erosionParams.TimeStep,
                                    erosionParams.ErosionRate,
                                    erosionParams.DepositionRate,
                                    iterations
                                )
                                Ok ()
                            )
                        )
                    )

                    // Unflatten back to 2D
                    return flatHeightmap
                        |> Array.chunkBySize res
                        |> Array.map (Array.map float32)
            }

/// Routing: CPU vs GPU decision
module Dispatch =

    type DispatchThresholds = {
        NodeCount: int         // Below: CPU (cache-friendly), Above: GPU (parallelism)
        EdgeCount: int
        Iterations: int
    }

    let mutable private thresholds = {
        NodeCount = 200
        EdgeCount = 500
        Iterations = 1000
    }

    let getThresholds () : DispatchThresholds = thresholds

    let updateThresholds (newThresholds: DispatchThresholds) : unit =
        if newThresholds.NodeCount > 0 && newThresholds.EdgeCount > 0 && newThresholds.Iterations > 0 then
            thresholds <- newThresholds

    /// Decide whether to use GPU based on problem size
    let useGPU (nodeCount: int) (edgeCount: int) (iterations: int) : bool =
        Wrappers.available() &&
        nodeCount >= thresholds.NodeCount &&
        edgeCount >= thresholds.EdgeCount &&
        iterations >= thresholds.Iterations

    /// Adaptive dispatch
    let optimize
        (temperatures: float array)
        (graphs: ParallelTempering.Core.SpatialGraph<'T> array)
        (mutate: ParallelTempering.Core.MutationStrategy<'T>)
        (cost: ParallelTempering.Core.CostFunction<'T>)
        (iterations: int) : ParallelTempering.Core.Chain<'T> array =

        let nodeCount = graphs.[0].Nodes.Length
        let edgeCount = graphs.[0].Edges.Length

        if useGPU nodeCount edgeCount iterations then
            match GPUTempering.optimize temperatures graphs iterations with
            | Ok optimized ->
                Array.map2 (fun temp graph ->
                    {
                        State = graph
                        Cost = cost graph
                        Temperature = temp
                        AcceptanceRate = 0.5
                    }
                ) temperatures optimized
            | Error _ ->
                // GPU failed, fallback to CPU
                let initialStates = graphs
                ParallelTempering.run temperatures initialStates mutate cost iterations 10
        else
            // CPU path
            let initialStates = graphs
            ParallelTempering.run temperatures initialStates mutate cost iterations 10

    /// Adaptive dispatch with convergence guarantees
    /// Uses GPU-accelerated convergence checks for large problems
    let optimizeUntilConvergence
        (temperatures: float array)
        (graphs: ParallelTempering.Core.SpatialGraph<'T> array)
        (mutate: ParallelTempering.Core.MutationStrategy<'T>)
        (cost: ParallelTempering.Core.CostFunction<'T>)
        (maxIterations: int)
        (strictness: float)
        (onProgress: int -> ParallelTempering.Core.Chain<'T> array -> ParallelTempering.Core.Convergence.ConvergenceMetrics -> unit)
        : (ParallelTempering.Core.Chain<'T> array * ParallelTempering.Core.Convergence.ConvergenceMetrics) =

        let nodeCount = graphs.[0].Nodes.Length
        let edgeCount = graphs.[0].Edges.Length
        let swapFreq = ParallelTempering.swapFrequency nodeCount
        let checkFreq = max 50 (maxIterations / 20)  // Check every 5%

        // Always use CPU convergence module with adaptive GPU acceleration internally
        // The CPU implementation automatically uses GPU for large-scale diagnostics
        ParallelTempering.Core.Convergence.runUntilConvergence
            temperatures
            graphs
            mutate
            cost
            maxIterations
            swapFreq
            checkFreq
            strictness
            onProgress
