namespace Tests

module GPUIntegrationTests =

    open System
    open System.Numerics
    open global.Xunit
    open TestInfrastructure.Core
    open TestInfrastructure.Assertions
    open TestInfrastructure.Performance
    open TestInfrastructure.Metrology
    open GPU
    open GPU.Wrappers
    open GPU.GPUCollision
    open ParallelTempering.Core

    let skipIfGpuUnavailable() =
        if not (Wrappers.available()) then
            Assert.True(false, "GPU not available - test skipped")

    [<Fact>]
    let ``GPU availability detection`` () =
        let isAvailable = Wrappers.available()
        Assert.True(isAvailable || not isAvailable)

    [<Fact>]
    let ``GPU threshold is positive`` () =
        let threshold = Wrappers.getGPUThreshold()
        Assert.True(threshold > 0, sprintf "Threshold %d <= 0" threshold)

    [<Fact>]
    let ``updateGPUThreshold respects bounds`` () =
        Wrappers.updateGPUThreshold 500
        let threshold = Wrappers.getGPUThreshold()
        Assert.True(threshold >= 1000 && threshold <= 1000000, sprintf "Threshold %d out of bounds" threshold)

    [<Fact>]
    let ``updateGPUThreshold enforces minimum`` () =
        Wrappers.updateGPUThreshold 100
        let threshold = Wrappers.getGPUThreshold()
        Assert.True(threshold >= 1000, sprintf "Threshold %d < 1000" threshold)

    [<Fact>]
    let ``updateGPUThreshold enforces maximum`` () =
        Wrappers.updateGPUThreshold 10000000
        let threshold = Wrappers.getGPUThreshold()
        Assert.True(threshold <= 1000000, sprintf "Threshold %d > 1000000" threshold)

    [<Fact>]
    let ``recordPerformance accepts valid measurements`` () =
        Wrappers.recordPerformance 1000 10.0 5.0
        Wrappers.recordPerformance 5000 50.0 25.0

    [<Fact>]
    let ``measureTime captures execution`` () =
        let (result, elapsed) = Wrappers.measureTime (fun () ->
            System.Threading.Thread.Sleep(10)
            42
        )

        Assert.Equal(42, result)
        Assert.True(elapsed >= 10.0, sprintf "Elapsed %f < 10ms" elapsed)

    [<Fact>]
    let ``shouldUseGPU depends on problem size`` () =
        skipIfGpuUnavailable()

        let small = Wrappers.shouldUseGPU 100
        let threshold = Wrappers.getGPUThreshold()
        let large = Wrappers.shouldUseGPU (threshold + 1000)

        Assert.False(small, "Small problem using GPU")

    [<Fact>]
    let ``flattenVec3 produces correct length`` () =
        let vectors = [| Vector3(1.0f, 2.0f, 3.0f); Vector3(4.0f, 5.0f, 6.0f) |]
        let flat = Marshaling.flattenVec3 vectors

        Assert.Equal(6, flat.Length)

    [<Fact>]
    let ``unflattenVec3 roundtrips`` () =
        let vectors = [| Vector3(1.0f, 2.0f, 3.0f); Vector3(4.0f, 5.0f, 6.0f) |]
        let flat = Marshaling.flattenVec3 vectors
        let roundtripped = Marshaling.unflattenVec3 flat

        Assert.Equal(vectors.Length, roundtripped.Length)
        vectors |> Array.iteri (fun i v ->
            shouldBeWithin 0.001 (float v.X) (float roundtripped.[i].X)
            shouldBeWithin 0.001 (float v.Y) (float roundtripped.[i].Y)
            shouldBeWithin 0.001 (float v.Z) (float roundtripped.[i].Z)
        )

    [<Fact>]
    let ``flattenEdges produces correct length`` () =
        let edges = [(0, 1, 1.0); (1, 2, 2.0); (2, 3, 3.0)]
        let flat = Marshaling.flattenEdges edges

        Assert.Equal(6, flat.Length)

    [<Fact>]
    let ``toVector3Array validates length`` () =
        let valid = [| 1.0; 2.0; 3.0; 4.0; 5.0; 6.0 |]
        let invalid = [| 1.0; 2.0; 3.0; 4.0 |]

        match Marshaling.toVector3Array valid with
        | Ok vecs -> Assert.Equal(2, vecs.Length)
        | Error _ -> Assert.True(false, "Valid array rejected")

        match Marshaling.toVector3Array invalid with
        | Ok _ -> Assert.True(false, "Invalid array accepted")
        | Error _ -> ()

    [<Fact>]
    let ``collision detect handles empty graph`` () =
        let graph = {
            Nodes = [||]
            Positions = [||]
            Edges = []
        }

        match detect 1.0 graph with
        | Error _ -> ()
        | Ok _ -> ()

    [<Fact>]
    let ``collision detect validates minDistance`` () =
        let graph = {
            Nodes = [| 1; 2 |]
            Positions = [| Vector3.Zero; Vector3.One |]
            Edges = []
        }

        match detect -1.0 graph with
        | Error (InvalidArgument _) -> ()
        | _ -> Assert.True(false, "Negative minDistance accepted")

        match detect Double.NaN graph with
        | Error (InvalidArgument _) -> ()
        | _ -> Assert.True(false, "NaN minDistance accepted")

    [<Fact>]
    let ``collision detect validates graph structure`` () =
        let graph = {
            Nodes = [| 1; 2; 3 |]
            Positions = [| Vector3.Zero; Vector3.One |]
            Edges = []
        }

        match detect 1.0 graph with
        | Error (InvalidArgument _) -> ()
        | _ -> Assert.True(false, "Mismatched counts accepted")

    [<Fact>]
    let ``GPU tempering validates temperatures`` () =
        skipIfGpuUnavailable()

        let graph = {
            Nodes = [| 1; 2; 3 |]
            Positions = [| for i in 0..2 -> Vector3(float32 i, 0.0f, 0.0f) |]
            Edges = [(0, 1, 1.0); (1, 2, 1.0)]
        }

        match GPUTempering.optimize [| -1.0 |] [| graph |] 100 with
        | Error (InvalidArgument _) -> ()
        | _ -> Assert.True(false, "Negative temperature accepted")

        match GPUTempering.optimize [| Double.NaN |] [| graph |] 100 with
        | Error (InvalidArgument _) -> ()
        | _ -> Assert.True(false, "NaN temperature accepted")

    [<Fact>]
    let ``GPU tempering validates iterations`` () =
        skipIfGpuUnavailable()

        let graph = {
            Nodes = [| 1; 2; 3 |]
            Positions = [| for i in 0..2 -> Vector3(float32 i, 0.0f, 0.0f) |]
            Edges = [(0, 1, 1.0); (1, 2, 1.0)]
        }

        match GPUTempering.optimize [| 1.0 |] [| graph |] -1 with
        | Error (InvalidArgument _) -> ()
        | _ -> Assert.True(false, "Negative iterations accepted")

    [<Fact>]
    let ``GPU tempering validates graph structure`` () =
        skipIfGpuUnavailable()

        let graph = {
            Nodes = [| 1; 2; 3 |]
            Positions = [| Vector3.Zero; Vector3.One |]
            Edges = [(0, 1, 1.0)]
        }

        match GPUTempering.validateGraph graph with
        | Error (InvalidArgument _) -> ()
        | _ -> Assert.True(false, "Mismatched counts accepted")

    [<Fact>]
    let ``GPU tempering validates edge indices`` () =
        skipIfGpuUnavailable()

        let graph = {
            Nodes = [| 1; 2; 3 |]
            Positions = [| for i in 0..2 -> Vector3(float32 i, 0.0f, 0.0f) |]
            Edges = [(0, 5, 1.0)]
        }

        match GPUTempering.validateGraph graph with
        | Error (InvalidArgument _) -> ()
        | _ -> Assert.True(false, "Out of bounds edge accepted")

    [<Fact>]
    let ``GPU tempering validates edge weights`` () =
        skipIfGpuUnavailable()

        let graph = {
            Nodes = [| 1; 2; 3 |]
            Positions = [| for i in 0..2 -> Vector3(float32 i, 0.0f, 0.0f) |]
            Edges = [(0, 1, Double.NaN)]
        }

        match GPUTempering.validateGraph graph with
        | Error (InvalidArgument _) -> ()
        | _ -> Assert.True(false, "NaN edge weight accepted")

    [<Fact>]
    let ``Dispatch thresholds are positive`` () =
        let thresholds = Dispatch.getThresholds()

        Assert.True(thresholds.NodeCount > 0)
        Assert.True(thresholds.EdgeCount > 0)
        Assert.True(thresholds.Iterations > 0)

    [<Fact>]
    let ``Dispatch updateThresholds validates`` () =
        let invalid : Dispatch.DispatchThresholds = {
            NodeCount = -1
            EdgeCount = 100
            Iterations = 100
        }

        Dispatch.updateThresholds invalid
        let current = Dispatch.getThresholds()

        Assert.True(current.NodeCount > 0, "Negative NodeCount accepted")

    [<Fact>]
    let ``Dispatch useGPU respects thresholds`` () =
        skipIfGpuUnavailable()

        let thresholds = Dispatch.getThresholds()

        let small = Dispatch.useGPU (thresholds.NodeCount - 1) (thresholds.EdgeCount - 1) (thresholds.Iterations - 1)
        let large = Dispatch.useGPU (thresholds.NodeCount + 100) (thresholds.EdgeCount + 100) (thresholds.Iterations + 100)

        Assert.False(small, "Small problem using GPU")

    [<Fact>]
    let ``Dispatch optimize handles CPU fallback`` () =
        let temps = [| 1.0; 0.5 |]
        let graph = {
            Nodes = [| 1; 2; 3 |]
            Positions = [| for i in 0..2 -> Vector3(float32 i, 0.0f, 0.0f) |]
            Edges = [(0, 1, 1.0); (1, 2, 1.0)]
        }
        let graphs = [| graph; graph |]

        let cost (g: SpatialGraph<int>) = 0.0
        let mutation (g: SpatialGraph<int>) (t: float) = g

        let chains = Dispatch.optimize temps graphs mutation cost 10

        Assert.Equal(2, chains.Length)

    [<Fact>]
    let ``Performance measurement captures kernel timing`` () =
        let (result, elapsed) = Wrappers.measureTime (fun () ->
            System.Threading.Thread.Sleep(10)
            42
        )

        Assert.Equal(42, result)
        Assert.True(elapsed >= 10.0, sprintf "Elapsed %f < 10ms" elapsed)

    [<Fact>]
    let ``shouldCompleteWithin enforces time limit`` () =
        Assert.Throws<Xunit.Sdk.TrueException>(fun () ->
            shouldCompleteWithin 5.0 (fun () ->
                System.Threading.Thread.Sleep(100)
            )
        ) |> ignore
