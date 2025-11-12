namespace Tests

module MutationsTests =

    open System
    open System.Numerics
    open global.Xunit
    open FsCheck
    open FsCheck.Xunit
    open TestInfrastructure.Core
    open TestInfrastructure.Assertions
    open TestInfrastructure.Generators
    open TestInfrastructure.Builders
    open ParallelTempering.Core

    [<Fact>]
    let ``perturbPosition preserves node count`` () =
        let graph = {
            Nodes = [| 1; 2; 3; 4; 5 |]
            Positions = [| for i in 0..4 -> Vector3(float32 i, 0.0f, 0.0f) |]
            Edges = []
        }

        let mutated = Mutations.perturbPosition 2 graph 1.0

        Assert.Equal(graph.Nodes.Length, mutated.Nodes.Length)
        Assert.Equal(graph.Positions.Length, mutated.Positions.Length)

    [<Fact>]
    let ``perturbPosition modifies correct position`` () =
        let graph = {
            Nodes = [| 1; 2; 3 |]
            Positions = [| Vector3(0.0f, 0.0f, 0.0f); Vector3(1.0f, 1.0f, 1.0f); Vector3(2.0f, 2.0f, 2.0f) |]
            Edges = []
        }

        let mutated = Mutations.perturbPosition 1 graph 1.0

        Assert.Equal(graph.Positions.[0], mutated.Positions.[0])
        Assert.Equal(graph.Positions.[2], mutated.Positions.[2])
        Assert.NotEqual(graph.Positions.[1], mutated.Positions.[1])

    [<Fact>]
    let ``perturbPosition scales with temperature`` () =
        let graph = {
            Nodes = [| 1 |]
            Positions = [| Vector3.Zero |]
            Edges = []
        }

        let lowTempTrials = [for _ in 1..100 -> Mutations.perturbPosition 0 graph 0.1]
        let highTempTrials = [for _ in 1..100 -> Mutations.perturbPosition 0 graph 10.0]

        let lowTempDist = lowTempTrials |> List.map (fun g -> Vector3.Distance(Vector3.Zero, g.Positions.[0])) |> List.average
        let highTempDist = highTempTrials |> List.map (fun g -> Vector3.Distance(Vector3.Zero, g.Positions.[0])) |> List.average

        Assert.True(lowTempDist < highTempDist, sprintf "Low temp %f >= high temp %f" lowTempDist highTempDist)

    [<Fact>]
    let ``addEdge preserves node count and positions`` () =
        let graph = {
            Nodes = [| 1; 2; 3; 4 |]
            Positions = [| for i in 0..3 -> Vector3(float32 i, 0.0f, 0.0f) |]
            Edges = []
        }

        let mutated = Mutations.addEdge graph

        Assert.Equal(graph.Nodes.Length, mutated.Nodes.Length)
        Assert.True((graph.Positions, mutated.Positions) ||> Array.forall2 (=))

    [<Fact>]
    let ``addEdge eventually adds edges`` () =
        let graph = {
            Nodes = [| 1; 2; 3; 4; 5 |]
            Positions = [| for i in 0..4 -> Vector3(float32 i, 0.0f, 0.0f) |]
            Edges = []
        }

        let trials = [for _ in 1..100 -> Mutations.addEdge graph]
        let anyAdded = trials |> List.exists (fun g -> g.Edges.Length > 0)

        Assert.True(anyAdded, "No edges added in 100 trials")

    [<Fact>]
    let ``addEdge does not create self-loops`` () =
        let graph = {
            Nodes = [| 1; 2; 3 |]
            Positions = [| for i in 0..2 -> Vector3(float32 i, 0.0f, 0.0f) |]
            Edges = []
        }

        let trials = [for _ in 1..1000 -> Mutations.addEdge graph]

        trials |> List.iter (fun g ->
            g.Edges |> List.iter (fun (i, j, _) ->
                Assert.NotEqual(i, j)
            )
        )

    [<Fact>]
    let ``addEdge indices within bounds`` () =
        let graph = {
            Nodes = [| 1; 2; 3; 4 |]
            Positions = [| for i in 0..3 -> Vector3(float32 i, 0.0f, 0.0f) |]
            Edges = []
        }

        let trials = [for _ in 1..1000 -> Mutations.addEdge graph]

        trials |> List.iter (fun g ->
            g.Edges |> List.iter (fun (i, j, _) ->
                Assert.True(i >= 0 && i < graph.Nodes.Length, sprintf "Index %d out of bounds" i)
                Assert.True(j >= 0 && j < graph.Nodes.Length, sprintf "Index %d out of bounds" j)
            )
        )

    [<Fact>]
    let ``removeEdge preserves node count and positions`` () =
        let graph = {
            Nodes = [| 1; 2; 3 |]
            Positions = [| for i in 0..2 -> Vector3(float32 i, 0.0f, 0.0f) |]
            Edges = [(0, 1, 1.0); (1, 2, 1.0)]
        }

        let mutated = Mutations.removeEdge graph

        Assert.Equal(graph.Nodes.Length, mutated.Nodes.Length)
        Assert.True((graph.Positions, mutated.Positions) ||> Array.forall2 (=))

    [<Fact>]
    let ``removeEdge eventually removes edges`` () =
        let graph = {
            Nodes = [| 1; 2; 3; 4; 5 |]
            Positions = [| for i in 0..4 -> Vector3(float32 i, 0.0f, 0.0f) |]
            Edges = [(0, 1, 1.0); (1, 2, 1.0); (2, 3, 1.0); (3, 4, 1.0)]
        }

        let trials = [for _ in 1..100 -> Mutations.removeEdge graph]
        let anyRemoved = trials |> List.exists (fun g -> g.Edges.Length < graph.Edges.Length)

        Assert.True(anyRemoved, "No edges removed in 100 trials")

    [<Fact>]
    let ``removeEdge respects minimum edge count`` () =
        let graph = {
            Nodes = [| 1; 2; 3 |]
            Positions = [| for i in 0..2 -> Vector3(float32 i, 0.0f, 0.0f) |]
            Edges = [(0, 1, 1.0); (1, 2, 1.0)]
        }

        let trials = [for _ in 1..100 -> Mutations.removeEdge graph]

        trials |> List.iter (fun g ->
            Assert.True(g.Edges.Length >= graph.Nodes.Length - 1, sprintf "Edge count %d < minimum %d" g.Edges.Length (graph.Nodes.Length - 1))
        )

    [<Fact>]
    let ``adaptive mutation at high temperature favors exploration`` () =
        let graph = {
            Nodes = [| 1; 2; 3; 4; 5 |]
            Positions = [| for i in 0..4 -> Vector3(float32 i, 0.0f, 0.0f) |]
            Edges = [(0, 1, 1.0)]
        }

        let trials = [for _ in 1..100 -> Mutations.adaptive graph 10.0]
        let topologyChanges = trials |> List.filter (fun g -> g.Edges.Length <> graph.Edges.Length) |> List.length

        Assert.True(topologyChanges > 30, sprintf "Only %d/%d topology changes at high temp" topologyChanges 100)

    [<Fact>]
    let ``adaptive mutation at low temperature favors refinement`` () =
        let graph = {
            Nodes = [| 1; 2; 3; 4; 5 |]
            Positions = [| for i in 0..4 -> Vector3(float32 i, 0.0f, 0.0f) |]
            Edges = [(0, 1, 1.0)]
        }

        let trials = [for _ in 1..100 -> Mutations.adaptive graph 0.1]
        let positionChanges =
            trials
            |> List.filter (fun g ->
                g.Positions |> Array.mapi (fun i p -> p <> graph.Positions.[i]) |> Array.exists id)
            |> List.length

        Assert.True(positionChanges > 70, sprintf "Only %d/%d position changes at low temp" positionChanges 100)

    [<Fact>]
    let ``mutations maintain graph structure invariants`` () =
        let graph = {
            Nodes = [| 1; 2; 3; 4 |]
            Positions = [| for i in 0..3 -> Vector3(float32 i, 0.0f, 0.0f) |]
            Edges = [(0, 1, 1.0); (1, 2, 1.0)]
        }

        let mutations = [
            Mutations.perturbPosition 0 graph 1.0
            Mutations.addEdge graph
            Mutations.removeEdge graph
            Mutations.adaptive graph 1.0
        ]

        mutations |> List.iter (fun g ->
            Assert.Equal(graph.Nodes.Length, g.Nodes.Length)
            Assert.Equal(graph.Positions.Length, g.Positions.Length)
            g.Edges |> List.iter (fun (i, j, w) ->
                Assert.True(i >= 0 && i < g.Nodes.Length)
                Assert.True(j >= 0 && j < g.Nodes.Length)
                Assert.True(i <> j)
                Assert.True(w >= 0.0)
            )
        )

    [<Property>]
    let ``perturbPosition always preserves node count`` (nodes: NonEmptyArray<int>) (index: NonNegativeInt) (temp: float) =
        if temp > 0.0 then
            let nodeArr = nodes.Get
            let idx = (int index) % nodeArr.Length
            let graph = {
                Nodes = nodeArr
                Positions = Array.init nodeArr.Length (fun i -> Vector3(float32 i, 0.0f, 0.0f))
                Edges = []
            }

            let mutated = Mutations.perturbPosition idx graph temp

            mutated.Nodes.Length = graph.Nodes.Length && mutated.Positions.Length = graph.Positions.Length
        else
            true
