namespace Tests

module ConstraintsTests =

    open System.Numerics
    open global.Xunit
    open FsCheck
    open FsCheck.Xunit
    open TestInfrastructure.Core
    open TestInfrastructure.Assertions
    open TestInfrastructure.PropertyDSL
    open TestInfrastructure.AlgebraicLaws
    open TestInfrastructure.Builders
    open TestInfrastructure.Generators
    open ProceduralGeneration
    open Terrain.Constraints
    open Physics.Constraints
    open ParallelTempering.Core

    [<Fact>]
    let ``Connectivity fully connected returns zero`` () =
        let graph = fullyConnectedGraph 5 |> toSpatialGraph
        shouldBeNonNegative (Constraints.connectivity graph)
        Assert.Equal(0.0, Constraints.connectivity graph)

    [<Fact>]
    let ``Connectivity penalizes disconnected graphs`` () =
        let disconnected = disconnectedGraph 3 2 |> toSpatialGraph
        let connected = fullyConnectedGraph 5 |> toSpatialGraph
        shouldPenalize (Constraints.connectivity disconnected) (Constraints.connectivity connected)

    [<Fact>]
    let ``Edge length deviation increases cost quadratically`` () =
        let ideal = spatialGraphWithIdealEdgeLengths 10.0 5
        let distorted = spatialGraphWithDistortedEdgeLengths 10.0 5.0 5
        shouldPenalize (Constraints.edgeLength 10.0 distorted) (Constraints.edgeLength 10.0 ideal)

    [<Fact>]
    let ``Collision penalizes overlapping nodes`` () =
        let separated : ParallelTempering.Core.SpatialGraph<int> = {
            Nodes = [| 0; 1 |]
            Positions = [| Vector3.Zero; Vector3(10.0f, 0.0f, 0.0f) |]
            Edges = []
        }
        let overlapping : ParallelTempering.Core.SpatialGraph<int> = {
            Nodes = [| 0; 1 |]
            Positions = [| Vector3.Zero; Vector3(0.5f, 0.0f, 0.0f) |]
            Edges = []
        }
        shouldPenalize (Constraints.collision 2.0 overlapping) (Constraints.collision 2.0 separated)

    [<Fact>]
    let ``Boundary penalty proportional to distance`` () =
        let inside : ParallelTempering.Core.SpatialGraph<int> = {
            Nodes = [| 0 |]
            Positions = [| Vector3(5.0f, 5.0f, 5.0f) |]
            Edges = []
        }
        let outside : ParallelTempering.Core.SpatialGraph<int> = {
            Nodes = [| 0 |]
            Positions = [| Vector3(150.0f, 150.0f, 150.0f) |]
            Edges = []
        }
        Assert.Equal(0.0, ParallelTempering.Core.Constraints.boundary 100.0 inside)
        Assert.True(ParallelTempering.Core.Constraints.boundary 100.0 outside > 0.0)

    [<Property>]
    let ``All constraints non-negative`` () =
        forAllGraphs (fun graph ->
            Constraints.connectivity graph >= 0.0 &&
            Constraints.edgeLength 10.0 graph >= 0.0 &&
            Constraints.collision 1.0 graph >= 0.0 &&
            ParallelTempering.Core.Constraints.boundary 100.0 graph >= 0.0 &&
            Constraints.distribution graph >= 0.0)

    [<Fact>]
    let ``Composition combines linearly`` () =
        let graph = fullyConnectedGraph 4 |> toSpatialGraph
        let weights = Map.ofList [("connectivity", 2.0); ("distribution", 3.0)]
        shouldCompose weights graph (fun w g ->
            w * (Constraints.connectivity g + Constraints.distribution g))

    [<Property>]
    let ``Deterministic repeated calls`` () =
        forAllGraphs (fun graph ->
            let result1 = Constraints.connectivity graph
            let result2 = Constraints.connectivity graph
            result1 = result2)

    [<Fact>]
    let ``Empty graph handled gracefully`` () =
        let empty = { Nodes = [||]; Positions = [||]; Edges = [] }
        shouldBeNonNegative (Constraints.connectivity empty)
        shouldBeNonNegative (Constraints.distribution empty)

    [<Fact>]
    let ``Weight sensitivity doubles contribution`` () =
        let graph = fullyConnectedGraph 3 |> toSpatialGraph
        let single = EnvironmentConstraints.build (Map.ofList [("connectivity", 1.0)]) graph
        let double = EnvironmentConstraints.build (Map.ofList [("connectivity", 2.0)]) graph
        shouldBeWithin 0.001 (single * 2.0) double

    [<Property>]
    let ``Commutativity of weighted combination`` () =
        forAllGraphs (fun graph ->
            let w1 = Map.ofList [("connectivity", 1.0); ("distribution", 2.0)]
            let w2 = Map.ofList [("distribution", 2.0); ("connectivity", 1.0)]
            abs (EnvironmentConstraints.build w1 graph - EnvironmentConstraints.build w2 graph) < 0.001)

    [<Fact>]
    let ``Zero weight produces zero`` () =
        let graph = fullyConnectedGraph 5 |> toSpatialGraph
        let weights = Map.ofList [("connectivity", 0.0)]
        Assert.Equal(0.0, EnvironmentConstraints.build weights graph)
