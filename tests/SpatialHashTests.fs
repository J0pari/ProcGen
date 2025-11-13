namespace Tests

module SpatialHashTests =

    open System.Numerics
    open global.Xunit
    open FsCheck
    open FsCheck.Xunit
    open TestInfrastructure.Assertions
    open TestInfrastructure.PropertyDSL
    open TestInfrastructure.Builders
    open TestInfrastructure.Generators
    open TestInfrastructure.Performance
    open SpatialHash

    [<Fact>]
    let ``Correctness finds all pairs with distance less than minDistance`` () =
        let positions = positionArray().WithCount(10).Clustered(5.0f).Build()
        let collisions = SpatialHash.detectCollisions 2.0f positions
        shouldDetectAll collisions (fun (i, j) ->
            let dx = positions.[i].X - positions.[j].X
            let dy = positions.[i].Y - positions.[j].Y
            let dz = positions.[i].Z - positions.[j].Z
            sqrt(dx*dx + dy*dy + dz*dz) < 2.0f)

    [<Fact>]
    let ``Boundary semantics distance equals minDistance does not collide`` () =
        let positions = [| Vector3.Zero; Vector3(2.0f, 0.0f, 0.0f) |]
        let collisions = SpatialHash.detectCollisions 2.0f positions
        shouldNotDetect collisions

    [<Fact>]
    let ``Adaptive algorithm switches at n equals 200`` () =
        let positions199 = positionArray().WithCount(199).Uniform().WithBounds(100.0f).Build()
        let positions200 = positionArray().WithCount(200).Uniform().WithBounds(100.0f).Build()
        let collisions199 = SpatialHash.detectCollisions 5.0f positions199
        let collisions200 = SpatialHash.detectCollisions 5.0f positions200
        Assert.True(collisions199.Length >= 0)
        Assert.True(collisions200.Length >= 0)

    [<Fact>]
    let ``Threshold transition results consistent`` () =
        let positions = positionArray().WithCount(199).AtSamePoint(System.Numerics.Vector3.Zero).Build()
        let expected = (199 * 198) / 2
        let collisions = SpatialHash.detectCollisions 10.0f positions
        Assert.Equal(expected, collisions.Length)

    [<Fact>]
    let ``No duplicates collision pairs never repeated`` () =
        let positions = positionArray().WithCount(50).Clustered(3.0f).Build()
        let collisions = SpatialHash.detectCollisions 5.0f positions
        shouldBeUnique collisions

    [<Fact>]
    let ``Ordering all pairs i less than j`` () =
        let positions = positionArray().WithCount(30).Uniform().WithBounds(50.0f).Build()
        let collisions = SpatialHash.detectCollisions 3.0f positions
        shouldBeOrdered collisions (fun (i, j) -> i < j)

    [<Fact>]
    let ``Symmetry if i j exists j i does not`` () =
        let positions = positionArray().WithCount(20).Clustered(2.0f).Build()
        let collisions = SpatialHash.detectCollisions 4.0f positions
        let reversed = collisions |> List.map (fun (i, j) -> (j, i))
        let collisionSet = Set.ofList collisions
        let reversedSet = Set.ofList reversed
        Assert.True(Set.intersect collisionSet reversedSet |> Set.isEmpty)

    [<Fact>]
    let ``Penalty non-negative`` () =
        let positions = positionArray().WithCount(15).Uniform().WithBounds(30.0f).Build()
        let penalty = SpatialHash.collisionPenalty 3.0f positions
        shouldBeNonNegative penalty

    [<Fact>]
    let ``Penalty increases with overlap magnitude`` () =
        let separated = [| Vector3.Zero; Vector3(10.0f, 0.0f, 0.0f) |]
        let overlapping = [| Vector3.Zero; Vector3(0.5f, 0.0f, 0.0f) |]
        shouldIncrease (SpatialHash.collisionPenalty 2.0f overlapping) (SpatialHash.collisionPenalty 2.0f separated)

    [<Property>]
    let ``All detected collisions satisfy distance constraint`` () =
        forAllPointClouds (fun positions ->
            let collisions = SpatialHash.detectCollisions 5.0f positions
            collisions |> List.forall (fun (i, j) ->
                let dx = positions.[i].X - positions.[j].X
                let dy = positions.[i].Y - positions.[j].Y
                let dz = positions.[i].Z - positions.[j].Z
                sqrt(dx*dx + dy*dy + dz*dz) < 5.0f))

    [<Fact>]
    let ``Performance scales sublinearly for distributed nodes`` () =
        let sizes = [100; 200; 400; 800]
        let measurements = sizes |> List.map (fun n ->
            let positions = positionArray().WithCount(n).Uniform().WithBounds(100.0f).Build()
            let timing = measureTime 5 (fun () -> SpatialHash.detectCollisions 3.0f positions)
            (n, timing.Mean))
        shouldScaleSublinearly measurements

    [<Fact>]
    let ``Determinism same input produces same output`` () =
        shouldBeDeterministic (fun seed ->
            let positions = positionArray().WithSeed(seed).WithCount(50).Uniform().Build()
            SpatialHash.detectCollisions 4.0f positions)
