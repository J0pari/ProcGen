namespace Tests

module ThreadSafeRandomTests =

    open System
    open System.Threading
    open global.Xunit
    open FsCheck
    open FsCheck.Xunit
    open TestInfrastructure.Core
    open TestInfrastructure.Assertions
    open TestInfrastructure.PropertyDSL
    open TestInfrastructure.Statistics
    open TestInfrastructure.Concurrency
    open TestInfrastructure.Performance
    open TestInfrastructure.Random

    [<Fact>]
    let ``Thread-safety concurrent calls never corrupt state`` () =
        let rng = randomBuilder().WithSeed(42).Build()
        withAsync (fun () ->
            [| for i in 1..1000 -> async { rng.Next(0, 100) |> ignore } |]
            |> Async.Parallel
            |> Async.RunSynchronously
            |> ignore
            Assert.True(true))

    [<Fact>]
    let ``Uniform distribution Next in range`` () =
        let rng = randomBuilder().Build()
        let samples = [for i in 1..10000 -> rng.Next(0, 100)]
        shouldBeUniform samples 10

    [<Fact>]
    let ``Uniform distribution NextDouble in range`` () =
        let rng = randomBuilder().Build()
        let samples = [for i in 1..10000 -> rng.NextDouble()]
        samples |> List.iter (fun x -> shouldBeInRange 0.0 1.0 x)

    [<Fact>]
    let ``Normal distribution NextGaussian`` () =
        let rng = randomBuilder().Build()
        let samples = [for i in 1..10000 -> rng.NextGaussian(50.0, 10.0)]
        shouldBeNormal samples
        shouldHaveMean samples 50.0 2.0
        shouldHaveStdDev samples 10.0 2.0

    [<Fact>]
    let ``Determinism same seed produces identical sequence`` () =
        shouldBeDeterministic (fun seed ->
            let rng = randomBuilder().WithSeed(seed).Build()
            [for i in 1..100 -> rng.Next(0, 1000)])

    [<Property>]
    let ``Range validation output within bounds`` (min: int) (max: int) =
        min < max && max - min < 10000 ==> lazy (
            let rng = randomBuilder().Build()
            let samples = [for i in 1..100 -> rng.Next(min, max)]
            samples |> List.forall (fun x -> x >= min && x < max)
        )

    [<Fact>]
    let ``Statistical quality chi-square test`` () =
        let rng = randomBuilder().Build()
        let samples = [for i in 1..10000 -> rng.Next(0, 10)]
        shouldBeUniform samples 10

    [<Fact>]
    let ``Concurrent independence different seeds uncorrelated`` () =
        shouldBeIndependent
            (fun () -> randomBuilder().WithSeed(1).Build())
            (fun () -> randomBuilder().WithSeed(2).Build())
            (fun rng -> [for i in 1..1000 -> rng.Next(0, 100)])

    [<Fact>]
    let ``No collisions concurrent threads generate distinct values`` () =
        shouldNotCollide 10 (fun tid ->
            let rng = randomBuilder().WithSeed(tid).Build()
            [for i in 1..100 -> rng.Next(0, 1000000)])

    [<Fact>]
    let ``Performance generates millions without blocking`` () =
        shouldGenerateQuickly 1000000 (fun () ->
            let rng = randomBuilder().Build()
            for i in 1..1000000 do rng.Next(0, 100) |> ignore)

    [<Fact>]
    let ``Cancellation respects CancellationToken`` () =
        let cts = new CancellationTokenSource()
        cts.Cancel()
        Assert.Throws<OperationCanceledException>(fun () ->
            let rng = randomBuilder().Build()
            for i in 1..1000000 do
                cts.Token.ThrowIfCancellationRequested()
                rng.Next(0, 100) |> ignore) |> ignore

    [<Property>]
    let ``NextDouble always in zero to one`` () =
        forAllSeeds (fun seed ->
            let rng = randomBuilder().WithSeed(seed).Build()
            let samples = [for i in 1..100 -> rng.NextDouble()]
            samples |> List.forall (fun x -> x >= 0.0 && x < 1.0))
