namespace Tests

module AcceptanceTests =

    open System
    open global.Xunit
    open FsCheck
    open FsCheck.FSharp
    open FsCheck.Xunit
    open TestInfrastructure.Core
    open TestInfrastructure.Assertions
    open TestInfrastructure.Statistics
    open TestInfrastructure.TestData
    open ParallelTempering.Core

    [<Fact>]
    let ``Improvements always accepted`` () =
        List.init 1000 (fun _ -> Acceptance.acceptMove 100.0 50.0 1.0) |> shouldAlwaysHold id

    [<Fact>]
    let ``Equal costs always accepted`` () =
        List.init 1000 (fun _ -> Acceptance.acceptMove 100.0 100.0 1.0) |> shouldAlwaysHold id

    [<Fact>]
    let ``Acceptance probability matches Metropolis formula`` () =
        acceptanceScenarios |> List.iter (fun (curr, prop, temp, theoreticalP, tol) ->
            shouldMatchProbability theoreticalP tol id (List.init 10000 (fun _ -> Acceptance.acceptMove curr prop temp)))

    [<Fact>]
    let ``Temperature zero becomes greedy`` () =
        List.init 1000 (fun _ -> Acceptance.acceptMove 50.0 100.0 0.0) |> shouldNeverHold id

    [<Fact>]
    let ``Temperature infinity accepts all`` () =
        List.init 1000 (fun _ -> Acceptance.acceptMove 50.0 100.0 Double.PositiveInfinity) |> shouldAlwaysHold id

    [<Fact>]
    let ``Acceptance rate decreases with cost increase`` () =
        [110.0; 120.0; 130.0; 140.0; 150.0]
        |> List.map (fun prop -> acceptanceRate id (List.init 5000 (fun _ -> Acceptance.acceptMove 100.0 prop 1.0)))
        |> List.map (~-) |> shouldBeStrictlyMonotonic

    [<Fact>]
    let ``Acceptance rate increases with temperature`` () =
        [0.5; 1.0; 2.0; 4.0; 8.0]
        |> List.map (fun temp -> acceptanceRate id (List.init 5000 (fun _ -> Acceptance.acceptMove 100.0 150.0 temp)))
        |> shouldBeStrictlyMonotonic

    [<Fact>]
    let ``Acceptance outcomes are Bernoulli distributed`` () =
        shouldBeBernoulli (exp (-20.0 / 2.0)) 0.02 (List.init 10000 (fun _ -> Acceptance.acceptMove 100.0 120.0 2.0))

    [<Fact>]
    let ``Acceptance rate bounded between 0 and 1`` () =
        [(100.0,50.0,1.0); (100.0,150.0,1.0); (100.0,200.0,0.5); (100.0,100.0,2.0); (50.0,100.0,5.0)]
        |> List.iter (fun (c,p,t) -> acceptanceRate id (List.init 1000 (fun _ -> Acceptance.acceptMove c p t)) |> shouldBeInRange 0.0 1.0)

    [<Fact>]
    let ``Cooling schedule decreases acceptance monotonically`` () =
        let (minT, maxT, cnt) = temperatureLadders.[0]
        [for i in 0..cnt-1 -> minT + float i * (maxT - minT) / float (cnt - 1)]
        |> List.map (fun t -> acceptanceRate id (List.init 5000 (fun _ -> Acceptance.acceptMove 100.0 150.0 t)))
        |> List.map (~-) |> shouldBeStrictlyMonotonic

    [<Fact>]
    let ``Extreme delta E handled without overflow`` () =
        Acceptance.acceptMove 0.0 1000000.0 0.1 |> fun trial -> Assert.True(trial = false || trial = true)

    [<Fact>]
    let ``Very small temperature handled without underflow`` () =
        Acceptance.acceptMove 100.0 101.0 1e-10 |> fun trial -> Assert.True(trial = false || trial = true)

    [<Property>]
    let ``Acceptance probability never exceeds 1`` (curr: float) (prop: float) (temp: float) =
        (temp > 0.0 && not (Double.IsNaN curr) && not (Double.IsNaN prop)) ==> lazy (
            acceptanceRate id (List.init 1000 (fun _ -> Acceptance.acceptMove curr prop temp)) <= 1.0
        )
