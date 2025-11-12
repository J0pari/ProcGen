namespace Tests

module ParallelTemperingTests =

    open System
    open System.Numerics
    open global.Xunit
    open FsCheck
    open FsCheck.Xunit
    open TestInfrastructure.Core
    open TestInfrastructure.Assertions
    open TestInfrastructure.Statistics
    open TestInfrastructure.Builders
    open ParallelTempering.Core

    [<Fact>]
    let ``geometricLadder creates monotonic decreasing sequence`` () =
        let ladder = ParallelTempering.geometricLadder 0.5 5.0 5

        shouldBeStrictlyMonotonic (ladder |> Array.map (~-) |> Array.toList)

    [<Fact>]
    let ``geometricLadder endpoints correct`` () =
        let tMin = 0.5
        let tMax = 5.0
        let ladder = ParallelTempering.geometricLadder tMin tMax 5

        shouldBeWithin 0.001 tMax ladder.[0]
        shouldBeWithin 0.001 tMin ladder.[4]

    [<Fact>]
    let ``geometricLadder has constant ratio`` () =
        let ladder = ParallelTempering.geometricLadder 0.5 5.0 6

        let ratios =
            ladder
            |> Array.pairwise
            |> Array.map (fun (t1, t2) -> t1 / t2)

        let avgRatio = ratios |> Array.average
        ratios |> Array.iter (fun r ->
            shouldBeWithin 0.01 avgRatio r
        )

    [<Fact>]
    let ``geometricLadder single temperature`` () =
        let ladder = ParallelTempering.geometricLadder 0.5 5.0 1

        Assert.Equal(1, ladder.Length)
        Assert.Equal(5.0, ladder.[0])

    [<Fact>]
    let ``swapProbability follows Metropolis criterion`` () =
        let graphState : ParallelTempering.Core.SpatialGraph<int> = {
            Nodes = [| 0; 1; 2 |]
            Positions = [| for i in 0..2 -> Vector3(float32 i, 0.0f, 0.0f) |]
            Edges = [(0, 1, 1.0); (1, 2, 1.0)]
        }
        let chain1 = { State = graphState; Cost = 100.0; Temperature = 1.0; AcceptanceRate = 0.5 }
        let chain2 = { State = graphState; Cost = 50.0; Temperature = 2.0; AcceptanceRate = 0.5 }

        let beta1 = 1.0 / chain1.Temperature
        let beta2 = 1.0 / chain2.Temperature
        let deltaC = chain2.Cost - chain1.Cost

        let expectedP = exp (deltaC * (beta1 - beta2))
        let actualP = ParallelTempering.swapProbability chain1 chain2

        shouldBeWithin 0.001 expectedP actualP

    [<Fact>]
    let ``swapProbability zero for invalid temperature`` () =
        let graphState : SpatialGraph<int> = { Nodes = [| 0; 1; 2 |]; Positions = [| for i in 0..2 -> Vector3(float32 i, 0.0f, 0.0f) |]; Edges = [(0, 1, 1.0); (1, 2, 1.0)] }
        let chain1 = { State = graphState; Cost = 100.0; Temperature = 0.0; AcceptanceRate = 0.5 }
        let chain2 = { State = graphState; Cost = 50.0; Temperature = 2.0; AcceptanceRate = 0.5 }

        let p = ParallelTempering.swapProbability chain1 chain2

        Assert.Equal(0.0, p)

    [<Fact>]
    let ``attemptSwap preserves temperatures`` () =
        let graphState : SpatialGraph<int> = { Nodes = [| 0; 1; 2 |]; Positions = [| for i in 0..2 -> Vector3(float32 i, 0.0f, 0.0f) |]; Edges = [(0, 1, 1.0); (1, 2, 1.0)] }
        let chain1 = { State = graphState; Cost = 100.0; Temperature = 1.0; AcceptanceRate = 0.5 }
        let chain2 = { State = graphState; Cost = 50.0; Temperature = 2.0; AcceptanceRate = 0.5 }

        let (c1, c2, _) = ParallelTempering.attemptSwap chain1 chain2

        Assert.Equal(chain1.Temperature, c1.Temperature)
        Assert.Equal(chain2.Temperature, c2.Temperature)

    [<Fact>]
    let ``attemptSwap exchanges states when accepted`` () =
        let state1 : SpatialGraph<int> = { Nodes = [| 0; 1; 2 |]; Positions = [| for i in 0..2 -> Vector3(float32 i, 0.0f, 0.0f) |]; Edges = [(0, 1, 1.0); (1, 2, 1.0)] }
        let state2 : SpatialGraph<int> = { Nodes = [| 0; 1; 2 |]; Positions = [| for i in 0..2 -> Vector3(float32 i, 0.0f, 0.0f) |]; Edges = [(0, 1, 1.0); (0, 2, 1.0); (1, 2, 1.0)] }
        let chain1 = { State = state1; Cost = 200.0; Temperature = 0.5; AcceptanceRate = 0.5 }
        let chain2 = { State = state2; Cost = 50.0; Temperature = 5.0; AcceptanceRate = 0.5 }

        let mutable swapped = false
        for _ in 1..100 do
            let (c1, c2, accepted) = ParallelTempering.attemptSwap chain1 chain2
            if accepted then
                Assert.Equal(state2.Nodes.Length, c1.State.Nodes.Length)
                Assert.Equal(state1.Nodes.Length, c2.State.Nodes.Length)
                swapped <- true

        Assert.True(swapped, "No swaps in 100 trials")

    [<Fact>]
    let ``run preserves chain count`` () =
        let temps = [| 5.0; 2.5; 1.0; 0.5 |]
        let graph : SpatialGraph<int> = { Nodes = [| 0; 1; 2; 3; 4 |]; Positions = [| for i in 0..4 -> Vector3(float32 i, 0.0f, 0.0f) |]; Edges = [(0, 1, 1.0); (1, 2, 1.0); (2, 3, 1.0); (3, 4, 1.0)] }
        let initialStates = Array.init temps.Length (fun _ -> graph)

        let simpleCost (g: SpatialGraph<int>) = float g.Edges.Length
        let simpleMutation (g: SpatialGraph<int>) (t: float) = g

        let chains = ParallelTempering.run temps initialStates simpleMutation simpleCost 100 10

        Assert.Equal(temps.Length, chains.Length)

    [<Fact>]
    let ``run chains have correct temperatures`` () =
        let temps = [| 5.0; 2.5; 1.0; 0.5 |]
        let graph : SpatialGraph<int> = { Nodes = [| 0; 1; 2; 3; 4 |]; Positions = [| for i in 0..4 -> Vector3(float32 i, 0.0f, 0.0f) |]; Edges = [(0, 1, 1.0); (1, 2, 1.0); (2, 3, 1.0); (3, 4, 1.0)] }
        let initialStates = Array.init temps.Length (fun _ -> graph)

        let simpleCost (g: SpatialGraph<int>) = float g.Edges.Length
        let simpleMutation (g: SpatialGraph<int>) (t: float) = g

        let chains = ParallelTempering.run temps initialStates simpleMutation simpleCost 100 10

        chains |> Array.iteri (fun i chain ->
            Assert.Equal(temps.[i], chain.Temperature)
        )

    [<Fact>]
    let ``runWithProgress invokes callback`` () =
        let temps = [| 2.0; 1.0 |]
        let graph : SpatialGraph<int> = { Nodes = [| 0; 1; 2 |]; Positions = [| for i in 0..2 -> Vector3(float32 i, 0.0f, 0.0f) |]; Edges = [(0, 1, 1.0); (1, 2, 1.0)] }
        let initialStates = Array.init temps.Length (fun _ -> graph)
        let simpleCost (g: SpatialGraph<int>) = float g.Edges.Length
        let simpleMutation (g: SpatialGraph<int>) (t: float) = g

        let mutable callCount = 0
        let iterations = 200
        let progressFreq = 50

        ParallelTempering.runWithProgress temps initialStates simpleMutation simpleCost iterations 10 progressFreq (fun step chains ->
            callCount <- callCount + 1
        ) |> ignore

        let expectedCalls = iterations / progressFreq
        Assert.True(callCount >= expectedCalls - 1 && callCount <= expectedCalls + 1, sprintf "Called %d times, expected ~%d" callCount expectedCalls)

    [<Fact>]
    let ``getBest returns lowest cost chain`` () =
        let graph1 : SpatialGraph<int> = { Nodes = [| 0; 1; 2 |]; Positions = [| for i in 0..2 -> Vector3(float32 i, 0.0f, 0.0f) |]; Edges = [(0, 1, 1.0); (1, 2, 1.0)] }
        let graph2 : SpatialGraph<int> = { Nodes = [| 0; 1; 2; 3 |]; Positions = [| for i in 0..3 -> Vector3(float32 i, 0.0f, 0.0f) |]; Edges = [(0, 1, 1.0); (1, 2, 1.0); (2, 3, 1.0)] }
        let graph3 : SpatialGraph<int> = { Nodes = [| 0; 1; 2; 3; 4 |]; Positions = [| for i in 0..4 -> Vector3(float32 i, 0.0f, 0.0f) |]; Edges = [(0, 1, 1.0); (1, 2, 1.0); (2, 3, 1.0); (3, 4, 1.0)] }
        let chains = [|
            { State = graph1; Cost = 100.0; Temperature = 1.0; AcceptanceRate = 0.5 }
            { State = graph2; Cost = 50.0; Temperature = 2.0; AcceptanceRate = 0.5 }
            { State = graph3; Cost = 150.0; Temperature = 3.0; AcceptanceRate = 0.5 }
        |]

        let best = ParallelTempering.getBest chains

        Assert.Equal(4, best.Nodes.Length)

    [<Fact>]
    let ``swapFrequency scales with node count`` () =
        let freq5 = ParallelTempering.swapFrequency 5
        let freq50 = ParallelTempering.swapFrequency 50
        let freq500 = ParallelTempering.swapFrequency 500

        Assert.True(freq5 < freq50, sprintf "%d >= %d" freq5 freq50)
        Assert.True(freq50 < freq500, sprintf "%d >= %d" freq50 freq500)

    [<Fact>]
    let ``swapFrequency minimum bound`` () =
        let freq = ParallelTempering.swapFrequency 1

        Assert.True(freq >= 5, sprintf "Frequency %d < 5" freq)

    [<Fact>]
    let ``updateAcceptanceRate tracks moving average`` () =
        let graphState : SpatialGraph<int> = { Nodes = [| 0; 1; 2 |]; Positions = [| for i in 0..2 -> Vector3(float32 i, 0.0f, 0.0f) |]; Edges = [(0, 1, 1.0); (1, 2, 1.0)] }
        let chain = { State = graphState; Cost = 100.0; Temperature = 1.0; AcceptanceRate = 0.5 }
        let windowSize = 100

        let updated1 = ParallelTempering.updateAcceptanceRate chain true windowSize
        let updated2 = ParallelTempering.updateAcceptanceRate updated1 false windowSize

        Assert.True(updated1.AcceptanceRate > chain.AcceptanceRate, "Acceptance rate didn't increase")
        Assert.True(updated2.AcceptanceRate < updated1.AcceptanceRate, "Acceptance rate didn't decrease")

    [<Fact>]
    let ``optimizationStep applies mutation`` () =
        let graph : SpatialGraph<int> = { Nodes = [| 0; 1; 2; 3; 4 |]; Positions = [| for i in 0..4 -> Vector3(float32 i, 0.0f, 0.0f) |]; Edges = [(0, 1, 1.0); (1, 2, 1.0); (2, 3, 1.0); (3, 4, 1.0)] }
        let chain = { State = graph; Cost = 0.0; Temperature = 1.0; AcceptanceRate = 0.5 }

        let mutable mutationCalled = false
        let mutation (g: SpatialGraph<int>) (t: float) =
            mutationCalled <- true
            g
        let cost (g: SpatialGraph<int>) = 0.0

        ParallelTempering.optimizationStep mutation cost chain |> ignore

        Assert.True(mutationCalled, "Mutation not called")

    [<Fact>]
    let ``optimizationStep evaluates cost`` () =
        let graph : SpatialGraph<int> = { Nodes = [| 0; 1; 2; 3; 4 |]; Positions = [| for i in 0..4 -> Vector3(float32 i, 0.0f, 0.0f) |]; Edges = [(0, 1, 1.0); (1, 2, 1.0); (2, 3, 1.0); (3, 4, 1.0)] }
        let chain = { State = graph; Cost = 0.0; Temperature = 1.0; AcceptanceRate = 0.5 }

        let mutable costCalled = false
        let mutation (g: SpatialGraph<int>) (t: float) = g
        let cost (g: SpatialGraph<int>) =
            costCalled <- true
            0.0

        ParallelTempering.optimizationStep mutation cost chain |> ignore

        Assert.True(costCalled, "Cost not called")

    [<Fact>]
    let ``coldest chain converges to better solution`` () =
        let temps = [| 10.0; 5.0; 2.0; 1.0; 0.5 |]
        let graph = {
            Nodes = [| 1; 2; 3; 4; 5 |]
            Positions = Array.init 5 (fun i -> Vector3(float32 i * 10.0f, 0.0f, 0.0f))
            Edges = []
        }
        let initialStates = Array.init temps.Length (fun _ -> graph)

        let cost (g: SpatialGraph<int>) =
            ParallelTempering.Core.Constraints.distribution g

        let chains = ParallelTempering.run temps initialStates Mutations.adaptive cost 1000 20

        let coldestCost = chains |> Array.minBy (fun c -> c.Temperature) |> fun c -> c.Cost
        let hottestCost = chains |> Array.maxBy (fun c -> c.Temperature) |> fun c -> c.Cost

        Assert.True(coldestCost <= hottestCost * 1.5, sprintf "Coldest %f >> Hottest %f" coldestCost hottestCost)

    [<Property>]
    let ``geometricLadder always monotonic`` (tMin: float) (tMax: float) (count: PositiveInt) =
        let min' = min (abs tMin + 0.1) (abs tMax + 0.1)
        let max' = max (abs tMin + 0.1) (abs tMax + 0.1)
        let n = min (int count) 100

        if max' > min' && n > 0 then
            let ladder = ParallelTempering.geometricLadder min' max' n
            ladder |> Array.pairwise |> Array.forall (fun (a, b) -> a >= b)
        else
            true
