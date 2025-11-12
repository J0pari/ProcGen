namespace TestInfrastructure

module TestSpecifications =

    open System
    open System.Threading
    open System.Threading.Tasks
    open FsCheck
    open FsCheck.FSharp
    open global.Xunit
    open Core
    open Generators


    type ConvergenceTest<'State> = {
        InitialState: 'State
        Step: 'State -> 'State
        CostFunction: 'State -> float
        MaxIterations: int
        ExpectedImprovement: float
        Tolerance: float
        ValidateInvariant: ('State -> bool) option
    }

    type ConvergenceResult<'State> = {
        FinalState: 'State
        InitialCost: float
        FinalCost: float
        Improvement: float
        ConvergedSuccessfully: bool
        CostHistory: float list
        IterationsRun: int
        InvariantViolations: int
    }

    let testConvergence (spec: ConvergenceTest<'State>) : ConvergenceResult<'State> =
        let mutable violations = 0

        let rec evolve state iter costs =
            if iter >= spec.MaxIterations then (state, List.rev costs, iter)
            else
                match spec.ValidateInvariant with
                | Some validate when not (validate state) -> violations <- violations + 1
                | _ -> ()

                let nextState = spec.Step state
                let cost = spec.CostFunction nextState
                evolve nextState (iter + 1) (cost :: costs)

        let initialCost = spec.CostFunction spec.InitialState
        let (finalState, costs, iterations) = evolve spec.InitialState 0 [initialCost]
        let finalCost = List.last costs
        let improvement = if initialCost > 0.0 then (initialCost - finalCost) / initialCost else 0.0

        {
            FinalState = finalState
            InitialCost = initialCost
            FinalCost = finalCost
            Improvement = improvement
            ConvergedSuccessfully = improvement >= spec.ExpectedImprovement
            CostHistory = costs
            IterationsRun = iterations
            InvariantViolations = violations
        }

    let shouldConverge (result: ConvergenceResult<'State>) : unit =
        Assert.True(result.ConvergedSuccessfully,
            sprintf "Convergence failed: %f < expected (init: %f, final: %f)" result.Improvement result.InitialCost result.FinalCost)
        Assert.Equal(0, result.InvariantViolations)

 
    type ConcurrencyTest<'Context> = {
        Setup: unit -> 'Context
        Operation: 'Context -> unit
        Threads: int
        OperationsPerThread: int
        Cleanup: 'Context -> unit
        ValidateInvariant: ('Context -> bool) option
    }

    type ConcurrencyResult = {
        ErrorCount: int
        ThreadsRun: int
        TotalOperations: int
        Success: bool
        InvariantHeld: bool
        Exceptions: exn list
    }

    let testConcurrency (spec: ConcurrencyTest<'Context>) : ConcurrencyResult =
        let context = spec.Setup()
        let errors = ref 0
        let exceptions = System.Collections.Concurrent.ConcurrentBag<exn>()

        let tasks =
            [| for _ in 1..spec.Threads ->
                async {
                    try
                        for _ in 1..spec.OperationsPerThread do
                            spec.Operation context
                    with ex ->
                        Interlocked.Increment(errors) |> ignore
                        exceptions.Add(ex)
                } |]

        Async.Parallel tasks |> Async.RunSynchronously |> ignore

        let invariantHeld =
            match spec.ValidateInvariant with
            | Some validate -> validate context
            | None -> true

        spec.Cleanup context

        {
            ErrorCount = !errors
            ThreadsRun = spec.Threads
            TotalOperations = spec.Threads * spec.OperationsPerThread
            Success = !errors = 0
            InvariantHeld = invariantHeld
            Exceptions = List.ofSeq exceptions
        }

    let shouldBeThreadSafe (result: ConcurrencyResult) : unit =
        Assert.True(result.Success, sprintf "Concurrency failed: %d errors" result.ErrorCount)
        Assert.True(result.InvariantHeld, "Invariant violated")


    type PropertyTest<'T> = {
        Generator: Gen<'T>
        Property: 'T -> bool
        Samples: int
        Shrinker: ('T -> 'T seq) option
        Verbose: bool
    }

    let testProperty (spec: PropertyTest<'T>) : unit =
        let arb =
            match spec.Shrinker with
            | Some shrink -> Arb.fromGenShrink(spec.Generator, shrink)
            | None -> Arb.fromGen spec.Generator

        let config = Config.QuickThrowOnFailure.WithMaxTest(spec.Samples).WithQuietOnSuccess(not spec.Verbose)
        Check.One(config, Prop.forAll arb spec.Property)


    type TemporalTest<'T> = {
        Trace: 'T seq
        Property: TemporalLogic.LTL<'T>
        Window: int option
    }

    let testTemporal (spec: TemporalTest<'T>) : unit =
        let trace = spec.Trace |> Seq.toList
        let traces = match spec.Window with | Some w -> trace |> List.windowed w | None -> [trace]
        let allSatisfy = traces |> List.forall (fun t -> TemporalLogic.checkLTL spec.Property t)
        Assert.True(allSatisfy, sprintf "Temporal property violated in trace of length %d" (List.length trace))


    type DistributionConstraint =
        | InRange of min: float * max: float
        | NormalDistribution of mean: float * stddev: float
        | UniformDistribution of min: float * max: float
        | BernoulliDistribution of p: float

    type MonotonicDirection = Increasing | Decreasing | NonDecreasing | NonIncreasing

    type StatisticalProperty = {
        Samples: float seq
        Distribution: DistributionConstraint option
        Monotonic: MonotonicDirection option
        ConvergesTo: float option
        Tolerance: float
    }

    let testStatisticalProperty (spec: StatisticalProperty) : unit =
        let arr = spec.Samples |> Seq.toArray

        match spec.Distribution with
        | Some (InRange (min, max)) ->
            arr |> Array.iter (fun x -> Assert.True(x >= min && x <= max, sprintf "%f outside [%f,%f]" x min max))
        | Some (NormalDistribution (mu, sigma)) ->
            if arr.Length >= 3 && arr.Length <= 5000 then
                let test = Accord.Statistics.Testing.ShapiroWilkTest(arr)
                Assert.False(test.Significant, sprintf "Normality rejected at p=%f" test.PValue)
            let sampleMean = Statistics.mean arr
            let sampleStd = Statistics.stdDev arr
            Assert.True(abs (sampleMean - mu) < spec.Tolerance, sprintf "Mean %f ≠ %f" sampleMean mu)
            Assert.True(abs (sampleStd - sigma) < spec.Tolerance, sprintf "Std %f ≠ %f" sampleStd sigma)
        | Some (UniformDistribution (min, max)) ->
            let bins = int (sqrt (float arr.Length))
            let (_, pValue, rejected) = Statistics.chiSquareUniformity bins arr
            Assert.False(rejected, sprintf "Uniformity rejected at p=%f" pValue)
        | Some (BernoulliDistribution p) ->
            let boolArr = arr |> Array.map (fun x -> x > 0.5)
            Assertions.shouldBeBernoulli p spec.Tolerance boolArr
        | None -> ()

        match spec.Monotonic with
        | Some Increasing -> arr |> Array.pairwise |> Array.iter (fun (a, b) -> Assert.True(b > a, sprintf "%f ≤ %f" a b))
        | Some Decreasing -> arr |> Array.pairwise |> Array.iter (fun (a, b) -> Assert.True(b < a, sprintf "%f ≥ %f" a b))
        | Some NonDecreasing -> arr |> Array.pairwise |> Array.iter (fun (a, b) -> Assert.True(b >= a, sprintf "%f < %f" a b))
        | Some NonIncreasing -> arr |> Array.pairwise |> Array.iter (fun (a, b) -> Assert.True(b <= a, sprintf "%f > %f" a b))
        | None -> ()

        match spec.ConvergesTo with
        | Some target ->
            let finalValue = Array.last arr
            Assert.True(abs (finalValue - target) < spec.Tolerance, sprintf "Converged to %f, expected %f±%f" finalValue target spec.Tolerance)
        | None -> ()
