namespace TestInfrastructure

module Performance =

    open System
    open System.Diagnostics
    open BenchmarkDotNet.Attributes
    open BenchmarkDotNet.Running
    open BenchmarkDotNet.Configs
    open BenchmarkDotNet.Jobs
    open BenchmarkDotNet.Environments
    open BenchmarkDotNet.Toolchains.InProcess.Emit
    open global.Xunit
    open System.Numerics

    type TimingResult = {
        Mean: float
        StdDev: float
        Min: float
        Max: float
        Median: float
        Iterations: int
    }

    let measureTime (iterations: int) (operation: unit -> 'T) : TimingResult =
        let times = Array.init iterations (fun _ ->
            let sw = Stopwatch.StartNew()
            operation() |> ignore
            sw.Stop()
            sw.Elapsed.TotalMilliseconds)

        let sorted = Array.sort times
        let mean = Array.average times
        let stddev = times |> Array.averageBy (fun t -> (t - mean) ** 2.0) |> sqrt
        let median = sorted.[sorted.Length / 2]

        {
            Mean = mean
            StdDev = stddev
            Min = Array.min times
            Max = Array.max times
            Median = median
            Iterations = iterations
        }

    let shouldCompleteWithin (maxMs: float) (operation: unit -> 'T) : unit =
        let sw = Stopwatch.StartNew()
        operation() |> ignore
        sw.Stop()
        Assert.True(sw.Elapsed.TotalMilliseconds <= maxMs,
            sprintf "Operation took %.2fms, expected ≤ %.2fms" sw.Elapsed.TotalMilliseconds maxMs)

    let shouldHaveThroughput (minOpsPerSec: float) (operation: unit -> 'T) : unit =
        let warmup = 10
        for _ in 1..warmup do operation() |> ignore

        let duration = TimeSpan.FromSeconds(1.0)
        let sw = Stopwatch.StartNew()
        let mutable ops = 0
        while sw.Elapsed < duration do
            operation() |> ignore
            ops <- ops + 1
        sw.Stop()

        let throughput = float ops / sw.Elapsed.TotalSeconds
        Assert.True(throughput >= minOpsPerSec,
            sprintf "Throughput %.0f ops/sec < %.0f ops/sec" throughput minOpsPerSec)


    type RegressionResult = {
        BaselineMean: float
        CurrentMean: float
        RelativeChange: float
        IsRegression: bool
        IsImprovement: bool
        StatisticallySignificant: bool
        PValue: float option
    }

    let comparePerformance (baseline: TimingResult) (current: TimingResult) (threshold: float) : RegressionResult =
        let relativeChange = (current.Mean - baseline.Mean) / baseline.Mean

        let n1 = float baseline.Iterations
        let n2 = float current.Iterations
        let s1Sq = baseline.StdDev ** 2.0
        let s2Sq = current.StdDev ** 2.0

        let tStatistic = (baseline.Mean - current.Mean) / sqrt (s1Sq / n1 + s2Sq / n2)
        let df = (s1Sq / n1 + s2Sq / n2) ** 2.0 / ((s1Sq / n1) ** 2.0 / (n1 - 1.0) + (s2Sq / n2) ** 2.0 / (n2 - 1.0))

        let tDist = MathNet.Numerics.Distributions.StudentT(0.0, 1.0, df)
        let pValue = 2.0 * (1.0 - tDist.CumulativeDistribution(abs tStatistic))

        {
            BaselineMean = baseline.Mean
            CurrentMean = current.Mean
            RelativeChange = relativeChange
            IsRegression = relativeChange > threshold && pValue < 0.05
            IsImprovement = relativeChange < -threshold && pValue < 0.05
            StatisticallySignificant = pValue < 0.05
            PValue = Some pValue
        }

    let shouldNotRegress (baseline: TimingResult) (current: TimingResult) (maxSlowdown: float) : unit =
        let result = comparePerformance baseline current maxSlowdown
        Assert.False(result.IsRegression,
            sprintf "Performance regression detected: %.1f%% slower (p=%.4f)"
                (result.RelativeChange * 100.0) (result.PValue |> Option.defaultValue 1.0))


    let detectChangepoints (series: float[]) (threshold: float) : int list =
        let n = series.Length
        let mean = Array.average series
        let stddev = series |> Array.averageBy (fun x -> (x - mean) ** 2.0) |> sqrt

        let mutable cumSum = 0.0
        let mutable changepoints = []

        for i in 0..n-1 do
            cumSum <- cumSum + (series.[i] - mean) / stddev
            if abs cumSum > threshold then
                changepoints <- i :: changepoints
                cumSum <- 0.0

        List.rev changepoints

    let binarySegmentation (series: float[]) (minSegmentSize: int) (significance: float) : int list =
        let n = series.Length

        let computeVariance (data: float[]) (start: int) (end': int) =
            let segment = data.[start..end']
            let mean = Array.average segment
            segment |> Array.averageBy (fun x -> (x - mean) ** 2.0)

        let findBestSplit (start: int) (end': int) =
            let mutable bestSplit = -1
            let mutable bestImprovement = 0.0

            let fullVar = computeVariance series start end'

            for split in start+minSegmentSize..end'-minSegmentSize do
                let leftVar = computeVariance series start split
                let rightVar = computeVariance series (split+1) end'
                let leftWeight = float (split - start + 1) / float (end' - start + 1)
                let rightWeight = float (end' - split) / float (end' - start + 1)
                let combinedVar = leftWeight * leftVar + rightWeight * rightVar
                let improvement = fullVar - combinedVar

                if improvement > bestImprovement then
                    bestImprovement <- improvement
                    bestSplit <- split

            (bestSplit, bestImprovement)

        let rec segment (start: int) (end': int) =
            if end' - start < 2 * minSegmentSize then []
            else
                let (split, improvement) = findBestSplit start end'
                if split = -1 || improvement < significance then []
                else
                    split :: (segment start split @ segment (split+1) end')

        segment 0 (n-1) |> List.sort


    type Complexity =
        | Constant      // O(1)
        | Logarithmic   // O(log n)
        | Linear        // O(n)
        | Linearithmic  // O(n log n)
        | Quadratic     // O(n²)
        | Cubic         // O(n³)
        | Exponential   // O(2^n)

    let detectComplexity (measurements: (int * float) list) : Complexity * float =
        let data = measurements |> List.map (fun (n, t) -> (float n, t)) |> List.toArray

        let models = [
            (Constant, fun n -> 1.0)
            (Logarithmic, fun n -> log n)
            (Linear, fun n -> n)
            (Linearithmic, fun n -> n * log n)
            (Quadratic, fun n -> n * n)
            (Cubic, fun n -> n * n * n)
        ]

        let fitModel (modelFn: float -> float) =
            let X = data |> Array.map (fun (n, _) -> modelFn n)
            let Y = data |> Array.map snd
            let meanX = Array.average X
            let meanY = Array.average Y

            let b =
                let num = Array.map2 (fun x y -> (x - meanX) * (y - meanY)) X Y |> Array.sum
                let denom = X |> Array.sumBy (fun x -> (x - meanX) ** 2.0)
                if denom > 0.0 then num / denom else 0.0

            let a = meanY - b * meanX

            let yPred = X |> Array.map (fun x -> a + b * x)
            let ssRes = Array.map2 (fun y yp -> (y - yp) ** 2.0) Y yPred |> Array.sum
            let ssTot = Y |> Array.sumBy (fun y -> (y - meanY) ** 2.0)
            let rSquared = if ssTot > 0.0 then 1.0 - ssRes / ssTot else 0.0

            rSquared

        let results = models |> List.map (fun (complexity, modelFn) -> (complexity, fitModel modelFn))
        let (bestComplexity, bestR2) = results |> List.maxBy snd

        (bestComplexity, bestR2)

    let shouldHaveComplexity (expected: Complexity) (operation: int -> unit) (sizes: int list) : unit =
        let measurements =
            sizes |> List.map (fun n ->
                let result = measureTime 10 (fun () -> operation n)
                (n, result.Mean))

        let (detected, r2) = detectComplexity measurements

        Assert.True(detected = expected || r2 < 0.7,
            sprintf "Expected %A complexity, detected %A (R²=%.3f)" expected detected r2)


    let findCrossover (algo1: int -> float) (algo2: int -> float) (minSize: int) (maxSize: int) (tolerance: float) : int option =
        let rec binarySearch low high =
            if high - low <= 1 then
                let t1Low = algo1 low
                let t2Low = algo2 low
                let t1High = algo1 high
                let t2High = algo2 high

                if (t1Low < t2Low && t1High > t2High) || (t1Low > t2Low && t1High < t2High) then
                    Some low
                else
                    None
            else
                let mid = (low + high) / 2
                let t1Mid = algo1 mid
                let t2Mid = algo2 mid
                let t1Low = algo1 low
                let t2Low = algo2 low

                let diffLow = t1Low - t2Low
                let diffMid = t1Mid - t2Mid

                if abs diffMid < tolerance then Some mid
                elif diffLow * diffMid < 0.0 then
                    binarySearch low mid
                else
                    binarySearch mid high

        binarySearch minSize maxSize

    let measureCrossover (fastSmall: int -> unit) (fastLarge: int -> unit) (minSize: int) (maxSize: int) : int option =
        let timingFastSmall n =
            let result = measureTime 5 (fun () -> fastSmall n)
            result.Mean

        let timingFastLarge n =
            let result = measureTime 5 (fun () -> fastLarge n)
            result.Mean

        findCrossover timingFastSmall timingFastLarge minSize maxSize 0.01


    type ScalabilityResult = {
        ThreadCounts: int list
        Throughputs: float list
        SpeedupFactors: float list
        Efficiency: float list
        ScalabilityScore: float
    }

    let measureScalability (operation: int -> unit) (threadCounts: int list) (workload: int) : ScalabilityResult =
        let baselineThroughput =
            let sw = Stopwatch.StartNew()
            operation 1
            sw.Stop()
            1.0 / sw.Elapsed.TotalSeconds

        let throughputs =
            threadCounts |> List.map (fun threads ->
                let sw = Stopwatch.StartNew()
                operation threads
                sw.Stop()
                1.0 / sw.Elapsed.TotalSeconds)

        let speedups = throughputs |> List.map (fun t -> t / baselineThroughput)
        let efficiency = List.map2 (fun speedup threads -> speedup / float threads) speedups threadCounts

        let avgEfficiency = List.average efficiency
        let scalabilityScore = avgEfficiency * 100.0

        {
            ThreadCounts = threadCounts
            Throughputs = throughputs
            SpeedupFactors = speedups
            Efficiency = efficiency
            ScalabilityScore = scalabilityScore
        }

    let shouldScaleWell (result: ScalabilityResult) : unit =
        Assert.True(result.ScalabilityScore >= 70.0,
            sprintf "Poor scalability: %.1f%% efficiency" result.ScalabilityScore)


    type MemoryResult = {
        AllocatedBytes: int64
        Gen0Collections: int
        Gen1Collections: int
        Gen2Collections: int
    }

    let measureMemory (operation: unit -> 'T) : MemoryResult * 'T =
        GC.Collect()
        GC.WaitForPendingFinalizers()
        GC.Collect()

        let gen0Before = GC.CollectionCount(0)
        let gen1Before = GC.CollectionCount(1)
        let gen2Before = GC.CollectionCount(2)
        let memBefore = GC.GetTotalMemory(false)

        let result = operation()

        let memAfter = GC.GetTotalMemory(false)
        let gen0After = GC.CollectionCount(0)
        let gen1After = GC.CollectionCount(1)
        let gen2After = GC.CollectionCount(2)

        let memory = {
            AllocatedBytes = memAfter - memBefore
            Gen0Collections = gen0After - gen0Before
            Gen1Collections = gen1After - gen1Before
            Gen2Collections = gen2After - gen2Before
        }

        (memory, result)

    let shouldAllocateLessThan (maxBytes: int64) (operation: unit -> 'T) : unit =
        let (memory, _) = measureMemory operation
        Assert.True(memory.AllocatedBytes <= maxBytes,
            sprintf "Allocated %d bytes, expected ≤ %d bytes" memory.AllocatedBytes maxBytes)

    let shouldNotTriggerGen2 (operation: unit -> 'T) : unit =
        let (memory, _) = measureMemory operation
        Assert.Equal(0, memory.Gen2Collections)


    let measureCacheEfficiency (sequential: unit -> 'T) (random: unit -> 'T) : float =
        let seqResult = measureTime 100 sequential
        let randResult = measureTime 100 random

        randResult.Mean / seqResult.Mean

    let shouldBeCacheFriendly (operation: unit -> 'T) (baseline: unit -> 'T) : unit =
        let opResult = measureTime 50 operation
        let baseResult = measureTime 50 baseline

        let ratio = opResult.Mean / baseResult.Mean
        Assert.True(ratio <= 2.0,
            sprintf "Operation %.1fx slower than baseline (poor cache locality)" ratio)
