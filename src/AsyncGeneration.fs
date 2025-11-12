namespace AsyncGeneration

open System
open System.Threading
open System.Threading.Tasks
open ParallelTempering.Core
open ProceduralGeneration

/// Progress information during generation
type ProgressInfo = {
    Step: int
    TotalSteps: int
    BestCost: float
    Temperature: float
    AcceptanceRate: float
    ElapsedMs: int64
}

/// Async generation with streaming progress
module AsyncGenerator =
    
    /// Generate with async progress callbacks
    let generateWithProgress
        (config: Generator.Config)
        (progressCallback: ProgressInfo -> Task)
        (cancellationToken: CancellationToken) : Task<SpatialGraph<NodeType>> =

        task {
            let startTime = System.Diagnostics.Stopwatch.StartNew()

            let initialStates =
                Array.init config.Temperatures.Length (fun _ ->
                    Initialization.createGraph config.NodeCount
                )

            let cost = EnvironmentConstraints.build config.ConstraintWeights
            let mutate = Mutations.adaptive

            let mutable currentChains =
                Array.map2 (fun temp state ->
                    { State = state; Cost = cost state; Temperature = temp; AcceptanceRate = 0.5 }
                ) config.Temperatures initialStates

            for step in 0 .. config.Iterations - 1 do
                cancellationToken.ThrowIfCancellationRequested()

                currentChains <- Execution.Parallel.map (ParallelTempering.optimizationStep mutate cost) currentChains

                if step % config.SwapFrequency = 0 then
                    for i in 0 .. currentChains.Length - 2 do
                        let (c1, c2, _) = ParallelTempering.attemptSwap currentChains.[i] currentChains.[i+1]
                        currentChains.[i] <- c1
                        currentChains.[i+1] <- c2

                if step % 10 = 0 then
                    let best = currentChains |> Array.minBy (fun c -> c.Cost)
                    let progress = {
                        Step = step
                        TotalSteps = config.Iterations
                        BestCost = best.Cost
                        Temperature = best.Temperature
                        AcceptanceRate = best.AcceptanceRate
                        ElapsedMs = startTime.ElapsedMilliseconds
                    }
                    do! progressCallback progress

            return ParallelTempering.getBest currentChains
        }
    
    /// Generate with timeout
    let generateWithTimeout
        (config: Generator.Config)
        (timeoutMs: int) : Task<Result<SpatialGraph<NodeType>, string>> =
        
        task {
            use cts = new CancellationTokenSource(timeoutMs)
            
            try
                let! result = generateWithProgress config (fun _ -> Task.CompletedTask) cts.Token
                return Ok result
            with
            | :? OperationCanceledException ->
                return Error "Generation timeout exceeded"
            | ex ->
                return Error ex.Message
        }
    
    /// Parallel generation of multiple graphs
    let generateBatch
        (config: Generator.Config)
        (count: int)
        (maxParallel: int) : Task<SpatialGraph<NodeType> array> =

        Execution.Scope.usingAsync
            (fun () -> new SemaphoreSlim(maxParallel))
            (fun semaphore ->
                let generateOne () =
                    task {
                        do! semaphore.WaitAsync()
                        try
                            return Generator.generate config
                        finally
                            semaphore.Release() |> ignore
                    }

                Task.WhenAll(Array.init count (fun _ -> generateOne()))
            )
