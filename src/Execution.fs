namespace Execution

open System
open System.Threading
open System.Threading.Tasks
open System.Collections.Concurrent

/// Work unit with resource tracking
type Work<'T> = {
    Compute: unit -> 'T
    Resources: IDisposable list
}

/// Execution context for partitioned work
type Context<'T> = {
    Items: 'T array
    Partition: int
    TotalPartitions: int
}

/// Resource-tracked result
type Tracked<'T> = {
    Value: 'T
    Cleanup: unit -> unit
}

/// Parallel execution with automatic cleanup
module Parallel =

    /// Map over array with work-stealing parallelism
    let map (f: 'T -> 'U) (items: 'T array) : 'U array =
        let results = Array.zeroCreate items.Length
        let options = ParallelOptions(MaxDegreeOfParallelism = Environment.ProcessorCount)

        System.Threading.Tasks.Parallel.For(0, items.Length, options, fun i ->
            results.[i] <- f items.[i]
        ) |> ignore

        results

    /// Map with indexed access
    let mapi (f: int -> 'T -> 'U) (items: 'T array) : 'U array =
        let results = Array.zeroCreate items.Length
        let options = ParallelOptions(MaxDegreeOfParallelism = Environment.ProcessorCount)

        System.Threading.Tasks.Parallel.For(0, items.Length, options, fun i ->
            results.[i] <- f i items.[i]
        ) |> ignore

        results

    /// Execute with partition context
    let withContext (f: Context<'T> -> 'U) (items: 'T array) : 'U array =
        let partitionCount = min items.Length Environment.ProcessorCount
        let partitionSize = (items.Length + partitionCount - 1) / partitionCount

        let partitions =
            [| 0 .. partitionCount - 1 |]
            |> Array.map (fun p ->
                let start = p * partitionSize
                let count = min partitionSize (items.Length - start)
                if count > 0 then
                    Some {
                        Items = items.[start .. start + count - 1]
                        Partition = p
                        TotalPartitions = partitionCount
                    }
                else None
            )
            |> Array.choose id

        map f partitions

/// Composable async operations with resource tracking
module Pipeline =

    /// Lift value into tracked context
    let wrap (value: 'T) : Tracked<'T> =
        { Value = value; Cleanup = fun () -> () }

    /// Transform tracked value
    let map (f: 'T -> 'U) (tracked: Tracked<'T>) : Tracked<'U> =
        { Value = f tracked.Value; Cleanup = tracked.Cleanup }

    /// Chain operations
    let bind (f: 'T -> Tracked<'U>) (tracked: Tracked<'T>) : Tracked<'U> =
        let next = f tracked.Value
        {
            Value = next.Value
            Cleanup = fun () ->
                try next.Cleanup()
                finally tracked.Cleanup()
        }

    /// Execute and ensure cleanup
    let run (tracked: Tracked<'T>) : 'T =
        try tracked.Value
        finally tracked.Cleanup()

    /// Track disposable resource
    let track (resource: #IDisposable) (value: 'T) : Tracked<'T> =
        {
            Value = value
            Cleanup = fun () -> resource.Dispose()
        }

/// Async work pool with lifecycle management
type WorkPool(workerCount: int) =
    let queue = new BlockingCollection<unit -> unit>(1000)
    let workers = ResizeArray<Task>()
    let cts = new CancellationTokenSource()

    let workerLoop () =
        task {
            while not cts.Token.IsCancellationRequested do
                try
                    let work = queue.Take(cts.Token)
                    work()
                with
                | :? OperationCanceledException -> ()
                | ex -> printfn "Worker error: %s" ex.Message
        }

    do
        for _ in 1 .. workerCount do
            workers.Add(workerLoop())

    member _.Enqueue(work: unit -> unit) : bool =
        try
            queue.Add(work, cts.Token)
            true
        with
        | :? OperationCanceledException -> false
        | :? InvalidOperationException -> false

    member _.EnqueueAsync(work: unit -> 'T) : Task<'T> =
        let tcs = TaskCompletionSource<'T>()
        let wrapped () =
            try
                let result = work()
                tcs.SetResult(result)
            with ex ->
                tcs.SetException(ex)

        if queue.TryAdd(wrapped, 0, cts.Token) then
            tcs.Task
        else
            Task.FromException<'T>(InvalidOperationException("Queue full"))

    interface IDisposable with
        member _.Dispose() =
            cts.Cancel()
            queue.CompleteAdding()
            Task.WaitAll(workers.ToArray(), TimeSpan.FromSeconds(5.0)) |> ignore
            cts.Dispose()
            queue.Dispose()

/// Scoped resource management
module Scope =

    /// Execute with automatic resource cleanup
    let using (create: unit -> #IDisposable) (f: #IDisposable -> 'T) : 'T =
        let resource = create()
        try f resource
        finally resource.Dispose()

    /// Execute async with cleanup
    let usingAsync (create: unit -> #IDisposable) (f: #IDisposable -> Task<'T>) : Task<'T> =
        task {
            let resource = create()
            try
                return! f resource
            finally
                resource.Dispose()
        }

    /// Batch resource cleanup
    let collect (resources: IDisposable list) : IDisposable =
        { new IDisposable with
            member _.Dispose() =
                resources |> List.iter (fun r ->
                    try r.Dispose()
                    with _ -> ()
                )
        }

/// Graph algorithms
module Graph =

    /// BFS to count reachable nodes from start node
    /// Returns number of nodes reachable via edges
    /// Time complexity: O(V + E)
    let countReachableFrom (nodeCount: int) (edges: (int * int * 'T) list) (startNode: int) : int =
        if nodeCount <= 0 || startNode < 0 || startNode >= nodeCount then 0
        else
            let visited = Array.create nodeCount false
            let queue = System.Collections.Generic.Queue<int>()
            queue.Enqueue(startNode)
            visited.[startNode] <- true

            while queue.Count > 0 do
                let node = queue.Dequeue()
                edges
                |> List.iter (fun (a, b, _) ->
                    let neighbor = if a = node then b elif b = node then -1 else -1
                    if neighbor >= 0 && not visited.[neighbor] then
                        visited.[neighbor] <- true
                        queue.Enqueue(neighbor)
                )

            visited |> Array.filter id |> Array.length
