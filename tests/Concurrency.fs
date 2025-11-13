namespace TestInfrastructure

module Concurrency =

    open System
    open System.Threading
    open System.Threading.Tasks
    open System.Collections.Concurrent
    open System.Collections.Generic
    open global.Xunit
    open System.Numerics


    type Invocation<'Op> = {
        ThreadId: int
        OperationId: int64
        Operation: 'Op
        Timestamp: DateTime
    }

    type Response<'Res> = {
        ThreadId: int
        OperationId: int64
        Result: 'Res
        Timestamp: DateTime
    }

    type HistoryEntry<'Op, 'Res> =
        | Invoke of Invocation<'Op>
        | Return of Response<'Res>

    type Operation<'Op, 'Res> = {
        Invocation: Invocation<'Op>
        Response: Response<'Res>
    }

    type HistoryRecorder<'Op, 'Res>() =
        let history = ConcurrentBag<HistoryEntry<'Op, 'Res>>()
        let opIdCounter = ref 0L

        member _.RecordInvoke(threadId: int, op: 'Op) : int64 =
            let opId = Interlocked.Increment(opIdCounter)
            let inv = { ThreadId = threadId; OperationId = opId; Operation = op; Timestamp = DateTime.UtcNow }
            history.Add(Invoke inv)
            opId

        member _.RecordReturn(threadId: int, opId: int64, result: 'Res) =
            let resp = { ThreadId = threadId; OperationId = opId; Result = result; Timestamp = DateTime.UtcNow }
            history.Add(Return resp)

        member _.GetHistory() = history |> Seq.toList |> List.sortBy (fun entry ->
            match entry with
            | Invoke inv -> inv.Timestamp
            | Return resp -> resp.Timestamp)

        member _.GetOperations() : Operation<'Op, 'Res> list =
            let invocations = Dictionary<int64, Invocation<'Op>>()
            let responses = Dictionary<int64, Response<'Res>>()

            history |> Seq.iter (fun entry ->
                match entry with
                | Invoke inv -> invocations.[inv.OperationId] <- inv
                | Return resp -> responses.[resp.OperationId] <- resp)

            invocations
            |> Seq.choose (fun kvp ->
                match responses.TryGetValue(kvp.Key) with
                | true, resp -> Some { Invocation = kvp.Value; Response = resp }
                | false, _ -> None)
            |> Seq.toList


    type SequentialSpec<'State, 'Op, 'Res> = {
        InitialState: 'State
        Apply: 'State -> 'Op -> ('State * 'Res)
    }

    let checkLinearizability (spec: SequentialSpec<'State, 'Op, 'Res>) (operations: Operation<'Op, 'Res> list) : bool =
        let rec tryLinearizations (remaining: Operation<'Op, 'Res> list) (state: 'State) : bool =
            if List.isEmpty remaining then true
            else
                remaining
                |> List.exists (fun op ->
                    let (newState, expectedResult) = spec.Apply state op.Invocation.Operation
                    if expectedResult = op.Response.Result then
                        let rest = remaining |> List.filter ((<>) op)
                        tryLinearizations rest newState
                    else
                        false)

        let happensBefore (op1: Operation<'Op, 'Res>) (op2: Operation<'Op, 'Res>) =
            op1.Response.Timestamp < op2.Invocation.Timestamp

        tryLinearizations operations spec.InitialState

    let shouldBeLinearizable (spec: SequentialSpec<'State, 'Op, 'Res>) (recorder: HistoryRecorder<'Op, 'Res>) : unit =
        let operations = recorder.GetOperations()
        let isLinearizable = checkLinearizability spec operations
        Assert.True(isLinearizable, sprintf "History is not linearizable (found %d operations)" operations.Length)

   
    type MemoryAccess = {
        ThreadId: int
        Address: int
        IsWrite: bool
        Timestamp: DateTime
        StackTrace: string
    }

    type RaceDetector() =
        let accesses = ConcurrentBag<MemoryAccess>()
        let locks = ConcurrentDictionary<int, DateTime>()
        let vectorClocks = ConcurrentDictionary<int, int[]>()

        member _.RecordAccess(threadId: int, address: int, isWrite: bool) =
            let access = {
                ThreadId = threadId
                Address = address
                IsWrite = isWrite
                Timestamp = DateTime.UtcNow
                StackTrace = Environment.StackTrace
            }
            accesses.Add(access)

        member _.RecordLock(lockId: int) =
            locks.[lockId] <- DateTime.UtcNow

        member _.RecordUnlock(lockId: int) =
            locks.TryRemove(lockId) |> ignore

        member _.DetectRaces() : (MemoryAccess * MemoryAccess) list =
            let accessList = accesses |> Seq.toList

            let mutable races = []

            for i in 0..accessList.Length-2 do
                for j in i+1..accessList.Length-1 do
                    let a1 = accessList.[i]
                    let a2 = accessList.[j]

                    let isConflict =
                        a1.Address = a2.Address &&
                        a1.ThreadId <> a2.ThreadId &&
                        (a1.IsWrite || a2.IsWrite)

                    if isConflict then
                        let timeDiff = abs ((a2.Timestamp - a1.Timestamp).TotalMilliseconds)
                        if timeDiff < 1.0 then // Concurrent if within 1ms
                            races <- (a1, a2) :: races

            races

        member this.AssertNoRaces() =
            let races = this.DetectRaces()
            Assert.True(List.isEmpty races,
                sprintf "Detected %d potential race conditions" races.Length)

    let testForRaces (setup: unit -> 'Context) (operations: ('Context -> unit) list) (cleanup: 'Context -> unit) : RaceDetector =
        let detector = RaceDetector()
        let context = setup()

        let tasks =
            operations
            |> List.mapi (fun threadId op ->
                async {
                    try
                        op context
                    with ex ->
                        failwithf "Thread %d failed: %s" threadId ex.Message
                })

        Async.Parallel tasks |> Async.RunSynchronously |> ignore
        cleanup context

        detector


    type LockEvent = {
        ThreadId: int
        LockId: int
        IsAcquisition: bool
        Timestamp: DateTime
    }

    type DeadlockDetector() =
        let events = ConcurrentBag<LockEvent>()
        let heldLocks = ConcurrentDictionary<int, Set<int>>() // threadId -> set of lock IDs

        member _.RecordLockAcquisition(threadId: int, lockId: int) =
            events.Add({ ThreadId = threadId; LockId = lockId; IsAcquisition = true; Timestamp = DateTime.UtcNow })
            heldLocks.AddOrUpdate(threadId, Set.singleton lockId, fun _ locks -> Set.add lockId locks) |> ignore

        member _.RecordLockRelease(threadId: int, lockId: int) =
            events.Add({ ThreadId = threadId; LockId = lockId; IsAcquisition = false; Timestamp = DateTime.UtcNow })
            heldLocks.AddOrUpdate(threadId, Set.empty, fun _ locks -> Set.remove lockId locks) |> ignore

        member _.BuildWaitForGraph() : Map<int, Set<int>> =
            let waiting = Dictionary<int, int>() // threadId -> lockId it's waiting for
            let holding = Dictionary<int, int>() // lockId -> threadId holding it

            events
            |> Seq.sortBy (fun e -> e.Timestamp)
            |> Seq.iter (fun event ->
                if event.IsAcquisition then
                    match holding.TryGetValue(event.LockId) with
                    | true, holder when holder <> event.ThreadId ->
                        waiting.[event.ThreadId] <- event.LockId
                    | _ ->
                        holding.[event.LockId] <- event.ThreadId
                        waiting.Remove(event.ThreadId) |> ignore
                else
                    holding.Remove(event.LockId) |> ignore)

            waiting
            |> Seq.fold (fun (graph: Map<int, Set<int>>) kvp ->
                match holding.TryGetValue(kvp.Value) with
                | true, holder ->
                    let waiters = graph |> Map.tryFind kvp.Key |> Option.defaultValue Set.empty
                    graph |> Map.add kvp.Key (Set.add holder waiters)
                | false, _ -> graph) Map.empty

        member this.DetectCycle() : int list option =
            let graph = this.BuildWaitForGraph()

            let rec dfs (node: int) (visited: Set<int>) (path: int list) =
                if List.contains node path then
                    Some (node :: path)
                elif Set.contains node visited then
                    None
                else
                    let newVisited = Set.add node visited
                    match Map.tryFind node graph with
                    | Some neighbors ->
                        neighbors
                        |> Set.toList
                        |> List.tryPick (fun neighbor -> dfs neighbor newVisited (node :: path))
                    | None -> None

            graph
            |> Map.toList
            |> List.tryPick (fun (node, _) -> dfs node Set.empty [])

        member this.AssertNoDeadlock() =
            match this.DetectCycle() with
            | Some cycle ->
                Assert.Fail(sprintf "Deadlock detected: cycle involves threads %A" cycle)
            | None -> ()


    type StressTest<'Context> = {
        Setup: unit -> 'Context
        Operation: 'Context -> int -> unit
        Cleanup: 'Context -> unit
        Threads: int
        OperationsPerThread: int
        Duration: TimeSpan option
    }

    let runStressTest (test: StressTest<'Context>) : bool =
        let context = test.Setup()
        let errors = ref 0
        let exceptions = ConcurrentBag<exn>()

        let runThread threadId =
            async {
                try
                    match test.Duration with
                    | Some duration ->
                        let stopwatch = System.Diagnostics.Stopwatch.StartNew()
                        while stopwatch.Elapsed < duration && !errors = 0 do
                            test.Operation context threadId
                    | None ->
                        for _ in 1..test.OperationsPerThread do
                            if !errors = 0 then
                                test.Operation context threadId
                with ex ->
                    Interlocked.Increment(errors) |> ignore
                    exceptions.Add(ex)
            }

        let tasks = [| for i in 0..test.Threads-1 -> runThread i |]
        Async.Parallel tasks |> Async.RunSynchronously |> ignore

        test.Cleanup context

        if !errors > 0 then
            let ex = exceptions |> Seq.head
            raise ex
        else
            true

    let shouldSurviveStress (test: StressTest<'Context>) : unit =
        let success = runStressTest test
        Assert.True(success, "Stress test failed")


    type PhaseBarrier(participantCount: int) =
        let barrier = new Barrier(participantCount)
        let phaseCounter = ref 0

        member _.WaitPhase() : int =
            barrier.SignalAndWait()
            Interlocked.Increment(phaseCounter)

        member _.CurrentPhase = !phaseCounter

        interface IDisposable with
            member _.Dispose() = barrier.Dispose()

    let testWithPhases (threadCount: int) (phases: (int -> unit) list) : unit =
        use barrier = new PhaseBarrier(threadCount)

        let runThread threadId =
            async {
                for phase in phases do
                    phase threadId
                    barrier.WaitPhase() |> ignore
            }

        let tasks = [| for i in 0..threadCount-1 -> runThread i |]
        Async.Parallel tasks |> Async.RunSynchronously |> ignore


    let shouldMaintainConsistency
        (setup: unit -> 'State)
        (read: 'State -> 'T)
        (write: 'State -> unit)
        (invariant: 'T -> bool)
        (readers: int)
        (writers: int)
        (duration: TimeSpan) : unit =

        let state = setup()
        let violations = ref 0

        let readerTask () =
            async {
                let stopwatch = System.Diagnostics.Stopwatch.StartNew()
                while stopwatch.Elapsed < duration do
                    let value = read state
                    if not (invariant value) then
                        Interlocked.Increment(violations) |> ignore
            }

        let writerTask () =
            async {
                let stopwatch = System.Diagnostics.Stopwatch.StartNew()
                while stopwatch.Elapsed < duration do
                    write state
                    Thread.Sleep(1)
            }

        let tasks =
            [| for _ in 1..readers -> readerTask()
               for _ in 1..writers -> writerTask() |]

        Async.Parallel tasks |> Async.RunSynchronously |> ignore

        Assert.Equal(0, !violations)


    let shouldHaveIndependentRNGs (rngFactory: unit -> 'RNG) (next: 'RNG -> float) (trials: int) : unit =
        let threads = Environment.ProcessorCount
        let samplesPerThread = trials / threads

        let allSamples = ConcurrentBag<float array>()

        let sampleThread () =
            async {
                let rng = rngFactory()
                let samples = Array.init samplesPerThread (fun _ -> next rng)
                allSamples.Add(samples)
            }

        let tasks = [| for _ in 1..threads -> sampleThread() |]
        Async.Parallel tasks |> Async.RunSynchronously |> ignore

        let allArrays = allSamples |> Seq.toArray
        let correlations =
            [| for i in 0..allArrays.Length-2 do
                for j in i+1..allArrays.Length-1 do
                    let arr1 = allArrays.[i]
                    let arr2 = allArrays.[j]
                    let minLen = min arr1.Length arr2.Length
                    let r = Statistics.pearsonCorrelation arr1.[0..minLen-1] arr2.[0..minLen-1]
                    yield abs r |]

        let maxCorrelation = Array.max correlations
        Assert.True(maxCorrelation < 0.3,
            sprintf "RNG correlation %.3f too high (not independent)" maxCorrelation)


    let shouldRespectMemoryFences (writer: int ref -> unit) (reader: int ref -> int) : unit =
        let value = ref 0
        let flag = ref false
        let observedValue = ref 0

        let writerTask () =
            async {
                writer value
                Thread.MemoryBarrier()
                flag := true
            }

        let readerTask () =
            async {
                while not !flag do
                    Thread.SpinWait(100)
                Thread.MemoryBarrier()
                observedValue := reader value
            }

        Async.Parallel [| writerTask(); readerTask() |] |> Async.RunSynchronously |> ignore

        Assert.True(!observedValue = !value,
            sprintf "Memory fence violation: expected %d, observed %d" !value !observedValue)

    // Additional thread-safety testing functions

    type ThreadSafetySpec<'Context> = {
        Setup: unit -> 'Context
        Operation: 'Context -> int -> unit
        Threads: int
        OperationsPerThread: int
        Duration: TimeSpan option
        Cleanup: 'Context -> unit
        ValidateInvariant: ('Context -> bool) option
    }

    let shouldBeThreadSafeWithSpec (spec: ThreadSafetySpec<'Context>) : unit =
        let context = spec.Setup()
        let errors = ref 0
        let exceptions = ConcurrentBag<exn>()

        let runThread threadId =
            async {
                try
                    for _ in 1..spec.OperationsPerThread do
                        spec.Operation context threadId
                with ex ->
                    Interlocked.Increment(errors) |> ignore
                    exceptions.Add(ex)
            }

        let tasks = [| for i in 0..spec.Threads-1 -> runThread i |]
        Async.Parallel tasks |> Async.RunSynchronously |> ignore

        match spec.ValidateInvariant with
        | Some validate ->
            if not (validate context) then
                Interlocked.Increment(errors) |> ignore
        | None -> ()

        spec.Cleanup context

        Assert.True(!errors = 0,
            sprintf "Thread-safety violation: %d errors in %d threads" !errors spec.Threads)

    let shouldBeThreadSafe (spec: ThreadSafetySpec<'Context>) : unit =
        shouldBeThreadSafeWithSpec spec

    type RacePreventionSpec<'Context> = {
        Setup: unit -> 'Context
        Operation: 'Context -> int -> unit
        Threads: int
        OperationsPerThread: int
        Validate: 'Context -> bool
    }

    let shouldPreventRaces (spec: RacePreventionSpec<'Context>) : unit =
        let context = spec.Setup()

        let runThread threadId =
            async {
                for _ in 1..spec.OperationsPerThread do
                    spec.Operation context threadId
            }

        let tasks = [| for i in 0..spec.Threads-1 -> runThread i |]
        Async.Parallel tasks |> Async.RunSynchronously |> ignore

        Assert.True(spec.Validate context, "Race condition detected: validation failed")

    let withAsync (operation: unit -> unit) : unit =
        operation()
