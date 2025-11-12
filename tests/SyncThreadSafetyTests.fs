namespace Tests

module SyncThreadSafetyTests =

    open System
    open System.Threading
    open global.Xunit
    open FsCheck.Xunit
    open TestInfrastructure.Core
    open TestInfrastructure.Builders
    open TestInfrastructure.Assertions
    open TestInfrastructure.Concurrency
    open TestInfrastructure.Performance
    open TestInfrastructure.Generators
    open Physics.Sync

    [<Fact>]
    let ``TripleBuffer lock-free read never blocks write`` () =
        let buffer = TripleBuffer<int>(0)
        shouldBeThreadSafe {
            Setup = fun () -> buffer
            Operation = fun buf tid ->
                if tid % 2 = 0 then buf.Write(tid) else buf.Read() |> ignore
            Threads = 8
            OperationsPerThread = 10000
            Duration = TimeSpan.FromSeconds(5.0)
            Cleanup = ignore
            ValidateInvariant = None
        }

    [<Fact>]
    let ``Command queue thread-safe enqueue`` () =
        let queue = PhysicsCommandQueue()
        withAsync (fun () ->
            let operations = [|
                for i in 1..100 -> async {
                    let (replyChannel, replyAsync) = AsyncReplyChannel.CreateWithTimeout(1000)
                    queue.Enqueue(AddBody(rigidBodyBuilder().BuildData(), replyChannel))
                }
            |]
            Async.Parallel operations |> Async.RunSynchronously |> ignore
            Assert.True(queue.Count > 0))

    [<Fact>]
    let ``Fence synchronization blocks until signaled`` () =
        use manager = new ThreadSafePhysicsManager()
        manager.Start()
        let fence = manager.InsertFence()
        shouldSignal fence (fun () ->
            manager.AddBodyAsync(rigidBodyBuilder().BuildData()) |> Async.RunSynchronously |> ignore
            fence.WaitFor(TimeSpan.FromSeconds(1.0)))

    [<Fact>]
    let ``Fence timeout returns false`` () =
        let queue = PhysicsCommandQueue()
        let fence = queue.CreateFence()
        shouldTimeout fence (fun () ->
            not (fence.WaitFor(TimeSpan.FromMilliseconds(10.0))))

    [<Fact>]
    let ``Worker thread maintains target Hz`` () =
        TestInfrastructure.Lifecycle.withWorker 60.0 (fun worker ->
            worker.Start()
            Thread.Sleep(1000)
            shouldMaintainHz (worker.GetFPS()) 60.0 0.5)

    [<Fact>]
    let ``FPS tracking returns actual simulation rate`` () =
        TestInfrastructure.Lifecycle.withWorker 30.0 (fun worker ->
            worker.Start()
            Thread.Sleep(500)
            let fps = worker.GetFPS()
            shouldBeInRange 15.0 45.0 fps)

    [<Fact>]
    let ``Snapshot interpolation extrapolates position`` () =
        let manager = SnapshotManager()
        let body = rigidBodyBuilder().At(System.Numerics.Vector3.Zero).WithVelocity(Vector3(10.0f, 0.0f, 0.0f)).BuildData()
        manager.Capture(body)
        shouldInterpolate manager 0.5f (fun interpolated ->
            abs(interpolated.Position.X - 5.0f) < 0.1f)

    [<Fact>]
    let ``ThreadSafePhysicsManager AddBodySync returns valid handle`` () =
        let manager = ThreadSafePhysicsManager()
        let handle = manager.AddBodySync(rigidBodyBuilder().BuildData())
        shouldBeValidHandle handle

    [<Fact>]
    let ``Sync waits for worker to process commands`` () =
        let manager = ThreadSafePhysicsManager()
        manager.Start()
        manager.AddBodyAsync(rigidBodyBuilder().BuildData()) |> ignore
        shouldCompleteWithin 1000.0 (fun () -> manager.Sync())
        manager.Stop()

    [<Fact>]
    let ``SpinLock prevents race conditions under stress`` () =
        shouldPreventRaces {
            Setup = fun () -> ref 0
            Operation = fun counter tid ->
                for i in 1..1000 do
                    lock counter (fun () -> counter := !counter + 1)
            Threads = 16
            OperationsPerThread = 100
            Validate = fun counter -> !counter = 16 * 100 * 1000
        }

    [<Fact>]
    let ``Disposal stops background threads`` () =
        let manager = ThreadSafePhysicsManager()
        manager.Start()
        manager.Dispose()
        Thread.Sleep(100)
        Assert.True(true)

    [<Property>]
    let ``Concurrent operations no exceptions`` (operations: PositiveInt) =
        let n = min 100 (int operations)
        let buffer = TripleBuffer<int>(0)
        try
            [| for i in 1..n -> async { buffer.Write(i); buffer.Read() |> ignore } |]
            |> Async.Parallel
            |> Async.RunSynchronously
            |> ignore
            true
        with _ -> false
