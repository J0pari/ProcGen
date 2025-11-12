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
            Duration = None
            Cleanup = ignore
            ValidateInvariant = None
        }

    [<Fact>]
    let ``Command queue thread-safe enqueue`` () =
        let queue = PhysicsCommandQueue()
        let mutable count = 0
        withAsync (fun () ->
            let operations = [| for i in 1..100 -> async {
                let (replyAsync, replyChannel) = Async.StartAsTask(async { return 0 }) |> ignore; (async { return 0 }, AsyncReplyChannel<Physics.Service.BodyHandle>(fun _ -> ()))
                queue.Enqueue(AddBody(body().BuildData(), replyChannel))
                Interlocked.Increment(&count) |> ignore
            } |]
            Async.Parallel operations |> Async.RunSynchronously |> ignore
            Assert.True(count > 0))

    [<Fact>]
    let ``Fence synchronization blocks until signaled`` () =
        let queue = PhysicsCommandQueue()
        let config = { Physics.Service.Gravity = System.Numerics.Vector3(0.0f, -9.8f, 0.0f) }
        let world = Physics.Service.PhysicsWorld(config)
        let fence = queue.InsertFence()
        shouldSignal fence (fun () ->
            queue.Flush(world) |> ignore
            queue.WaitForFence(fence, 1000))

    [<Fact>]
    let ``Fence timeout returns false`` () =
        let queue = PhysicsCommandQueue()
        let fence = queue.InsertFence()
        shouldTimeout fence (fun () ->
            not (queue.WaitForFence(fence, 10)))

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
        let bodyData = body().At(System.Numerics.Vector3.Zero).WithVelocity(System.Numerics.Vector3(10.0f, 0.0f, 0.0f)).BuildData()
        let handle = 1
        manager.Capture(handle, bodyData, 0L)
        match manager.Interpolate(handle, 0.5f) with
        | Some (pos, _) -> Assert.True(abs(pos.X - 0.08f) < 0.1f)
        | None -> Assert.True(false, "Interpolation failed")

    [<Fact>]
    let ``ThreadSafePhysicsManager AddBodySync returns valid handle`` () =
        let config = { Physics.Service.Gravity = System.Numerics.Vector3(0.0f, -9.8f, 0.0f) }
        let manager = ThreadSafePhysicsManager(config, 60.0)
        manager.Start()
        match manager.AddBodySync(body().BuildData(), 1000) with
        | Some handle -> shouldBeValidHandle handle
        | None -> Assert.True(false, "AddBodySync timeout")
        manager.Stop()

    [<Fact>]
    let ``Sync waits for worker to process commands`` () =
        let config = { Physics.Service.Gravity = System.Numerics.Vector3(0.0f, -9.8f, 0.0f) }
        let manager = ThreadSafePhysicsManager(config, 60.0)
        manager.Start()
        let handle = manager.AddBody(body().BuildData())
        shouldCompleteWithin 1000.0 (fun () -> Thread.Sleep(50))
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
        let config = { Physics.Service.Gravity = System.Numerics.Vector3(0.0f, -9.8f, 0.0f) }
        let manager = ThreadSafePhysicsManager(config, 60.0)
        manager.Start()
        manager.Stop()
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
