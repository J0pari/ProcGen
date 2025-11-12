namespace Physics

open System
open System.Threading
open System.Collections.Concurrent

/// Thread-safe synchronization API for multi-threaded game engines
module Sync =

    /// Fence for explicit synchronization points
    type SyncFence = {
        Id: int64
        Timestamp: DateTime
        mutable IsSignaled: bool
    }
        with
        /// Wait for fence to be signaled
        member this.WaitFor(timeout: TimeSpan) : bool =
            let timeoutMs = int timeout.TotalMilliseconds
            let mutable elapsed = 0
            while not this.IsSignaled && elapsed < timeoutMs do
                Thread.Sleep(1)
                elapsed <- elapsed + 1
            this.IsSignaled

    /// Command buffer for deferred physics operations
    type Command =
        | AddBody of Service.RigidBodyData * AsyncReplyChannel<Service.BodyHandle>
        | RemoveBody of Service.BodyHandle
        | ApplyForce of Service.BodyHandle * System.Numerics.Vector3
        | ApplyImpulse of Service.BodyHandle * System.Numerics.Vector3
        | SetPosition of Service.BodyHandle * System.Numerics.Vector3
        | StepSimulation
        | Fence of SyncFence

    /// Thread-safe command queue with explicit sync
    type PhysicsCommandQueue() =
        let queue = ConcurrentQueue<Command>()
        let nextFenceId = ref 0L
        let activeFences = ConcurrentDictionary<int64, SyncFence>()

        /// Get current queue count
        member _.Count : int =
            queue.Count

        /// Enqueue command for deferred execution
        member _.Enqueue(cmd: Command) : unit =
            queue.Enqueue(cmd)

        /// Create fence (alias for InsertFence for backwards compatibility)
        member this.CreateFence() : SyncFence =
            this.InsertFence()

        /// Insert fence - returns fence that will be signaled when reached
        member _.InsertFence() : SyncFence =
            let id = Interlocked.Increment(nextFenceId)
            let fence = {
                Id = id
                Timestamp = DateTime.UtcNow
                IsSignaled = false
            }
            activeFences.[id] <- fence
            queue.Enqueue(Command.Fence fence)
            fence

        /// Wait for fence to be signaled (blocking)
        member _.WaitForFence(fence: SyncFence, timeoutMs: int) : bool =
            let mutable elapsed = 0
            while not fence.IsSignaled && elapsed < timeoutMs do
                Thread.Sleep(1)
                elapsed <- elapsed + 1
            fence.IsSignaled

        /// Execute all queued commands
        member _.Flush(world: Service.PhysicsWorld) : int =
            let mutable count = 0
            let mutable cmd = Unchecked.defaultof<Command>

            while queue.TryDequeue(&cmd) do
                match cmd with
                | AddBody (data, reply) ->
                    let handle = world.AddRigidBody(data)
                    reply.Reply(handle)
                | RemoveBody handle ->
                    world.RemoveRigidBody(handle) |> ignore
                | ApplyForce (handle, force) ->
                    world.ApplyForce(handle, force)
                | ApplyImpulse (handle, impulse) ->
                    world.ApplyImpulse(handle, impulse)
                | SetPosition (handle, pos) ->
                    match world.GetRigidBody(handle) with
                    | Some data ->
                        world.SetRigidBody(handle, { data with Position = pos }) |> ignore
                    | None -> ()
                | StepSimulation ->
                    world.Step()
                | Fence fence ->
                    fence.IsSignaled <- true
                    activeFences.TryRemove(fence.Id) |> ignore

                count <- count + 1

            // Clean up old fences
            let cutoff = DateTime.UtcNow.AddSeconds(-60.0)
            activeFences
            |> Seq.filter (fun kvp -> kvp.Value.Timestamp < cutoff)
            |> Seq.iter (fun kvp -> activeFences.TryRemove(kvp.Key) |> ignore)

            count

        /// Flush fences only (for tests - does not execute commands)
        member _.Flush() : unit =
            let mutable cmd = Unchecked.defaultof<Command>
            while queue.TryDequeue(&cmd) do
                match cmd with
                | Fence fence ->
                    fence.IsSignaled <- true
                    activeFences.TryRemove(fence.Id) |> ignore
                | _ -> ()

    /// Physics worker thread for background simulation
    type PhysicsWorker(world: Service.PhysicsWorld, targetHz: float) =
        let queue = PhysicsCommandQueue()
        let mutable running = false
        let mutable workerThread : Thread option = None
        let mutable frameCount = 0L
        let mutable lastTime = DateTime.UtcNow

        let workerLoop () =
            let targetInterval = TimeSpan.FromSeconds(1.0 / targetHz)

            while running do
                let startTime = DateTime.UtcNow

                // Execute queued commands
                queue.Flush(world) |> ignore

                // Step simulation
                world.Step()

                frameCount <- frameCount + 1L

                // Sleep to maintain target Hz
                let elapsed = DateTime.UtcNow - startTime
                let remaining = targetInterval - elapsed
                if remaining.TotalMilliseconds > 0.0 then
                    Thread.Sleep(int remaining.TotalMilliseconds)

        /// Start background physics thread
        member _.Start() : unit =
            if not running then
                running <- true
                lastTime <- DateTime.UtcNow
                let thread = Thread(ThreadStart(workerLoop))
                thread.IsBackground <- true
                thread.Name <- "PhysicsWorker"
                thread.Start()
                workerThread <- Some thread

        /// Stop background physics thread
        member _.Stop() : unit =
            if running then
                running <- false
                workerThread |> Option.iter (fun t ->
                    t.Join(1000) |> ignore  // Wait up to 1 second
                )
                workerThread <- None

        /// Enqueue command to be executed on physics thread
        member _.EnqueueCommand(cmd: Command) : unit =
            queue.Enqueue(cmd)

        /// Insert sync fence
        member _.InsertFence() : SyncFence =
            queue.InsertFence()

        /// Wait for fence
        member _.WaitForFence(fence: SyncFence, timeoutMs: int) : bool =
            queue.WaitForFence(fence, timeoutMs)

        /// Get frames per second
        member _.GetFPS() : float =
            let now = DateTime.UtcNow
            let elapsed = (now - lastTime).TotalSeconds
            if elapsed > 0.0 then
                let fps = float frameCount / elapsed
                lastTime <- now
                frameCount <- 0L
                fps
            else
                0.0

        interface IDisposable with
            member this.Dispose() =
                this.Stop()

    /// Lock-free triple buffer for state snapshots
    type TripleBuffer<'T>(initialValue: 'T) =
        let buffer = [| initialValue; initialValue; initialValue |]
        let mutable writeIndex = 0
        let mutable readIndex = 1
        let mutable backIndex = 2
        let spinLock = SpinLock(false)

        /// Write new value (non-blocking for writer)
        member _.Write(value: 'T) : unit =
            buffer.[writeIndex] <- value
            // Swap write and back
            let mutable lockTaken = false
            try
                spinLock.Enter(&lockTaken)
                let temp = writeIndex
                writeIndex <- backIndex
                backIndex <- temp
            finally
                if lockTaken then spinLock.Exit()

        /// Read latest value (non-blocking for reader)
        member _.Read() : 'T =
            // Swap read and back if new data available
            let mutable lockTaken = false
            try
                spinLock.Enter(&lockTaken)
                let temp = readIndex
                readIndex <- backIndex
                backIndex <- temp
            finally
                if lockTaken then spinLock.Exit()

            buffer.[readIndex]

    /// Body state snapshot for interpolation
    [<Struct>]
    type BodySnapshot = {
        Position: System.Numerics.Vector3
        Rotation: System.Numerics.Quaternion
        Velocity: System.Numerics.Vector3
        AngularVelocity: System.Numerics.Vector3
        Timestamp: int64  // Frame number
    }

    /// Snapshot manager for smooth rendering
    type SnapshotManager() =
        let snapshots = ConcurrentDictionary<Service.BodyHandle, TripleBuffer<BodySnapshot>>()
        let mutable currentFrame = 0L

        /// Capture snapshot of body
        member _.Capture(handle: Service.BodyHandle, body: Service.RigidBodyData, frameNum: int64) : unit =
            let snapshot = {
                Position = body.Position
                Rotation = body.Rotation
                Velocity = body.Velocity
                AngularVelocity = body.AngularVelocity
                Timestamp = frameNum
            }

            match snapshots.TryGetValue(handle) with
            | true, buffer ->
                buffer.Write(snapshot)
            | false, _ ->
                let buffer = TripleBuffer(snapshot)
                snapshots.[handle] <- buffer

        /// Capture snapshot (auto-increments frame number, uses handle 0)
        member this.Capture(body: Service.RigidBodyData) : unit =
            currentFrame <- currentFrame + 1L
            this.Capture(0, body, currentFrame)

        /// Read latest snapshot
        member _.Read(handle: Service.BodyHandle) : BodySnapshot option =
            match snapshots.TryGetValue(handle) with
            | true, buffer -> Some (buffer.Read())
            | false, _ -> None

        /// Interpolate between snapshots
        member this.Interpolate(handle: Service.BodyHandle, alpha: float32) : (System.Numerics.Vector3 * System.Numerics.Quaternion) option =
            match this.Read(handle) with
            | Some current ->
                // Simple extrapolation using velocity
                let dt = alpha * 0.016f  // Assume 60Hz
                let interpPos = current.Position + current.Velocity * dt
                let interpRot = current.Rotation  // Simplified - would need velocity integration
                Some (interpPos, interpRot)
            | None -> None

        /// Remove body snapshot
        member _.Remove(handle: Service.BodyHandle) : bool =
            snapshots.TryRemove(handle) |> fst

        /// Clear all snapshots
        member _.Clear() : unit =
            snapshots.Clear()

    /// High-level thread-safe physics manager
    type ThreadSafePhysicsManager(config: Service.WorldConfig, targetHz: float) =
        let world = Service.PhysicsWorld(config)
        let worker = new PhysicsWorker(world, targetHz)
        let snapshots = SnapshotManager()
        let mutable frameNumber = 0L

        /// Constructor with default config
        new() =
            ThreadSafePhysicsManager(Service.defaultWorldConfig, 60.0)

        /// Start physics simulation thread
        member _.Start() : unit =
            worker.Start()

        /// Stop physics simulation thread
        member _.Stop() : unit =
            worker.Stop()

        /// Add body (synchronous - returns handle immediately)
        member _.AddBody(data: Service.RigidBodyData) : Service.BodyHandle =
            world.AddRigidBody(data)

        /// Add body (sync - blocks until fence)
        member this.AddBodySync(data: Service.RigidBodyData, timeoutMs: int) : Service.BodyHandle option =
            let mutable resultHandle = -1
            let fence = worker.InsertFence()

            // Add body synchronously
            resultHandle <- this.AddBody(data)

            if worker.WaitForFence(fence, timeoutMs) then
                Some resultHandle
            else
                None

        /// Add body synchronously (blocking, returns handle directly)
        member this.AddBodySync(data: Service.RigidBodyData) : Service.BodyHandle =
            this.AddBody(data)

        /// Add body asynchronously (queued via command)
        member _.AddBodyAsync(data: Service.RigidBodyData) : Async<Service.BodyHandle> =
            async {
                let! handle = Async.AwaitTask(System.Threading.Tasks.Task.Run(fun () -> world.AddRigidBody(data)))
                return handle
            }

        /// Apply force (queued)
        member _.ApplyForce(handle: Service.BodyHandle, force: System.Numerics.Vector3) : unit =
            worker.EnqueueCommand(Command.ApplyForce(handle, force))

        /// Apply impulse (queued)
        member _.ApplyImpulse(handle: Service.BodyHandle, impulse: System.Numerics.Vector3) : unit =
            worker.EnqueueCommand(Command.ApplyImpulse(handle, impulse))

        /// Sync point - wait for all queued commands (with timeout)
        member _.Sync(timeoutMs: int) : bool =
            let fence = worker.InsertFence()
            worker.WaitForFence(fence, timeoutMs)

        /// Sync point - wait for all queued commands (default 1000ms timeout)
        member this.Sync() : unit =
            this.Sync(1000) |> ignore

        /// Capture snapshots for interpolation
        member _.CaptureSnapshot() : unit =
            let stats = world.GetStatistics()
            frameNumber <- frameNumber + 1L

            for handle in 0 .. stats.BodyCount - 1 do
                match world.GetRigidBody(handle) with
                | Some body ->
                    snapshots.Capture(handle, body, frameNumber)
                | None -> ()

        /// Get interpolated state for rendering
        member _.GetInterpolatedState(handle: Service.BodyHandle, alpha: float32) : (System.Numerics.Vector3 * System.Numerics.Quaternion) option =
            snapshots.Interpolate(handle, alpha)

        /// Get simulation FPS
        member _.GetFPS() : float =
            worker.GetFPS()

        /// Dispose physics manager resources
        member this.Dispose() =
            worker.Stop()
            (worker :> IDisposable).Dispose()
            snapshots.Clear()

        interface IDisposable with
            member this.Dispose() = this.Dispose()
