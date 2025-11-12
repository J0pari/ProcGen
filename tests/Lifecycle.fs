namespace TestInfrastructure

module Lifecycle =

    open System
    open Physics.Exports
    open Physics.Service
    open Physics.Sync

    /// Stub WorkerThread for testing
    type WorkerThread(targetHz: float) =
        let mutable running = false
        let mutable fps = 0.0

        member _.Start() = running <- true
        member _.Stop() = running <- false
        member _.GetFPS() = fps

        interface IDisposable with
            member _.Dispose() = running <- false

    let withPhysicsEngine (f: unit -> unit) : unit =
        try
            physics_init() |> ignore
            f()
        finally
            physics_shutdown()

    let withPhysicsEngine' (f: unit -> 'T) : 'T =
        try
            physics_init() |> ignore
            f()
        finally
            physics_shutdown()

    let withWorld (f: PhysicsWorld -> unit) : unit =
        try
            initializeWorld defaultWorldConfig
            let world = getWorld()
            f world
        finally
            shutdownWorld()

    let withWorker (targetHz: float) (f: WorkerThread -> unit) : unit =
        let worker = new WorkerThread(targetHz)
        try
            f worker
        finally
            if worker :> IDisposable |> Option.ofObj |> Option.isSome then
                (worker :> IDisposable).Dispose()

    let withAsync (f: unit -> unit) : unit =
        f()

    let withConfig<'T> (get: unit -> 'T) (update: 'T -> unit) (f: 'T -> unit) : unit =
        let original = get()
        try
            f original
        finally
            update original
