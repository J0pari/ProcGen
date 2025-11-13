namespace TestInfrastructure

module Lifecycle =

    open System
    open Physics.Exports
    open Physics.Service
    open Physics.Sync

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

    let withWorker (targetHz: float) (f: PhysicsWorker -> unit) : unit =
        let world = PhysicsWorld(defaultWorldConfig)
        let worker = new PhysicsWorker(world, targetHz)
        try
            f worker
        finally
            (worker :> IDisposable).Dispose()

    let withAsync (f: unit -> unit) : unit =
        f()

    let withConfig<'T> (get: unit -> 'T) (update: 'T -> unit) (f: 'T -> unit) : unit =
        let original = get()
        try
            f original
        finally
            update original
