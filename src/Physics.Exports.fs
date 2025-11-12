namespace Physics

open System
open System.Numerics
open System.Runtime.InteropServices

/// C-compatible exports for Unity/Godot integration
module Exports =

    // C-compatible vector struct
    [<Struct; StructLayout(LayoutKind.Sequential)>]
    type Vec3 = {
        X: float32
        Y: float32
        Z: float32
    }

    // C-compatible quaternion struct
    [<Struct; StructLayout(LayoutKind.Sequential)>]
    type Quat = {
        X: float32
        Y: float32
        Z: float32
        W: float32
    }

    // C-compatible body data
    [<Struct; StructLayout(LayoutKind.Sequential)>]
    type CBodyData = {
        Position: Vec3
        Rotation: Quat
        Velocity: Vec3
        AngularVelocity: Vec3
        Mass: float32
        InertiaX: float32
        InertiaY: float32
        InertiaZ: float32
        IsKinematic: int  // 0 or 1
        IsStatic: int     // 0 or 1
    }

    // C-compatible raycast hit
    [<Struct; StructLayout(LayoutKind.Sequential)>]
    type CRaycastHit = {
        BodyHandle: int
        Point: Vec3
        Normal: Vec3
        Distance: float32
        Fraction: float32
    }

    // Convert helpers
    let inline toVec3 (v: Vector3) : Vec3 =
        { X = v.X; Y = v.Y; Z = v.Z }

    let inline fromVec3 (v: Vec3) : Vector3 =
        Vector3(v.X, v.Y, v.Z)

    let inline toQuat (q: Quaternion) : Quat =
        { X = q.X; Y = q.Y; Z = q.Z; W = q.W }

    let inline fromQuat (q: Quat) : Quaternion =
        Quaternion(q.X, q.Y, q.Z, q.W)

    let toCBodyData (data: Service.RigidBodyData) : CBodyData =
        {
            Position = toVec3 data.Position
            Rotation = toQuat data.Rotation
            Velocity = toVec3 data.Velocity
            AngularVelocity = toVec3 data.AngularVelocity
            Mass = float32 (float data.Mass)
            InertiaX = data.Inertia.X
            InertiaY = data.Inertia.Y
            InertiaZ = data.Inertia.Z
            IsKinematic = if data.IsKinematic then 1 else 0
            IsStatic = if data.IsStatic then 1 else 0
        }

    let fromCBodyData (c: CBodyData) : Service.RigidBodyData =
        {
            Position = fromVec3 c.Position
            Rotation = fromQuat c.Rotation
            Velocity = fromVec3 c.Velocity
            AngularVelocity = fromVec3 c.AngularVelocity
            Mass = float c.Mass * 1.0<kg>
            Inertia = Vector3(c.InertiaX, c.InertiaY, c.InertiaZ)
            IsKinematic = c.IsKinematic <> 0
            IsStatic = c.IsStatic <> 0
        }

    // ======================
    // WORLD MANAGEMENT
    // ======================

    /// Initialize physics world with default config
    
    let physics_init() : int =
        try
            Service.initializeWorld Service.defaultWorldConfig
            1  // Success
        with
        | _ -> 0  // Failure

    /// Initialize with custom gravity
    
    let physics_init_gravity(gravityX: float32, gravityY: float32, gravityZ: float32) : int =
        try
            let config = { Service.defaultWorldConfig with
                            Gravity = Vector3(gravityX, gravityY, gravityZ) }
            Service.initializeWorld config
            1
        with
        | _ -> 0

    /// Shutdown physics world
    
    let physics_shutdown() : unit =
        Service.shutdownWorld()

    /// Step simulation forward
    
    let physics_step() : unit =
        let world = Service.getWorld()
        world.Step()

    /// Set world gravity
    
    let physics_set_gravity(x: float32, y: float32, z: float32) : unit =
        let world = Service.getWorld()
        world.SetGravity(Vector3(x, y, z))

    /// Get world gravity
    
    let physics_get_gravity(outX: byref<float32>, outY: byref<float32>, outZ: byref<float32>) : unit =
        let world = Service.getWorld()
        let gravity = world.GetGravity()
        outX <- gravity.X
        outY <- gravity.Y
        outZ <- gravity.Z

    // ======================
    // BODY MANAGEMENT
    // ======================

    /// Add rigid body, returns handle
    
    let physics_add_body(bodyData: byref<CBodyData>) : int =
        try
            let world = Service.getWorld()
            let data = fromCBodyData bodyData
            world.AddRigidBody(data)
        with
        | _ -> -1  // Invalid handle

    /// Remove rigid body
    
    let physics_remove_body(handle: int) : int =
        let world = Service.getWorld()
        if world.RemoveRigidBody(handle) then 1 else 0

    /// Get body data
    
    let physics_get_body(handle: int, outData: byref<CBodyData>) : int =
        let world = Service.getWorld()
        match world.GetRigidBody(handle) with
        | Some data ->
            outData <- toCBodyData data
            1
        | None -> 0

    /// Set body data (for kinematic control)
    
    let physics_set_body(handle: int, bodyData: byref<CBodyData>) : int =
        let world = Service.getWorld()
        let data = fromCBodyData bodyData
        if world.SetRigidBody(handle, data) then 1 else 0

    /// Get body position
    
    let physics_get_position(handle: int, outX: byref<float32>, outY: byref<float32>, outZ: byref<float32>) : int =
        let world = Service.getWorld()
        match world.GetRigidBody(handle) with
        | Some data ->
            outX <- data.Position.X
            outY <- data.Position.Y
            outZ <- data.Position.Z
            1
        | None -> 0

    /// Set body position
    
    let physics_set_position(handle: int, x: float32, y: float32, z: float32) : int =
        let world = Service.getWorld()
        match world.GetRigidBody(handle) with
        | Some data ->
            let updated = { data with Position = Vector3(x, y, z) }
            if world.SetRigidBody(handle, updated) then 1 else 0
        | None -> 0

    // ======================
    // FORCES & IMPULSES
    // ======================

    /// Apply force to body
    
    let physics_apply_force(handle: int, forceX: float32, forceY: float32, forceZ: float32) : unit =
        let world = Service.getWorld()
        world.ApplyForce(handle, Vector3(forceX, forceY, forceZ))

    /// Apply force at world position
    
    let physics_apply_force_at_position(handle: int, forceX: float32, forceY: float32, forceZ: float32,
                                        posX: float32, posY: float32, posZ: float32) : unit =
        let world = Service.getWorld()
        world.ApplyForceAtPosition(handle, Vector3(forceX, forceY, forceZ), Vector3(posX, posY, posZ))

    /// Apply impulse (immediate velocity change)
    
    let physics_apply_impulse(handle: int, impulseX: float32, impulseY: float32, impulseZ: float32) : unit =
        let world = Service.getWorld()
        world.ApplyImpulse(handle, Vector3(impulseX, impulseY, impulseZ))

    /// Apply angular impulse
    
    let physics_apply_angular_impulse(handle: int, impulseX: float32, impulseY: float32, impulseZ: float32) : unit =
        let world = Service.getWorld()
        world.ApplyAngularImpulse(handle, Vector3(impulseX, impulseY, impulseZ))

    // ======================
    // RAYCASTS & QUERIES
    // ======================

    /// Raycast, returns 1 if hit, 0 if miss
    
    let physics_raycast(originX: float32, originY: float32, originZ: float32,
                        dirX: float32, dirY: float32, dirZ: float32,
                        maxDist: float32, outHit: byref<CRaycastHit>) : int =
        let world = Service.getWorld()
        let ray = {
            Queries.Origin = Vector3(originX, originY, originZ)
            Queries.Direction = Vector3.Normalize(Vector3(dirX, dirY, dirZ))
            Queries.MaxDistance = float maxDist
        }

        match Queries.raycast world ray Queries.defaultFilter with
        | Some hit ->
            outHit <- {
                BodyHandle = hit.BodyHandle
                Point = toVec3 hit.Point
                Normal = toVec3 hit.Normal
                Distance = float32 hit.Distance
                Fraction = float32 hit.Fraction
            }
            1
        | None -> 0

    /// Sphere overlap query, returns count
    
    let physics_overlap_sphere(centerX: float32, centerY: float32, centerZ: float32, radius: float32,
                                outHandles: nativeint, maxHandles: int) : int =
        let world = Service.getWorld()
        let sphere = {
            Queries.Center = Vector3(centerX, centerY, centerZ)
            Queries.Radius = float radius
        }

        let handles = Queries.overlapSphere world sphere Queries.defaultFilter
        let count = min handles.Length maxHandles

        if count > 0 && outHandles <> IntPtr.Zero then
            Marshal.Copy(handles, 0, outHandles, count)

        count

    /// Point test - returns body handle or -1
    
    let physics_point_test(x: float32, y: float32, z: float32) : int =
        let world = Service.getWorld()
        match Queries.pointTest world (Vector3(x, y, z)) Queries.defaultFilter with
        | Some handle -> handle
        | None -> -1

    // ======================
    // COLLISION EVENTS
    // ======================

    [<Struct; StructLayout(LayoutKind.Sequential)>]
    type CCollisionEvent = {
        BodyA: int
        BodyB: int
        ContactPoint: Vec3
        Normal: Vec3
        Penetration: float32
        Impulse: float32
    }

    /// Get collision events from last step
    
    let physics_get_collision_events(outEvents: nativeint, maxEvents: int) : int =
        let world = Service.getWorld()
        let events = world.GetCollisionEvents()
        let count = min events.Length maxEvents

        if count > 0 && outEvents <> IntPtr.Zero then
            let cEvents = events |> Array.take count |> Array.map (fun e ->
                {
                    BodyA = e.BodyA
                    BodyB = e.BodyB
                    ContactPoint = toVec3 e.ContactPoint
                    Normal = toVec3 e.Normal
                    Penetration = float32 e.Penetration
                    Impulse = float32 e.Impulse
                }
            )
            for i in 0 .. count - 1 do
                Marshal.StructureToPtr(cEvents.[i], IntPtr.Add(outEvents, i * sizeof<CCollisionEvent>), false)

        count

    // ======================
    // STATISTICS
    // ======================

    [<Struct; StructLayout(LayoutKind.Sequential)>]
    type CPhysicsStats = {
        TotalTime: float32
        FrameCount: int64
        BodyCount: int
        ActiveBodies: int
    }

    /// Get simulation statistics
    
    let physics_get_stats(outStats: byref<CPhysicsStats>) : unit =
        let world = Service.getWorld()
        let stats = world.GetStatistics()
        outStats <- {
            TotalTime = float32 (float stats.TotalTime)
            FrameCount = stats.FrameCount
            BodyCount = stats.BodyCount
            ActiveBodies = stats.ActiveBodies
        }
