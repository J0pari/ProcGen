namespace Physics

open System
open System.Numerics
open System.Collections.Concurrent
open System.Threading

/// Persistent physics simulation service for game engine integration
module Service =

    /// Handle for referencing bodies in the simulation
    type BodyHandle = int

    /// Rigid body data for external integration
    type RigidBodyData = {
        Position: Vector3
        Rotation: Quaternion
        Velocity: Vector3
        AngularVelocity: Vector3
        Mass: float<kg>
        Inertia: Vector3  // Diagonal inertia tensor (simplified)
        IsKinematic: bool  // If true, not affected by forces
        IsStatic: bool     // If true, infinite mass
    }

    /// Collision event data
    type CollisionEvent = {
        BodyA: BodyHandle
        BodyB: BodyHandle
        ContactPoint: Vector3
        Normal: Vector3
        Penetration: float
        Impulse: float
    }

    /// Physics world configuration
    type WorldConfig = {
        Gravity: Vector3
        TimeStep: float<s>
        SubSteps: int          // Physics substeps per frame
        SolverIterations: int  // Constraint solver iterations
        EnableSleeping: bool   // Put idle bodies to sleep
        SleepThreshold: float  // Velocity below which bodies sleep
    }

    let defaultWorldConfig = {
        Gravity = Vector3(0.0f, -9.81f, 0.0f)
        TimeStep = 0.016<s>  // 60fps
        SubSteps = 4
        SolverIterations = 10
        EnableSleeping = true
        SleepThreshold = 0.01
    }

    /// Internal body state with sleeping
    type private InternalBody = {
        Data: RigidBodyData
        Force: Vector3
        Torque: Vector3
        IsSleeping: bool
        SleepTimer: float
    }

    /// Thread-safe physics world with persistent state
    type PhysicsWorld(config: WorldConfig) =
        let mutable worldConfig = config
        let bodies = ConcurrentDictionary<BodyHandle, InternalBody>()
        let nextHandle = ref 0
        let collisionEvents = ConcurrentQueue<CollisionEvent>()
        let worldLock = obj()
        let mutable totalTime = 0.0<s>
        let mutable frameCount = 0L

        /// Add rigid body to simulation, returns handle
        member _.AddRigidBody(data: RigidBodyData) : BodyHandle =
            let handle = Interlocked.Increment(nextHandle)
            let internalBody = {
                Data = data
                Force = Vector3.Zero
                Torque = Vector3.Zero
                IsSleeping = false
                SleepTimer = 0.0
            }
            bodies.[handle] <- internalBody
            handle

        /// Remove body from simulation
        member _.RemoveRigidBody(handle: BodyHandle) : bool =
            let success, _ = bodies.TryRemove(handle)
            success

        /// Get body data (thread-safe snapshot)
        member _.GetRigidBody(handle: BodyHandle) : RigidBodyData option =
            match bodies.TryGetValue(handle) with
            | true, body -> Some body.Data
            | false, _ -> None

        /// Check if body is sleeping
        member _.IsBodySleeping(handle: BodyHandle) : bool option =
            match bodies.TryGetValue(handle) with
            | true, body -> Some body.IsSleeping
            | false, _ -> None

        /// Update body data (kinematic control)
        member _.SetRigidBody(handle: BodyHandle, data: RigidBodyData) : bool =
            match bodies.TryGetValue(handle) with
            | true, body ->
                bodies.[handle] <- { body with Data = data }
                true
            | false, _ -> false

        /// Apply force to body (accumulates until next step)
        member _.ApplyForce(handle: BodyHandle, force: Vector3) : unit =
            match bodies.TryGetValue(handle) with
            | true, body ->
                bodies.[handle] <- { body with Force = body.Force + force }
            | false, _ -> ()

        /// Apply force at world position (generates torque)
        member _.ApplyForceAtPosition(handle: BodyHandle, force: Vector3, position: Vector3) : unit =
            match bodies.TryGetValue(handle) with
            | true, body ->
                let r = position - body.Data.Position
                let torque = Vector3.Cross(r, force)
                bodies.[handle] <-
                    { body with
                        Force = body.Force + force
                        Torque = body.Torque + torque }
            | false, _ -> ()

        /// Apply impulse (immediate velocity change)
        member _.ApplyImpulse(handle: BodyHandle, impulse: Vector3) : unit =
            match bodies.TryGetValue(handle) with
            | true, body when not body.Data.IsKinematic && not body.Data.IsStatic ->
                let newVel = body.Data.Velocity + impulse / float32 (float body.Data.Mass)
                bodies.[handle] <-
                    { body with
                        Data = { body.Data with Velocity = newVel }
                        IsSleeping = false }
            | _ -> ()

        /// Apply angular impulse
        member _.ApplyAngularImpulse(handle: BodyHandle, impulse: Vector3) : unit =
            match bodies.TryGetValue(handle) with
            | true, body when not body.Data.IsKinematic && not body.Data.IsStatic ->
                let invInertia = Vector3(1.0f / body.Data.Inertia.X, 1.0f / body.Data.Inertia.Y, 1.0f / body.Data.Inertia.Z)
                let deltaOmega = impulse * invInertia
                bodies.[handle] <-
                    { body with
                        Data = { body.Data with AngularVelocity = body.Data.AngularVelocity + deltaOmega }
                        IsSleeping = false }
            | _ -> ()

        /// Set world gravity
        member _.SetGravity(gravity: Vector3) : unit =
            lock worldLock (fun () ->
                worldConfig <- { worldConfig with Gravity = gravity }
            )

        /// Get world gravity
        member _.GetGravity() : Vector3 = worldConfig.Gravity

        /// Get all collision events from last step
        member _.GetCollisionEvents() : CollisionEvent array =
            let events = ResizeArray<CollisionEvent>()
            let mutable event = Unchecked.defaultof<CollisionEvent>
            while collisionEvents.TryDequeue(&event) do
                events.Add(event)
            events.ToArray()

        /// Step simulation forward by configured timestep
        member this.Step() : unit =
            lock worldLock (fun () ->
                let dt = worldConfig.TimeStep
                let subDt = dt / float worldConfig.SubSteps

                for _ in 1 .. worldConfig.SubSteps do
                    // Integrate forces and velocities
                    for kvp in bodies do
                        let handle = kvp.Key
                        let body = kvp.Value

                        if body.IsSleeping || body.Data.IsKinematic || body.Data.IsStatic then
                            ()  // Skip sleeping/kinematic/static bodies
                        else
                            // Apply gravity
                            let gravityForce = worldConfig.Gravity * float32 (float body.Data.Mass)
                            let totalForce = body.Force + gravityForce

                            // Integrate linear motion
                            let accel = totalForce / float32 (float body.Data.Mass)
                            let newVel = body.Data.Velocity + accel * float32 (float subDt)
                            let newPos = body.Data.Position + newVel * float32 (float subDt)

                            // Integrate angular motion (simplified - no gyroscopic effects)
                            let invInertia = Vector3(1.0f / body.Data.Inertia.X, 1.0f / body.Data.Inertia.Y, 1.0f / body.Data.Inertia.Z)
                            let angAccel = body.Torque * invInertia
                            let newAngVel = body.Data.AngularVelocity + angAccel * float32 (float subDt)

                            // Integrate rotation (quaternion from angular velocity)
                            let angVelQuat = Quaternion(newAngVel * float32 (float subDt) * 0.5f, 0.0f)
                            let newRot = Quaternion.Normalize(body.Data.Rotation + angVelQuat * body.Data.Rotation)

                            // Check for sleeping
                            let speed = newVel.Length()
                            let angSpeed = newAngVel.Length()
                            let shouldSleep =
                                worldConfig.EnableSleeping &&
                                speed < float32 worldConfig.SleepThreshold &&
                                angSpeed < float32 worldConfig.SleepThreshold

                            let newSleepTimer = if shouldSleep then body.SleepTimer + float subDt else 0.0
                            let nowSleeping = newSleepTimer > 0.5  // Sleep after 0.5s of inactivity

                            bodies.[handle] <- {
                                Data = {
                                    body.Data with
                                        Position = newPos
                                        Rotation = newRot
                                        Velocity = newVel
                                        AngularVelocity = newAngVel
                                }
                                Force = Vector3.Zero  // Clear forces
                                Torque = Vector3.Zero
                                IsSleeping = nowSleeping
                                SleepTimer = newSleepTimer
                            }

                    // Detect collisions (simple sphere-sphere for now)
                    this.DetectAndResolveCollisions()

                totalTime <- totalTime + dt
                frameCount <- frameCount + 1L
            )

        /// Simple sphere-sphere collision detection and response
        member private this.DetectAndResolveCollisions() : unit =
            let bodyArray = bodies.ToArray()

            for i in 0 .. bodyArray.Length - 2 do
                for j in i + 1 .. bodyArray.Length - 1 do
                    let kvpA = bodyArray.[i]
                    let kvpB = bodyArray.[j]
                    let bodyA = kvpA.Value
                    let bodyB = kvpB.Value

                    // Skip if both sleeping or static
                    if (bodyA.IsSleeping || bodyA.Data.IsStatic) && (bodyB.IsSleeping || bodyB.Data.IsStatic) then
                        ()
                    else
                        let delta = bodyB.Data.Position - bodyA.Data.Position
                        let dist = delta.Length()
                        let minDist = 1.0f  // Assume unit sphere for now

                        if dist < minDist && dist > 0.001f then
                            let normal = delta / dist
                            let penetration = minDist - dist

                            // Calculate relative velocity at contact
                            let relVel = bodyB.Data.Velocity - bodyA.Data.Velocity

                            // Only resolve if bodies are approaching
                            let normalVel = Vector3.Dot(relVel, normal)
                            if normalVel < 0.0f then
                                // Collision response parameters
                                let restitution = 0.5f  // Bounciness
                                let friction = 0.3f     // Surface friction

                                // Calculate impulse magnitude
                                let invMassA = if bodyA.Data.IsStatic then 0.0f else 1.0f / float32 (float bodyA.Data.Mass)
                                let invMassB = if bodyB.Data.IsStatic then 0.0f else 1.0f / float32 (float bodyB.Data.Mass)
                                let invMassSum = invMassA + invMassB

                                if invMassSum > 0.0f then
                                    // Normal impulse
                                    let j = -(1.0f + restitution) * normalVel / invMassSum
                                    let impulse = normal * j

                                    // Apply impulses
                                    if not bodyA.Data.IsStatic && not bodyA.Data.IsKinematic then
                                        let newVelA = bodyA.Data.Velocity - impulse * invMassA
                                        bodies.[kvpA.Key] <-
                                            { bodyA with
                                                Data = { bodyA.Data with Velocity = newVelA }
                                                IsSleeping = false }

                                    if not bodyB.Data.IsStatic && not bodyB.Data.IsKinematic then
                                        let newVelB = bodyB.Data.Velocity + impulse * invMassB
                                        bodies.[kvpB.Key] <-
                                            { bodyB with
                                                Data = { bodyB.Data with Velocity = newVelB }
                                                IsSleeping = false }

                                    // Friction (tangential impulse)
                                    let tangent = relVel - normal * normalVel
                                    if tangent.Length() > 0.001f then
                                        let tangentNorm = Vector3.Normalize(tangent)
                                        let jt = -Vector3.Dot(relVel, tangentNorm) / invMassSum
                                        let frictionImpulse = tangentNorm * (min (abs jt) (friction * abs j))

                                        if not bodyA.Data.IsStatic && not bodyA.Data.IsKinematic then
                                            let velA = bodies.[kvpA.Key].Data.Velocity
                                            bodies.[kvpA.Key] <-
                                                { bodyA with
                                                    Data = { bodyA.Data with Velocity = velA - frictionImpulse * invMassA } }

                                        if not bodyB.Data.IsStatic && not bodyB.Data.IsKinematic then
                                            let velB = bodies.[kvpB.Key].Data.Velocity
                                            bodies.[kvpB.Key] <-
                                                { bodyB with
                                                    Data = { bodyB.Data with Velocity = velB + frictionImpulse * invMassB } }

                                    // Positional correction (prevent sinking)
                                    let correctionPercent = 0.2f
                                    let slop = 0.01f
                                    let correction = normal * (max (penetration - slop) 0.0f) * correctionPercent / invMassSum

                                    if not bodyA.Data.IsStatic && not bodyA.Data.IsKinematic then
                                        let posA = bodies.[kvpA.Key].Data.Position
                                        bodies.[kvpA.Key] <-
                                            { bodyA with
                                                Data = { bodyA.Data with Position = posA - correction * invMassA } }

                                    if not bodyB.Data.IsStatic && not bodyB.Data.IsKinematic then
                                        let posB = bodies.[kvpB.Key].Data.Position
                                        bodies.[kvpB.Key] <-
                                            { bodyB with
                                                Data = { bodyB.Data with Position = posB + correction * invMassB } }

                                    // Emit collision event
                                    collisionEvents.Enqueue({
                                        BodyA = kvpA.Key
                                        BodyB = kvpB.Key
                                        ContactPoint = bodyA.Data.Position + normal * (dist / 2.0f)
                                        Normal = normal
                                        Penetration = float penetration
                                        Impulse = float j
                                    })

        /// Get simulation statistics
        member _.GetStatistics() : {| TotalTime: float<s>; FrameCount: int64; BodyCount: int; ActiveBodies: int |} =
            let activeBodies = bodies |> Seq.filter (fun kvp -> not kvp.Value.IsSleeping) |> Seq.length
            {|
                TotalTime = totalTime
                FrameCount = frameCount
                BodyCount = bodies.Count
                ActiveBodies = activeBodies
            |}

        /// Clear all bodies and reset simulation
        member _.Clear() : unit =
            lock worldLock (fun () ->
                bodies.Clear()
                collisionEvents.Clear()
                totalTime <- 0.0<s>
                frameCount <- 0L
            )

    /// Global singleton instance for simple integration
    let mutable private globalWorld : PhysicsWorld option = None
    let private globalLock = obj()

    /// Initialize global physics world
    let initializeWorld (config: WorldConfig) : unit =
        lock globalLock (fun () ->
            globalWorld <- Some (PhysicsWorld(config))
        )

    /// Get or create global world
    let getWorld () : PhysicsWorld =
        lock globalLock (fun () ->
            match globalWorld with
            | Some world -> world
            | None ->
                let world = PhysicsWorld(defaultWorldConfig)
                globalWorld <- Some world
                world
        )

    /// Shutdown global world
    let shutdownWorld () : unit =
        lock globalLock (fun () ->
            globalWorld |> Option.iter (fun w -> w.Clear())
            globalWorld <- None
        )
