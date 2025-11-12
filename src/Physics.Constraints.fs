namespace Physics

open System
open System.Numerics

/// Advanced constraint types for physics engine integration
module Constraints =

    /// Hinge joint (revolute joint) - 1 rotational DOF
    type HingeConstraint = {
        BodyA: int
        BodyB: int
        AnchorA: Vector3      // Anchor point in A's local space
        AnchorB: Vector3      // Anchor point in B's local space
        AxisA: Vector3        // Hinge axis in A's local space
        AxisB: Vector3        // Hinge axis in B's local space
        EnableLimits: bool
        LowerLimit: float     // Radians
        UpperLimit: float     // Radians
        EnableMotor: bool
        MotorSpeed: float     // Target angular velocity (rad/s)
        MaxMotorTorque: float // Maximum torque motor can apply
    }

    /// Slider joint (prismatic joint) - 1 translational DOF
    type SliderConstraint = {
        BodyA: int
        BodyB: int
        AxisA: Vector3        // Slide axis in A's local space
        AxisB: Vector3        // Slide axis in B's local space
        EnableLimits: bool
        LowerLimit: float     // Distance in meters
        UpperLimit: float     // Distance in meters
        EnableMotor: bool
        MotorSpeed: float     // Target linear velocity (m/s)
        MaxMotorForce: float  // Maximum force motor can apply
    }

    /// Fixed joint - 0 DOF (welds bodies together)
    type FixedConstraint = {
        BodyA: int
        BodyB: int
        LocalFrameA: Matrix4x4  // Relative transform in A's space
        LocalFrameB: Matrix4x4  // Relative transform in B's space
        BreakForce: float option   // If Some, joint breaks above this force
        BreakTorque: float option  // If Some, joint breaks above this torque
    }

    /// Ball-and-socket joint (spherical joint) - 3 rotational DOF
    type BallSocketConstraint = {
        BodyA: int
        BodyB: int
        AnchorA: Vector3
        AnchorB: Vector3
        EnableSwingLimits: bool
        MaxSwingAngle: float  // Cone angle limit (radians)
        EnableTwistLimits: bool
        LowerTwistLimit: float
        UpperTwistLimit: float
    }

    /// Distance constraint - maintains fixed distance
    type DistanceConstraint = {
        BodyA: int
        BodyB: int
        AnchorA: Vector3
        AnchorB: Vector3
        Distance: float
        Compliance: float     // Softness (0 = rigid, higher = softer)
        Damping: float
    }

    /// Cone twist constraint (like shoulder joint)
    type ConeTwistConstraint = {
        BodyA: int
        BodyB: int
        FrameA: Matrix4x4
        FrameB: Matrix4x4
        SwingSpan1: float     // Cone angle limit around axis 1
        SwingSpan2: float     // Cone angle limit around axis 2
        TwistSpan: float      // Twist angle limit
        Softness: float
        BiasFactor: float
        RelaxationFactor: float
    }

    /// Generic 6-DOF constraint (Unity ConfigurableJoint equivalent)
    type Generic6DOFConstraint = {
        BodyA: int
        BodyB: int
        FrameA: Matrix4x4
        FrameB: Matrix4x4
        // Linear limits (X, Y, Z)
        LinearLowerLimit: Vector3
        LinearUpperLimit: Vector3
        // Angular limits (X, Y, Z) in radians
        AngularLowerLimit: Vector3
        AngularUpperLimit: Vector3
        // Linear motors
        EnableLinearMotor: Vector3  // Per-axis flags (0 or 1)
        LinearMotorVelocity: Vector3
        MaxLinearMotorForce: Vector3
        // Angular motors
        EnableAngularMotor: Vector3
        AngularMotorVelocity: Vector3
        MaxAngularMotorTorque: Vector3
    }

    /// Constraint solver configuration
    type SolverConfig = {
        Iterations: int              // Number of solver iterations
        Baumgarte: float             // Position correction factor (0-1)
        Slop: float                  // Allowed penetration before correction
        Beta: float                  // Velocity bias factor
        RestitutionThreshold: float  // Velocity below which restitution is ignored
    }

    let defaultSolverConfig = {
        Iterations = 10
        Baumgarte = 0.2
        Slop = 0.01
        Beta = 0.8
        RestitutionThreshold = 1.0
    }

    /// Constraint impulse for sequential impulse solver
    type ConstraintImpulse = {
        LinearImpulseA: Vector3
        AngularImpulseA: Vector3
        LinearImpulseB: Vector3
        AngularImpulseB: Vector3
    }

    /// Solve hinge constraint (simplified)
    let solveHinge (bodies: Map<int, Service.RigidBodyData>) (constraintDef: HingeConstraint) (config: SolverConfig) : ConstraintImpulse =
        match bodies.TryFind constraintDef.BodyA, bodies.TryFind constraintDef.BodyB with
        | Some bodyA, Some bodyB ->
            // Transform anchors to world space
            let worldAnchorA = Vector3.Transform(constraintDef.AnchorA, bodyA.Rotation) + bodyA.Position
            let worldAnchorB = Vector3.Transform(constraintDef.AnchorB, bodyB.Rotation) + bodyB.Position

            // Position constraintDef: keep anchors aligned
            let posError = worldAnchorB - worldAnchorA
            let posCorrection = posError * float32 config.Baumgarte

            // Calculate relative velocity at contact
            let velA = bodyA.Velocity
            let velB = bodyB.Velocity
            let relVel = velB - velA

            // Target velocity to correct position error
            let targetVel = posCorrection / float32 0.016  // Assume 60fps

            // Calculate impulse needed
            let deltaV = targetVel - relVel
            let invMassA = if bodyA.IsStatic then 0.0f else 1.0f / float32 (float bodyA.Mass)
            let invMassB = if bodyB.IsStatic then 0.0f else 1.0f / float32 (float bodyB.Mass)
            let effectiveMass = 1.0f / (invMassA + invMassB)

            let linearImpulse = deltaV * effectiveMass

            // Transform axis to world space
            let worldAxisA = Vector3.Transform(constraintDef.AxisA, bodyA.Rotation)
            let worldAxisB = Vector3.Transform(constraintDef.AxisB, bodyB.Rotation)

            // Angular constraintDef: keep axes aligned
            let axisError = Vector3.Cross(worldAxisA, worldAxisB)
            let angularCorrection = axisError * float32 config.Baumgarte

            {
                LinearImpulseA = -linearImpulse
                AngularImpulseA = -angularCorrection
                LinearImpulseB = linearImpulse
                AngularImpulseB = angularCorrection
            }
        | _ ->
            // Bodies not found, return zero impulse
            {
                LinearImpulseA = Vector3.Zero
                AngularImpulseA = Vector3.Zero
                LinearImpulseB = Vector3.Zero
                AngularImpulseB = Vector3.Zero
            }

    /// Solve slider constraint (simplified)
    let solveSlider (bodies: Map<int, Service.RigidBodyData>) (constraintDef: SliderConstraint) (config: SolverConfig) : ConstraintImpulse =
        match bodies.TryFind constraintDef.BodyA, bodies.TryFind constraintDef.BodyB with
        | Some bodyA, Some bodyB ->
            let worldAxisA = Vector3.Transform(constraintDef.AxisA, bodyA.Rotation)
            let worldAxisB = Vector3.Transform(constraintDef.AxisB, bodyB.Rotation)

            // Position along slide axis
            let delta = bodyB.Position - bodyA.Position
            let slideDistance = Vector3.Dot(delta, worldAxisA)

            // Limit enforcement
            let correction =
                if constraintDef.EnableLimits then
                    if slideDistance < float32 constraintDef.LowerLimit then
                        worldAxisA * (float32 constraintDef.LowerLimit - slideDistance)
                    elif slideDistance > float32 constraintDef.UpperLimit then
                        worldAxisA * (float32 constraintDef.UpperLimit - slideDistance)
                    else
                        Vector3.Zero
                else
                    Vector3.Zero

            let linearImpulse = correction * float32 config.Baumgarte

            // Keep axes aligned
            let axisError = Vector3.Cross(worldAxisA, worldAxisB)
            let angularCorrection = axisError * float32 config.Baumgarte

            // Motor
            let motorImpulse =
                if constraintDef.EnableMotor then
                    let currentVel = Vector3.Dot(bodyB.Velocity - bodyA.Velocity, worldAxisA)
                    let velError = float32 constraintDef.MotorSpeed - currentVel
                    let impulse = velError * float32 config.Beta
                    let clamped = max -constraintDef.MaxMotorForce (min constraintDef.MaxMotorForce (float impulse))
                    worldAxisA * float32 clamped
                else
                    Vector3.Zero

            {
                LinearImpulseA = -(linearImpulse + motorImpulse)
                AngularImpulseA = -angularCorrection
                LinearImpulseB = linearImpulse + motorImpulse
                AngularImpulseB = angularCorrection
            }
        | _ ->
            {
                LinearImpulseA = Vector3.Zero
                AngularImpulseA = Vector3.Zero
                LinearImpulseB = Vector3.Zero
                AngularImpulseB = Vector3.Zero
            }

    /// Solve distance constraint
    let solveDistance (bodies: Map<int, Service.RigidBodyData>) (constraintDef: DistanceConstraint) (config: SolverConfig) : ConstraintImpulse =
        match bodies.TryFind constraintDef.BodyA, bodies.TryFind constraintDef.BodyB with
        | Some bodyA, Some bodyB ->
            let worldAnchorA = Vector3.Transform(constraintDef.AnchorA, bodyA.Rotation) + bodyA.Position
            let worldAnchorB = Vector3.Transform(constraintDef.AnchorB, bodyB.Rotation) + bodyB.Position

            let delta = worldAnchorB - worldAnchorA
            let currentDist = delta.Length()

            if currentDist > 0.001f then
                let n = delta / currentDist
                let error = currentDist - float32 constraintDef.Distance

                // Compliance-based constraint (XPBD)
                let alpha = float32 constraintDef.Compliance / (0.016f * 0.016f)  // dt²
                let invMassA = if bodyA.IsStatic then 0.0f else 1.0f / float32 (float bodyA.Mass)
                let invMassB = if bodyB.IsStatic then 0.0f else 1.0f / float32 (float bodyB.Mass)

                let w = invMassA + invMassB
                let lambda = -error / (w + alpha)

                let impulse = n * lambda

                // Damping
                let relVel = bodyB.Velocity - bodyA.Velocity
                let velAlongN = Vector3.Dot(relVel, n)
                let dampingImpulse = n * (-velAlongN * float32 constraintDef.Damping)

                let totalImpulse = impulse + dampingImpulse

                {
                    LinearImpulseA = -totalImpulse
                    AngularImpulseA = Vector3.Zero
                    LinearImpulseB = totalImpulse
                    AngularImpulseB = Vector3.Zero
                }
            else
                {
                    LinearImpulseA = Vector3.Zero
                    AngularImpulseA = Vector3.Zero
                    LinearImpulseB = Vector3.Zero
                    AngularImpulseB = Vector3.Zero
                }
        | _ ->
            {
                LinearImpulseA = Vector3.Zero
                AngularImpulseA = Vector3.Zero
                LinearImpulseB = Vector3.Zero
                AngularImpulseB = Vector3.Zero
            }

    /// Apply constraint impulse to bodies
    let applyImpulse (world: Service.PhysicsWorld) (bodyHandle: Service.BodyHandle) (linearImpulse: Vector3) (angularImpulse: Vector3) : unit =
        world.ApplyImpulse(bodyHandle, linearImpulse)
        world.ApplyAngularImpulse(bodyHandle, angularImpulse)

    /// Sequential impulse constraint solver
    let solveConstraints
        (world: Service.PhysicsWorld)
        (hinges: HingeConstraint list)
        (sliders: SliderConstraint list)
        (distances: DistanceConstraint list)
        (config: SolverConfig) : unit =

        // Get all body data once
        let bodyHandles =
            (hinges |> List.collect (fun h -> [h.BodyA; h.BodyB]))
            @ (sliders |> List.collect (fun s -> [s.BodyA; s.BodyB]))
            @ (distances |> List.collect (fun d -> [d.BodyA; d.BodyB]))
            |> List.distinct

        // Iterative solver
        for _ in 1 .. config.Iterations do
            // Build body map
            let bodies =
                bodyHandles
                |> List.choose (fun handle ->
                    world.GetRigidBody(handle)
                    |> Option.map (fun body -> (handle, body))
                )
                |> Map.ofList

            // Solve hinges
            hinges |> List.iter (fun c ->
                let impulse = solveHinge bodies c config
                applyImpulse world c.BodyA impulse.LinearImpulseA impulse.AngularImpulseA
                applyImpulse world c.BodyB impulse.LinearImpulseB impulse.AngularImpulseB
            )

            // Solve sliders
            sliders |> List.iter (fun c ->
                let impulse = solveSlider bodies c config
                applyImpulse world c.BodyA impulse.LinearImpulseA impulse.AngularImpulseA
                applyImpulse world c.BodyB impulse.LinearImpulseB impulse.AngularImpulseB
            )

            // Solve distances
            distances |> List.iter (fun c ->
                let impulse = solveDistance bodies c config
                applyImpulse world c.BodyA impulse.LinearImpulseA impulse.AngularImpulseA
                applyImpulse world c.BodyB impulse.LinearImpulseB impulse.AngularImpulseB
            )
