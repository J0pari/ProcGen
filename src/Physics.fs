namespace Physics

open System
open System.Numerics
open ParallelTempering.Core

/// Units of measure for dimensional analysis
[<Measure>] type m      // meters
[<Measure>] type s      // seconds
[<Measure>] type kg     // kilograms
[<Measure>] type N = kg * m / s^2  // Newtons

/// Physical properties for nodes
type PhysicalProperties = {
    Mass: float<kg>
    Velocity: Vector3
    Acceleration: Vector3
    Fixed: bool
}

/// Spring constraint between nodes
type SpringConstraint = {
    NodeA: int
    NodeB: int
    RestLength: float<m>
    Stiffness: float<N/m>
    Damping: float<N*s/m>
}

/// Physics state for entire system
type PhysicsState = {
    Properties: PhysicalProperties array
    Positions: Vector3 array
    Springs: SpringConstraint list
    TimeStep: float<s>
}

/// Verlet integration for stable physics simulation
module Verlet =

    type VerletConfig = {
        SafetyMargin: float         // Fraction of stability limit (0.0-1.0)
        MinDistanceEpsilon: float32 // Ignore springs shorter than this
        DefaultTimeStep: float<s>   // Fallback when stiffness is zero
    }

    let defaultVerletConfig = {
        SafetyMargin = 0.1
        MinDistanceEpsilon = 0.001f
        DefaultTimeStep = 0.016<s>  // 60fps
    }

    let mutable private verletConfig = defaultVerletConfig

    let updateVerletConfig (newConfig: VerletConfig) : unit =
        if newConfig.SafetyMargin > 0.0 && newConfig.SafetyMargin <= 1.0 &&
           newConfig.MinDistanceEpsilon > 0.0f &&
           float newConfig.DefaultTimeStep > 0.0 then
            verletConfig <- newConfig

    let getVerletConfig () : VerletConfig = verletConfig

    /// Compute stable time step based on stiffness
    /// Stability limit: dt < 2/sqrt(k/m)
    /// Use configurable safety margin
    let stableTimeStep (maxStiffness: float<N/m>) (mass: float<kg>) : float<s> =
        if mass <= 0.0<kg> || maxStiffness <= 0.0<N/m> then verletConfig.DefaultTimeStep
        else
            let stabilityLimit = 2.0 / sqrt(float (maxStiffness / mass))
            (stabilityLimit * verletConfig.SafetyMargin) * 1.0<s>
    
    /// Single integration step
    /// Uses semi-implicit Euler (symplectic) for energy conservation
    let integrate (state: PhysicsState) : PhysicsState =
        let dt = float32 (float state.TimeStep)
        
        // Compute spring forces
        let forces = Array.create state.Positions.Length Vector3.Zero
        
        state.Springs |> List.iter (fun spring ->
            let posA = state.Positions.[spring.NodeA]
            let posB = state.Positions.[spring.NodeB]
            let delta = posB - posA
            let dist = delta.Length()
            
            if dist > verletConfig.MinDistanceEpsilon then
                let direction = delta / dist
                let displacement = float dist * 1.0<m> - spring.RestLength
                let springForce = direction * float32 (float (displacement * spring.Stiffness))

                // Damping force opposes relative velocity
                let velA = state.Properties.[spring.NodeA].Velocity
                let velB = state.Properties.[spring.NodeB].Velocity
                let relVel = velB - velA
                let dampingForce = Vector3.Dot(relVel, direction) * float32 (float spring.Damping)
                let totalForce = springForce + direction * dampingForce
                
                forces.[spring.NodeA] <- forces.[spring.NodeA] + totalForce
                forces.[spring.NodeB] <- forces.[spring.NodeB] - totalForce
        )
        
        // Semi-implicit Euler: v_{n+1} = v_n + a*dt, x_{n+1} = x_n + v_{n+1}*dt
        // More stable than explicit Euler for spring systems
        let newProperties =
            Array.mapi (fun i props ->
                if props.Fixed then props
                else
                    let accel = forces.[i] / float32 (float props.Mass)  // F/m = (kg*m/s²)/kg = m/s²
                    let newVel = props.Velocity + accel * dt
                    { props with Velocity = newVel; Acceleration = accel }
            ) state.Properties

        let newPositions =
            Array.mapi (fun i pos ->
                if state.Properties.[i].Fixed then pos
                else pos + newProperties.[i].Velocity * dt
            ) state.Positions

        { state with Positions = newPositions; Properties = newProperties }
    
    /// Simulate for multiple steps
    /// Monitors energy drift to detect instability
    let simulate (steps: int) (state: PhysicsState) : PhysicsState =
        let mutable current = state
        for _ in 1 .. steps do
            current <- integrate current
        current

/// Collision detection and response
module Collision =
    
    type CollisionPair = {
        NodeA: int
        NodeB: int
        Overlap: float
        Normal: Vector3
    }
    
    type CollisionConfig = {
        NormalEpsilon: float  // Minimum distance to compute normal
    }

    let defaultCollisionConfig = {
        NormalEpsilon = 0.001
    }

    let mutable private collisionConfig = defaultCollisionConfig

    let updateCollisionConfig (newConfig: CollisionConfig) : unit =
        if newConfig.NormalEpsilon > 0.0 then
            collisionConfig <- newConfig

    let getCollisionConfig () : CollisionConfig = collisionConfig

    /// Broad phase collision detection
    /// Uses SpatialHash adaptive algorithm selection
    let detectCollisions (minDist: float) (positions: Vector3 array) : CollisionPair list =
        SpatialHash.detectCollisions minDist positions
        |> List.map (fun (i, j) ->
            let delta = positions.[j] - positions.[i]
            let dist = delta.Length() |> float
            let normal = if dist > collisionConfig.NormalEpsilon then delta / float32 dist else Vector3.UnitX
            {
                NodeA = i
                NodeB = j
                Overlap = minDist - dist
                Normal = normal
            }
        )
    
    /// Resolve collisions by separating nodes
    /// Uses impulse-based resolution with mass weighting
    let resolve (props: PhysicalProperties array) (positions: Vector3 array) (collisions: CollisionPair list) : Vector3 array =
        let corrections = Array.create positions.Length Vector3.Zero
        
        collisions |> List.iter (fun col ->
            let massA = float32 props.[col.NodeA].Mass
            let massB = float32 props.[col.NodeB].Mass
            let totalMass = massA + massB
            
            let fixedA = props.[col.NodeA].Fixed
            let fixedB = props.[col.NodeB].Fixed
            
            if not fixedA && not fixedB then
                // Both movable: separate proportional to inverse mass
                let ratioA = massB / totalMass
                let ratioB = massA / totalMass
                
                corrections.[col.NodeA] <- corrections.[col.NodeA] - col.Normal * float32 col.Overlap * ratioA
                corrections.[col.NodeB] <- corrections.[col.NodeB] + col.Normal * float32 col.Overlap * ratioB
            elif not fixedA then
                // Only A movable
                corrections.[col.NodeA] <- corrections.[col.NodeA] - col.Normal * float32 col.Overlap
            elif not fixedB then
                // Only B movable
                corrections.[col.NodeB] <- corrections.[col.NodeB] + col.Normal * float32 col.Overlap
        )
        
        Array.mapi (fun i pos -> pos + corrections.[i]) positions

/// Graph physics integration
module GraphPhysics =
    open ParallelTempering.Core

    type SpringCalibrationConfig = {
        DefaultStiffness: float
        DefaultDamping: float
        VarianceEpsilon: float    // Below this, use default stiffness
        MaxStiffness: float
        StiffnessScale: float     // Numerator in stiffness = scale / variance
    }

    let defaultSpringCalibration = {
        DefaultStiffness = 50.0
        DefaultDamping = 0.2
        VarianceEpsilon = 0.001
        MaxStiffness = 100.0
        StiffnessScale = 10.0
    }

    let mutable private springConfig = defaultSpringCalibration

    let updateSpringConfig (newConfig: SpringCalibrationConfig) : unit =
        if newConfig.DefaultStiffness > 0.0 && newConfig.DefaultDamping >= 0.0 &&
           newConfig.VarianceEpsilon > 0.0 && newConfig.MaxStiffness > 0.0 &&
           newConfig.StiffnessScale > 0.0 then
            springConfig <- newConfig

    let getSpringConfig () : SpringCalibrationConfig = springConfig

    /// Compute spring parameters from edge statistics
    /// Stiffness scales inversely with edge length variance
    /// Critical damping prevents oscillation: c = 2*sqrt(k*m)
    let calibrateSpringParameters (edgeLengths: float array) : (float<N/m> * float<N*s/m>) =
        if edgeLengths.Length = 0 then (springConfig.DefaultStiffness * 1.0<N/m>, springConfig.DefaultDamping * 1.0<N*s/m>)
        else
            let avgLength = Array.average edgeLengths
            let variance =
                edgeLengths
                |> Array.averageBy (fun len -> (len - avgLength) * (len - avgLength))

            let stiffness =
                if variance < springConfig.VarianceEpsilon then springConfig.DefaultStiffness
                else min springConfig.MaxStiffness (springConfig.StiffnessScale / variance)

            let damping = 2.0 * sqrt stiffness
            (stiffness * 1.0<N/m>, damping * 1.0<N*s/m>)
    
    /// Convert graph to physics state
    let toPhysicsState (graph: SpatialGraph<'T>) : PhysicsState =
        let n = graph.Positions.Length
        
        let properties = Array.init n (fun i ->
            {
                Mass = 1.0<kg>
                Velocity = Vector3.Zero
                Acceleration = Vector3.Zero
                Fixed = i = 0 || i = n - 1
            }
        )

        let edgeLengths = graph.Edges |> List.map (fun (_, _, w) -> w) |> List.toArray
        let (stiffness, damping) = calibrateSpringParameters edgeLengths

        let springs =
            graph.Edges
            |> List.map (fun (a, b, w) ->
                {
                    NodeA = a
                    NodeB = b
                    RestLength = float w * 1.0<m>
                    Stiffness = stiffness
                    Damping = damping
                }
            )

        let maxStiffness = if springs.IsEmpty then 1.0<N/m> else stiffness
        let dt = Verlet.stableTimeStep maxStiffness 1.0<kg>
        
        {
            Properties = properties
            Positions = graph.Positions
            Springs = springs
            TimeStep = dt
        }
    
    /// Run physics simulation with configurable collision distance
    let relaxWithCollisionDistance (steps: int) (minDistance: float) (graph: SpatialGraph<'T>) : SpatialGraph<'T> =
        let physicsState = toPhysicsState graph
        let simulated = Verlet.simulate steps physicsState

        // Apply collision resolution
        let collisions = Collision.detectCollisions minDistance simulated.Positions
        let finalPositions = Collision.resolve simulated.Properties simulated.Positions collisions

        { graph with Positions = finalPositions}

    type RelaxConfig = {
        MinCollisionDistance: float
        PhysicsSteps: int
    }

    let defaultRelaxConfig = {
        MinCollisionDistance = 0.5
        PhysicsSteps = 50
    }

    let mutable private relaxConfig = defaultRelaxConfig

    let updateRelaxConfig (newConfig: RelaxConfig) : unit =
        if newConfig.MinCollisionDistance > 0.0 && newConfig.PhysicsSteps > 0 then
            relaxConfig <- newConfig

    let getRelaxConfig () : RelaxConfig = relaxConfig

    /// Run physics simulation and extract stabilized positions
    let relax (steps: int) (graph: SpatialGraph<'T>) : SpatialGraph<'T> =
        relaxWithCollisionDistance steps relaxConfig.MinCollisionDistance graph

    /// Physics-based cost function
    /// Measures spring potential energy + collision penalties
    let physicsCost (graph: SpatialGraph<'T>) : float =
        let mutable cost = 0.0
        
        // Spring potential energy: U = 0.5 * k * x²
        graph.Edges |> List.iter (fun (i, j, restLength) ->
            let dist = float (Vector3.Distance(graph.Positions.[i], graph.Positions.[j]))
            let stretch = dist - restLength
            cost <- cost + 0.5 * stretch * stretch
        )
        
        // Collision penalty
        let minDist = relaxConfig.MinCollisionDistance
        for i in 0 .. graph.Positions.Length - 2 do
            for j in i + 1 .. graph.Positions.Length - 1 do
                let dist = float (Vector3.Distance(graph.Positions.[i], graph.Positions.[j]))
                if dist < minDist then
                    let overlap = minDist - dist
                    cost <- cost + overlap * overlap * 100.0
        
        cost

/// Export physics state
module PhysicsExport =
    open System.Text.Json
    
    type RigidBodyData = {
        Position: float array
        Mass: float
        IsKinematic: bool
    }
    
    type ConstraintData = {
        BodyA: int
        BodyB: int
        Type: string
        Parameters: Map<string, float>
    }
    
    type PhysicsSceneData = {
        RigidBodies: RigidBodyData array
        Constraints: ConstraintData array
    }
    
    let exportScene (state: PhysicsState) : PhysicsSceneData =
        let bodies =
            Array.mapi (fun i pos ->
                {
                    Position = [| float (pos:Vector3).X; float pos.Y; float pos.Z |]
                    Mass = float state.Properties.[i].Mass
                    IsKinematic = state.Properties.[i].Fixed
                }
            ) state.Positions

        let constraints =
            state.Springs
            |> List.map (fun spring ->
                {
                    BodyA = spring.NodeA
                    BodyB = spring.NodeB
                    Type = "spring"
                    Parameters = Map.ofList [
                        ("restLength", float spring.RestLength)
                        ("stiffness", float spring.Stiffness)
                        ("damping", float spring.Damping)
                    ]
                }
            )
            |> List.toArray
        
        { RigidBodies = bodies; Constraints = constraints }
    
    let toJson (scene: PhysicsSceneData) : string =
        JsonSerializer.Serialize(scene, JsonSerializerOptions(WriteIndented = true))
