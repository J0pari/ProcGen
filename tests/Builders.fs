namespace TestInfrastructure

module Builders =

    open System
    open Core

    type GraphBuilder() =
        let mutable nodes = Set.empty
        let mutable edges = []

        member __.AddNode(node: GraphNode) =
            nodes <- Set.add node nodes
            __

        member __.AddEdge(from': GraphNode, to': GraphNode, weight: float) =
            edges <- { From = from'; To = to'; Weight = weight } :: edges
            __

        member __.AddNodes(nodeList: GraphNode list) =
            nodes <- Set.union nodes (Set.ofList nodeList)
            __

        member __.AddEdges(edgeList: Edge list) =
            edges <- edges @ edgeList
            __

        member _.Build() = { Nodes = nodes; Edges = edges }

    type BodyBuilder() =
        let mutable position = Vector3.zero
        let mutable velocity = Vector3.zero
        let mutable mass = 1.0f
        let mutable radius = 1.0f
        let mutable isFixed = false

        member __.At(x: float32, y: float32, z: float32) =
            position <- { X = x; Y = y; Z = z }
            __

        member __.At(pos: Vector3) =
            position <- pos
            __

        member __.AtPosition(pos: Vector3) =
            position <- pos
            __

        member __.WithVelocity(vx: float32, vy: float32, vz: float32) =
            velocity <- { X = vx; Y = vy; Z = vz }
            __

        member __.WithVelocityVector(vel: Vector3) =
            velocity <- vel
            __

        member __.WithMass(m: float32) =
            mass <- m
            __

        member __.WithRadius(r: float32) =
            radius <- r
            __

        member __.Fixed() =
            isFixed <- true
            __

        member _.Build() = {
            Position = position
            Velocity = velocity
            Mass = mass
            Radius = radius
            Fixed = isFixed
        }

        member _.BuildData() : Physics.Service.RigidBodyData = {
            Position = System.Numerics.Vector3(position.X, position.Y, position.Z)
            Rotation = System.Numerics.Quaternion.Identity
            Velocity = System.Numerics.Vector3(velocity.X, velocity.Y, velocity.Z)
            AngularVelocity = System.Numerics.Vector3.Zero
            Mass = float mass * 1.0<Physics.kg>
            Inertia = System.Numerics.Vector3.One
            IsKinematic = false
            IsStatic = isFixed
        }

    type SpringBuilder() =
        let mutable bodyA = 0
        let mutable bodyB = 1
        let mutable restLength = 10.0f
        let mutable stiffness = 1.0f
        let mutable damping = 0.1f

        member __.Between(a: int, b: int) =
            bodyA <- a
            bodyB <- b
            __

        member __.WithRestLength(length: float32) =
            restLength <- length
            __

        member __.WithStiffness(k: float32) =
            stiffness <- k
            __

        member __.WithDamping(c: float32) =
            damping <- c
            __

        member _.Build() = {
            BodyA = bodyA
            BodyB = bodyB
            RestLength = restLength
            Stiffness = stiffness
            Damping = damping
        }

    type StateBuilder() =
        let mutable bodies = [||]
        let mutable springs = []
        let mutable timeStep = 0.016f
        let mutable gravity = { X = 0.0f; Y = -9.81f; Z = 0.0f }

        member __.WithBodies(bodyArray: RigidBody array) =
            bodies <- bodyArray
            __

        member __.WithBody(body: RigidBody) =
            bodies <- Array.append bodies [| body |]
            __

        member __.WithSprings(springList: Spring list) =
            springs <- springList
            __

        member __.WithSpring(spring: Spring) =
            springs <- spring :: springs
            __

        member __.WithTimeStep(dt: float32) =
            timeStep <- dt
            __

        member __.WithGravity(gx: float32, gy: float32, gz: float32) =
            gravity <- { X = gx; Y = gy; Z = gz }
            __

        member _.Build() = {
            Bodies = bodies
            Springs = springs
            TimeStep = timeStep
            Gravity = gravity
        }

    type ChainBuilder() =
        let mutable nodeCount = 0
        let mutable spacing = 10.0f
        let mutable bodyMass = 1.0f
        let mutable bodyRadius = 1.0f
        let mutable springStiffness = 10.0f
        let mutable springDamping = 0.5f

        member __.WithNodeCount(count: int) =
            nodeCount <- count
            __

        member __.WithSpacing(s: float32) =
            spacing <- s
            __

        member __.WithBodyMass(m: float32) =
            bodyMass <- m
            __

        member __.WithBodyRadius(r: float32) =
            bodyRadius <- r
            __

        member __.WithSpringStiffness(k: float32) =
            springStiffness <- k
            __

        member __.WithSpringDamping(c: float32) =
            springDamping <- c
            __

        member _.Build() =
            let bodies =
                Array.init nodeCount (fun i ->
                    {
                        Position = { X = float32 i * spacing; Y = 0.0f; Z = 0.0f }
                        Velocity = Vector3.zero
                        Mass = bodyMass
                        Radius = bodyRadius
                        Fixed = false
                    })

            let springs =
                [0..nodeCount-2]
                |> List.map (fun i ->
                    {
                        BodyA = i
                        BodyB = i + 1
                        RestLength = spacing
                        Stiffness = springStiffness
                        Damping = springDamping
                    })

            {
                Bodies = bodies
                Springs = springs
                TimeStep = 0.016f
                Gravity = { X = 0.0f; Y = -9.81f; Z = 0.0f }
            }

    type ConfigBuilder() =
        let mutable threadCount = Environment.ProcessorCount
        let mutable bufferSize = 4096
        let mutable timeout = TimeSpan.FromSeconds(30.0)
        let mutable enableCache = true

        member __.WithThreadCount(count: int) =
            threadCount <- count
            __

        member __.WithBufferSize(size: int) =
            bufferSize <- size
            __

        member __.WithTimeout(ts: TimeSpan) =
            timeout <- ts
            __

        member __.WithTimeoutSeconds(seconds: float) =
            timeout <- TimeSpan.FromSeconds(seconds)
            __

        member __.WithCaching(enabled: bool) =
            enableCache <- enabled
            __

        member _.Build() = {
            ThreadCount = threadCount
            BufferSize = bufferSize
            Timeout = timeout
            EnableCache = enableCache
        }

    type PositionArrayBuilder() =
        let mutable count = 0
        let mutable distribution = "uniform"
        let mutable bounds = 100.0f
        let mutable center = Vector3.zero
        let mutable seed = System.DateTime.Now.Ticks |> int

        member __.WithCount(n: int) =
            count <- n
            __

        member __.WithBounds(b: float32) =
            bounds <- b
            __

        member __.WithCenter(c: Vector3) =
            center <- c
            __

        member __.WithSeed(s: int) =
            seed <- s
            __

        member __.Uniform() =
            distribution <- "uniform"
            __

        member __.Clustered(radius: float32) =
            distribution <- "clustered"
            bounds <- radius
            __

        member __.AtSamePoint(pt: System.Numerics.Vector3) =
            distribution <- "samepoint"
            center <- { X = pt.X; Y = pt.Y; Z = pt.Z }
            __

        member __.Spherical() =
            distribution <- "spherical"
            __

        member __.Grid() =
            distribution <- "grid"
            __

        member _.Build() =
            let rng = Random(seed)
            match distribution with
            | "samepoint" ->
                Array.init count (fun _ -> center)
            | "clustered" ->
                Array.init count (fun _ ->
                    {
                        X = center.X + (float32 (rng.NextDouble() * 2.0 - 1.0)) * bounds
                        Y = center.Y + (float32 (rng.NextDouble() * 2.0 - 1.0)) * bounds
                        Z = center.Z + (float32 (rng.NextDouble() * 2.0 - 1.0)) * bounds
                    })
            | "uniform" ->
                Array.init count (fun _ ->
                    {
                        X = center.X + (float32 (rng.NextDouble() * 2.0 - 1.0)) * bounds
                        Y = center.Y + (float32 (rng.NextDouble() * 2.0 - 1.0)) * bounds
                        Z = center.Z + (float32 (rng.NextDouble() * 2.0 - 1.0)) * bounds
                    })
            | "spherical" ->
                Array.init count (fun _ ->
                    let theta = rng.NextDouble() * 2.0 * Math.PI
                    let phi = Math.Acos(2.0 * rng.NextDouble() - 1.0)
                    let r = float32 (rng.NextDouble() * float bounds)
                    {
                        X = center.X + r * float32 (Math.Sin(phi) * Math.Cos(theta))
                        Y = center.Y + r * float32 (Math.Sin(phi) * Math.Sin(theta))
                        Z = center.Z + r * float32 (Math.Cos(phi))
                    })
            | "grid" ->
                let side = int (Math.Ceiling(Math.Pow(float count, 1.0/3.0)))
                Array.init count (fun i ->
                    let x = i % side
                    let y = (i / side) % side
                    let z = i / (side * side)
                    let spacing = 2.0f * bounds / float32 side
                    {
                        X = center.X + (float32 x - float32 side / 2.0f) * spacing
                        Y = center.Y + (float32 y - float32 side / 2.0f) * spacing
                        Z = center.Z + (float32 z - float32 side / 2.0f) * spacing
                    })
            | _ -> Array.empty

    let linearGraph (nodeCount: int) : Graph =
        let nodes = Set.ofList [0..nodeCount-1]
        let edges =
            [0..nodeCount-2]
            |> List.map (fun i -> { From = i; To = i + 1; Weight = 1.0 })
        { Nodes = nodes; Edges = edges }

    let fullyConnectedGraph (nodeCount: int) : Graph =
        let nodes = Set.ofList [0..nodeCount-1]
        let edges =
            [ for i in 0..nodeCount-1 do
                for j in i+1..nodeCount-1 do
                    yield { From = i; To = j; Weight = 1.0 } ]
        { Nodes = nodes; Edges = edges }


    let disconnectedGraph (count1: int) (count2: int) : Graph =
        let nodes = Set.ofList [0..count1+count2-1]
        let edges1 = [0..count1-2] |> List.map (fun i -> { From = i; To = i + 1; Weight = 1.0 })
        let edges2 = [count1..count1+count2-2] |> List.map (fun i -> { From = i; To = i + 1; Weight = 1.0 })
        { Nodes = nodes; Edges = edges1 @ edges2 }

    let graphWithIdealEdgeLengths (length: float32) : Graph =
        fullyConnectedGraph 5

    let graphWithDistortedEdgeLengths (ideal: float32) (distortion: float32) : Graph =
        fullyConnectedGraph 5

    /// Convert test Graph to SpatialGraph with generated positions
    let toSpatialGraph (graph: Graph) : ParallelTempering.Core.SpatialGraph<GraphNode> =
        let nodeCount = Set.count graph.Nodes
        let nodes = graph.Nodes |> Set.toArray
        let positions =
            nodes
            |> Array.mapi (fun i _ ->
                System.Numerics.Vector3(float32 i * 10.0f, 0.0f, 0.0f))
        let edges =
            graph.Edges
            |> List.map (fun e -> (e.From, e.To, e.Weight))
        { ParallelTempering.Core.SpatialGraph.Nodes = nodes; Positions = positions; Edges = edges }

    /// Create SpatialGraph with ideal edge lengths
    let spatialGraphWithIdealEdgeLengths (length: float) (nodeCount: int) : ParallelTempering.Core.SpatialGraph<int> =
        let nodes = [|0..nodeCount-1|]
        let positions =
            nodes
            |> Array.mapi (fun i _ ->
                let angle = float32 i * 2.0f * System.MathF.PI / float32 nodeCount
                System.Numerics.Vector3(float32 length * cos angle, 0.0f, float32 length * sin angle))
        let edges =
            [ for i in 0..nodeCount-1 do
                for j in i+1..nodeCount-1 do
                    yield (i, j, System.Numerics.Vector3.Distance(positions.[i], positions.[j]) |> float) ]
        { ParallelTempering.Core.SpatialGraph.Nodes = nodes; Positions = positions; Edges = edges }

    /// Create SpatialGraph with distorted edge lengths
    let spatialGraphWithDistortedEdgeLengths (ideal: float) (distortion: float) (nodeCount: int) : ParallelTempering.Core.SpatialGraph<int> =
        let nodes = [|0..nodeCount-1|]
        let rng = System.Random(42)
        let positions =
            nodes
            |> Array.mapi (fun i _ ->
                let angle = float32 i * 2.0f * System.MathF.PI / float32 nodeCount
                let r = float32 ideal + float32 distortion * (float32 (rng.NextDouble()) - 0.5f) * 2.0f
                System.Numerics.Vector3(r * cos angle, 0.0f, r * sin angle))
        let edges =
            [ for i in 0..nodeCount-1 do
                for j in i+1..nodeCount-1 do
                    yield (i, j, System.Numerics.Vector3.Distance(positions.[i], positions.[j]) |> float) ]
        { ParallelTempering.Core.SpatialGraph.Nodes = nodes; Positions = positions; Edges = edges }

    let positionArray() = PositionArrayBuilder()

    let graph() = GraphBuilder()
    let body() = BodyBuilder()
    let spring() = SpringBuilder()
    let state() = StateBuilder()
    let chain() = ChainBuilder()
    let config() = ConfigBuilder()
    let positions() = PositionArrayBuilder()

    // Physics module wrapper functions
    // Bridge between test types (TestInfrastructure.Core) and production Physics types

    /// Wrapper for Physics.Verlet.stableTimeStep
    /// Takes float32 values (test convention) and returns float32
    let stableTimeStep (k: float32) (m: float32) : float32 =
        let kWithUnit = float k * 1.0<Physics.N/Physics.m>
        let mWithUnit = float m * 1.0<Physics.kg>
        let dtWithUnit = Physics.Verlet.stableTimeStep kWithUnit mWithUnit
        float32 (float dtWithUnit)

    /// Wrapper for Physics.Verlet.integrate
    /// Takes float32 timestep and test PhysicsState, returns test PhysicsState
    let integrateVerlet (dt: float32) (testState: Core.PhysicsState) : Core.PhysicsState =
        // Convert test PhysicsState to Physics.PhysicsState
        let physProps: Physics.PhysicalProperties array =
            testState.Bodies
            |> Array.map (fun b ->
                {
                    Mass = float b.Mass * 1.0<Physics.kg>
                    Velocity = System.Numerics.Vector3(b.Velocity.X, b.Velocity.Y, b.Velocity.Z)
                    Acceleration = System.Numerics.Vector3.Zero
                    Fixed = b.Fixed
                } : Physics.PhysicalProperties)

        let physPositions: System.Numerics.Vector3 array =
            testState.Bodies
            |> Array.map (fun b -> System.Numerics.Vector3(b.Position.X, b.Position.Y, b.Position.Z))

        let physSprings: Physics.SpringConstraint list =
            testState.Springs
            |> List.map (fun s ->
                {
                    NodeA = s.BodyA
                    NodeB = s.BodyB
                    RestLength = float s.RestLength * 1.0<Physics.m>
                    Stiffness = float s.Stiffness * 1.0<Physics.N/Physics.m>
                    Damping = float s.Damping * 1.0<Physics.N*Physics.s/Physics.m>
                } : Physics.SpringConstraint)

        let physState: Physics.PhysicsState = {
            Properties = physProps
            Positions = physPositions
            Springs = physSprings
            TimeStep = float dt * 1.0<Physics.s>
        }

        // Run integration
        let integrated = Physics.Verlet.integrate physState

        // Convert back to test types
        let newBodies =
            Array.mapi (fun i _ ->
                {
                    Position = {
                        X = integrated.Positions.[i].X
                        Y = integrated.Positions.[i].Y
                        Z = integrated.Positions.[i].Z
                    }
                    Velocity = {
                        X = integrated.Properties.[i].Velocity.X
                        Y = integrated.Properties.[i].Velocity.Y
                        Z = integrated.Properties.[i].Velocity.Z
                    }
                    Mass = testState.Bodies.[i].Mass
                    Radius = testState.Bodies.[i].Radius
                    Fixed = testState.Bodies.[i].Fixed
                }
            ) testState.Bodies

        { testState with Bodies = newBodies }

    /// Compute spring force between two bodies
    let springForce (spring: Core.Spring) (b1: Core.RigidBody) (b2: Core.RigidBody) : Core.Vector3 =
        let pos1 = System.Numerics.Vector3(b1.Position.X, b1.Position.Y, b1.Position.Z)
        let pos2 = System.Numerics.Vector3(b2.Position.X, b2.Position.Y, b2.Position.Z)
        let delta = pos2 - pos1
        let dist = delta.Length()

        if dist < 0.001f then Core.Vector3.zero
        else
            let direction = delta / dist
            let displacement = dist - spring.RestLength
            let force = direction * (displacement * spring.Stiffness)
            { X = force.X; Y = force.Y; Z = force.Z }

    /// Detect collisions using Physics.Collision module
    let detectCollisions (minDist: float32) (positions: System.Numerics.Vector3 array) : (int * int) list =
        Physics.Collision.detectCollisions (float minDist) positions
        |> List.map (fun pair -> (pair.NodeA, pair.NodeB))

    /// Convert test Graph to test PhysicsState
    let toPhysicsState (graph: Core.Graph) : Core.PhysicsState =
        let nodeList = Set.toList graph.Nodes
        let bodies =
            nodeList
            |> List.mapi (fun i _ ->
                {
                    Core.RigidBody.Position = { X = float32 i * 10.0f; Y = 0.0f; Z = 0.0f }
                    Velocity = Core.Vector3.zero
                    Mass = 1.0f
                    Radius = 1.0f
                    Fixed = (i = 0 || i = nodeList.Length - 1)  // Fix endpoints
                })
            |> List.toArray

        let springs =
            graph.Edges
            |> List.map (fun e ->
                let idxA = List.findIndex ((=) e.From) nodeList
                let idxB = List.findIndex ((=) e.To) nodeList
                {
                    Core.Spring.BodyA = idxA
                    BodyB = idxB
                    RestLength = float32 e.Weight
                    Stiffness = 10.0f
                    Damping = 0.5f
                })

        {
            Core.PhysicsState.Bodies = bodies
            Springs = springs
            TimeStep = 0.016f
            Gravity = { X = 0.0f; Y = 0.0f; Z = 0.0f }
        }

    /// Relax physics state over multiple iterations
    let relax (state: Core.PhysicsState) (iterations: int) : Core.PhysicsState =
        let mutable current = state
        for _ in 1 .. iterations do
            current <- integrateVerlet current.TimeStep current
        current

    /// Compute total spring energy cost
    let physicsCost (state: Core.PhysicsState) : float32 =
        state.Springs
        |> List.sumBy (fun spring ->
            let b1 = state.Bodies.[spring.BodyA]
            let b2 = state.Bodies.[spring.BodyB]
            let dx = b2.Position.X - b1.Position.X
            let dy = b2.Position.Y - b1.Position.Y
            let dz = b2.Position.Z - b1.Position.Z
            let dist = sqrt (dx*dx + dy*dy + dz*dz)
            let displacement = dist - spring.RestLength
            displacement * displacement * spring.Stiffness / 2.0f)

    /// Export physics scene (stub for now - returns empty structure)
    let exportPhysicsScene (graph: Core.Graph) : {| RigidBodies: Core.RigidBody array; Constraints: Core.Spring list |} =
        let state = toPhysicsState graph
        {| RigidBodies = state.Bodies; Constraints = state.Springs |}

    // Additional builders for missing functionality

    // Ray builder for physics queries
    type Ray = { Origin: System.Numerics.Vector3; Direction: System.Numerics.Vector3; MaxDistance: float32 }
    type QueryFilter = { ExcludeStatic: bool; ExcludeDynamic: bool }
    type SphereQuery = { Center: System.Numerics.Vector3; Radius: float32; Filter: QueryFilter option }
    type BoxQuery = { Min: System.Numerics.Vector3; Max: System.Numerics.Vector3 }
    type RaycastHit = { Distance: float32; Normal: System.Numerics.Vector3; BodyHandle: int }
    type PhysicsWorld = { Bodies: Core.RigidBody array }

    type RayBuilder() =
        let mutable from = System.Numerics.Vector3.Zero
        let mutable direction = System.Numerics.Vector3.UnitZ
        let mutable maxDistance = 100.0f

        member this.From(x: float32, y: float32, z: float32) =
            from <- System.Numerics.Vector3(x, y, z)
            this

        member this.From(v: System.Numerics.Vector3) =
            from <- v
            this

        member this.Direction(x: float32, y: float32, z: float32) =
            direction <- System.Numerics.Vector3(x, y, z)
            this

        member this.Direction(v: System.Numerics.Vector3) =
            direction <- v
            this

        member this.MaxDistance(d: float32) =
            maxDistance <- d
            this

        member this.Build() =
            { Origin = from; Direction = direction; MaxDistance = maxDistance }

    let rayBuilder() = RayBuilder()

    // Sphere query builder
    type SphereQueryBuilder() =
        let mutable center = System.Numerics.Vector3.Zero
        let mutable radius = 1.0f
        let mutable filter = None

        member this.At(x: float32, y: float32, z: float32) =
            center <- System.Numerics.Vector3(x, y, z)
            this

        member this.At(v: System.Numerics.Vector3) =
            center <- v
            this

        member this.WithRadius(r: float32) =
            radius <- r
            this

        member this.WithFilter(f: QueryFilter) =
            filter <- Some f
            this

        member this.Build() =
            { Center = center; Radius = radius; Filter = filter }

    let sphereQuery() = SphereQueryBuilder()

    // Box query builder
    type BoxQueryBuilder() =
        let mutable min = System.Numerics.Vector3.Zero
        let mutable max = System.Numerics.Vector3.One

        member this.Min(x: float32, y: float32, z: float32) =
            min <- System.Numerics.Vector3(x, y, z)
            this

        member this.Min(v: System.Numerics.Vector3) =
            min <- v
            this

        member this.Max(x: float32, y: float32, z: float32) =
            max <- System.Numerics.Vector3(x, y, z)
            this

        member this.Max(v: System.Numerics.Vector3) =
            max <- v
            this

        member this.Build() =
            { Min = min; Max = max }

    let boxQuery() = BoxQueryBuilder()

    // Filter builder
    type FilterBuilder() =
        let mutable excludeStatic = false
        let mutable excludeDynamic = false

        member this.ExcludeStatic() =
            excludeStatic <- true
            this

        member this.ExcludeDynamic() =
            excludeDynamic <- true
            this

        member this.Build() =
            { ExcludeStatic = excludeStatic; ExcludeDynamic = excludeDynamic }

    let withFilter() = FilterBuilder()

    // Physics world builder that returns a builder with WithBody methods
    type PhysicsWorldBuilder() =
        let mutable bodies = []

        member this.WithBody(body: Core.RigidBody) =
            bodies <- body :: bodies
            this

        member this.Build() =
            { Bodies = bodies |> List.rev |> List.toArray }

    let physicsWorld() = PhysicsWorldBuilder()

    // Rigid body builder alias
    let rigidBodyBuilder() = body()

    // Helper to run N simulation steps
    let stepN (world: 'World) (n: int) : unit =
        for _ in 1..n do
            ()  // Stub - actual implementation depends on Physics.Service

    // Spatial hash helper module
    module SpatialHash =
        let distance (a: Vector3) (b: Vector3) : float32 =
            let dx = a.X - b.X
            let dy = a.Y - b.Y
            let dz = a.Z - b.Z
            sqrt (dx * dx + dy * dy + dz * dz)

        let detectCollisions (minDist: float32) (positions: Vector3 array) : (int * int) list =
            let mutable collisions = []
            for i in 0..positions.Length-2 do
                for j in i+1..positions.Length-1 do
                    let dist = distance positions.[i] positions.[j]
                    if dist < minDist then
                        collisions <- (i, j) :: collisions
            List.rev collisions

        let intersect (positions: Vector3 array) (a: int) (b: int) : bool =
            let dist = distance positions.[a] positions.[b]
            dist < 1.0f

        let collisionPenalty (minDist: float32) (positions: Vector3 array) : float =
            let collisions = detectCollisions minDist positions
            collisions
            |> List.sumBy (fun (i, j) ->
                let dist = distance positions.[i] positions.[j]
                max 0.0 (float minDist - float dist))

    let intersect = SpatialHash.intersect
    let collisionPenalty = SpatialHash.collisionPenalty

    // Physics query functions (stubs for now)
    let raycast (world: PhysicsWorld) (ray: Ray) : RaycastHit option =
        None  // Stub implementation

    // Body builder extensions for physics service
    type BodyBuilder with
        member this.At(v: System.Numerics.Vector3) =
            this.At(v.X, v.Y, v.Z)

        member this.Static() =
            this.Fixed()

        member this.Dynamic() =
            this

        member this.Kinematic() =
            this.Fixed()

        member this.WithVelocity(v: System.Numerics.Vector3) =
            this.WithVelocity(v.X, v.Y, v.Z)
