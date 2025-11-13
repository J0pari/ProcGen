namespace TestInfrastructure

module Generators =

    open System
    open FsCheck
    open FsCheck.FSharp
    open Core
    open AdaptiveConfig
    open Physics

    let genPositiveFloat : Gen<float> =
        Gen.choose (1, 100000)
        |> Gen.map (fun n -> float n / 100.0)

    let genFloatRange (min: float) (max: float) : Gen<float> =
        genPositiveFloat
        |> Gen.map (fun x -> min + x * (max - min))

    let genFloat32Range (min: float32) (max: float32) : Gen<float32> =
        genFloatRange (float min) (float max)
        |> Gen.map float32

    let genNonZeroFloat : Gen<float> =
        Gen.oneof [
            genFloatRange -1000.0 -0.001
            genFloatRange 0.001 1000.0
        ]

    let genTemperature : Gen<Temperature> =
        genFloatRange 0.1 10000.0
        |> Gen.map Temperature

    let genVector3 (minVal: float32) (maxVal: float32) : Gen<Vector3> =
        gen {
            let! x = genFloat32Range minVal maxVal
            let! y = genFloat32Range minVal maxVal
            let! z = genFloat32Range minVal maxVal
            return Vector3(x, y, z)
        }

    let genUnitVector : Gen<Vector3> =
        gen {
            let! v = genVector3 -10.0f 10.0f
            return Vector3.Normalize(v)
        }

    let genPositionArray (n: int) (bounds: float32) : Gen<Vector3 array> =
        Gen.arrayOfLength n (genVector3 -bounds bounds)

    let genRigidBody (bounds: float32) : Gen<RigidBody> =
        gen {
            let! pos = genVector3 -bounds bounds
            let! vel = genVector3 -10.0f 10.0f
            let! mass = genFloat32Range 0.1f 100.0f
            let! radius = genFloat32Range 0.1f 5.0f
            return { Position = pos; Velocity = vel; Mass = mass; Radius = radius; Fixed = false }
        }

    let genNonOverlappingBodies (n: int) (minRadius: float32) (maxRadius: float32) (bounds: float32) : Gen<RigidBody array> =
        let rec generateBody existing =
            gen {
                let! pos = genVector3 -bounds bounds
                let! vel = genVector3 -10.0f 10.0f
                let! mass = genFloat32Range 0.1f 100.0f
                let! radius = genFloat32Range minRadius maxRadius
                let body = { Position = pos; Velocity = vel; Mass = mass; Radius = radius; Fixed = false }

                let overlaps =
                    existing
                    |> List.exists (fun (b: RigidBody) ->
                        let dist = Vector3.distanceTo b.Position pos
                        dist < (b.Radius + radius))

                if overlaps then
                    return! generateBody existing
                else
                    return body
            }

        let rec generateBodies count acc =
            if count = 0 then
                Gen.constant (List.rev acc |> List.toArray)
            else
                gen {
                    let! body = generateBody acc
                    return! generateBodies (count - 1) (body :: acc)
                }

        generateBodies n []

    let genSpring (bodyCount: int) : Gen<Spring> =
        gen {
            let! bodyA = Gen.choose (0, bodyCount - 1)
            let! bodyB = Gen.choose (0, bodyCount - 1) |> Gen.filter ((<>) bodyA)
            let! restLength = genFloat32Range 1.0f 50.0f
            let! stiffness = genFloat32Range 0.1f 100.0f
            let! damping = genFloat32Range 0.0f 10.0f
            return {
                BodyA = bodyA
                BodyB = bodyB
                RestLength = restLength
                Stiffness = stiffness
                Damping = damping
            }
        }

    let genConnectedGraph (minNodes: int) (maxNodes: int) (edgeProbability: float) : Gen<Graph> =
        gen {
            let! nodeCount = Gen.choose (minNodes, maxNodes)
            let nodes = Set.ofList [0..nodeCount-1]

            let spanningEdges =
                [1..nodeCount-1]
                |> List.map (fun i ->
                    let target = i
                    let source = Random().Next(0, i)
                    { From = source; To = target; Weight = Random().NextDouble() * 10.0 })

            let! additionalEdges =
                [for i in 0..nodeCount-1 do
                    for j in i+1..nodeCount-1 ->
                        gen {
                            let! shouldInclude = Gen.frequency [(int (edgeProbability * 100.0), Gen.constant true); (int ((1.0 - edgeProbability) * 100.0), Gen.constant false)]
                            if shouldInclude then
                                let! weight = genFloatRange 0.1 10.0
                                return Some { From = i; To = j; Weight = weight }
                            else
                                return None
                        }]
                |> Gen.sequenceToList
                |> Gen.map (List.choose id)

            return {
                Nodes = nodes
                Edges = spanningEdges @ additionalEdges
            }
        }

    let genDAG (minNodes: int) (maxNodes: int) : Gen<Graph> =
        gen {
            let! nodeCount = Gen.choose (minNodes, maxNodes)
            let nodes = Set.ofList [0..nodeCount-1]

            let! edges =
                [for i in 0..nodeCount-2 do
                    for j in i+1..nodeCount-1 ->
                        gen {
                            let! shouldInclude = Gen.frequency [(30, Gen.constant true); (70, Gen.constant false)]
                            if shouldInclude then
                                let! weight = genFloatRange 0.1 10.0
                                return Some { From = i; To = j; Weight = weight }
                            else
                                return None
                        }]
                |> Gen.sequenceToList
                |> Gen.map (List.choose id)

            return { Nodes = nodes; Edges = edges }
        }

    let genPhysicsState (minBodies: int) (maxBodies: int) (maxSprings: int) : Gen<PhysicsState> =
        gen {
            let! bodyCount = Gen.choose (minBodies, maxBodies)
            let! bodies = genNonOverlappingBodies bodyCount 0.5f 3.0f 100.0f
            let! springCount = Gen.choose (0, min maxSprings (bodyCount * (bodyCount - 1) / 2))
            let! springs = Gen.listOfLength springCount (genSpring bodyCount)
            let! timeStep = genFloat32Range 0.001f 0.1f
            return {
                Properties = bodies |> Array.map (fun b -> { Mass = LanguagePrimitives.FloatWithMeasure<kg> (float b.Mass); Velocity = System.Numerics.Vector3(b.Velocity.X, b.Velocity.Y, b.Velocity.Z); Acceleration = System.Numerics.Vector3(0.0f, 0.0f, 0.0f); Fixed = false })
                Positions = bodies |> Array.map (fun b -> System.Numerics.Vector3(b.Position.X, b.Position.Y, b.Position.Z))
                Springs = springs |> List.map (fun s -> { NodeA = s.BodyA; NodeB = s.BodyB; Stiffness = LanguagePrimitives.FloatWithMeasure<N/m> (float s.Stiffness); RestLength = LanguagePrimitives.FloatWithMeasure<m> (float s.RestLength); Damping = LanguagePrimitives.FloatWithMeasure<N*s/m> (float s.Damping) })
                TimeStep = LanguagePrimitives.FloatWithMeasure<s> (float timeStep)
            }
        }

    let genTestPhysicsState (minBodies: int) (maxBodies: int) (maxSprings: int) : Gen<Core.PhysicsState> =
        gen {
            let! bodyCount = Gen.choose (minBodies, maxBodies)
            let! bodies = genNonOverlappingBodies bodyCount 0.5f 3.0f 100.0f
            let! springCount = Gen.choose (0, min maxSprings (bodyCount * (bodyCount - 1) / 2))
            let! springs = Gen.listOfLength springCount (genSpring bodyCount)
            let! timeStep = genFloat32Range 0.001f 0.1f
            return {
                Core.PhysicsState.Bodies = bodies
                Springs = springs
                TimeStep = timeStep
                Gravity = Vector3(0.0f, -9.81f, 0.0f)
            }
        }

    let genCacheEntry (genKey: Gen<'K>) (genValue: Gen<'V>) : Gen<CacheEntry<'K, 'V>> =
        gen {
            let! key = genKey
            let! value = genValue
            let! accessCount = Gen.choose (0, 1000)
            let! minutesAgo = Gen.choose (0, 10080)
            return {
                Key = key
                Value = value
                AccessCount = accessCount
                LastAccess = DateTime.Now.AddMinutes(-float minutesAgo)
            }
        }

    let genConfig : Gen<Config> =
        gen {
            let! threadCount = Gen.choose (1, Environment.ProcessorCount * 2)
            let! bufferSize = Gen.choose (1024, 1024 * 1024) |> Gen.map (fun n -> (n / 1024) * 1024)
            let! timeoutMs = Gen.choose (100, 60000)
            let! enableCache = Gen.elements [true; false]
            return {
                ThreadCount = threadCount
                BufferSize = bufferSize
                Timeout = TimeSpan.FromMilliseconds(float timeoutMs)
                EnableCache = enableCache
            }
        }

    let shrinkVector3 (v: Vector3) : Vector3 seq =
        seq {
            if abs v.X > 0.1f then yield Vector3(v.X / 2.0f, v.Y, v.Z)
            if abs v.Y > 0.1f then yield Vector3(v.X, v.Y / 2.0f, v.Z)
            if abs v.Z > 0.1f then yield Vector3(v.X, v.Y, v.Z / 2.0f)
            if v.X <> 0.0f || v.Y <> 0.0f || v.Z <> 0.0f then
                yield Vector3.Zero
        }

    let shrinkRigidBody (body: RigidBody) : RigidBody seq =
        seq {
            for pos in shrinkVector3 body.Position do
                yield { body with Position = pos }

            for vel in shrinkVector3 body.Velocity do
                yield { body with Velocity = vel }

            if body.Mass > 1.1f then
                yield { body with Mass = (body.Mass + 1.0f) / 2.0f }

            if body.Radius > 1.1f then
                yield { body with Radius = (body.Radius + 1.0f) / 2.0f }
        }

    let shrinkGraph (graph: Graph) : Graph seq =
        seq {
            if graph.Edges.Length > graph.Nodes.Count - 1 then
                for i in 0..graph.Edges.Length-1 do
                    let newEdges = graph.Edges |> List.removeAt i
                    yield { graph with Edges = newEdges }
        }

    let shrinkPhysicsState (state: PhysicsState) : PhysicsState seq =
        seq {
            if state.Springs.Length > 0 then
                yield { state with Springs = state.Springs |> List.skip 1 }

            if state.Properties.Length > 1 then
                yield { state with Properties = state.Properties |> Array.take (state.Properties.Length - 1) }

            if state.TimeStep > 0.01<s> then
                yield { state with TimeStep = state.TimeStep / 2.0 }
        }

    type Arbitraries =
        static member Vector3() =
            Arb.fromGenShrink(genVector3 -100.0f 100.0f, shrinkVector3)

        static member RigidBody() =
            Arb.fromGenShrink(genRigidBody 100.0f, shrinkRigidBody)

        static member Graph() =
            Arb.fromGenShrink(genConnectedGraph 3 20 0.3, shrinkGraph)

        static member PhysicsState() =
            Arb.fromGenShrink(genPhysicsState 2 10 5, shrinkPhysicsState)

        static member Temperature() =
            Arb.fromGen genTemperature

        static member Config() =
            Arb.fromGen genConfig

    let genGPUConfig() : Gen<AdaptiveConfig.GPUConfigData> =
        gen {
            let! deviceId = Gen.choose (0, 7)
            let! maxThreads = Gen.elements [64; 128; 256; 512; 1024]
            let! sharedMem = Gen.choose (16, 96) |> Gen.map (fun kb -> kb * 1024)
            return {
                AdaptiveConfig.GPUConfigData.DeviceId = deviceId
                MaxThreadsPerBlock = maxThreads
                SharedMemorySize = sharedMem
            }
        }

    let genSpatialHashConfig() : Gen<AdaptiveConfig.SpatialHashConfigData> =
        gen {
            let! cellSize = genFloatRange 0.1 10.0
            let! maxObjects = Gen.choose (100, 100000)
            let! loadFactor = genFloatRange 0.5 0.95
            return {
                AdaptiveConfig.SpatialHashConfigData.CellSize = cellSize
                MaxObjects = maxObjects
                LoadFactor = loadFactor
            }
        }

    // PositiveInt type for property tests
    type PositiveInt = PositiveInt of int
        with
        static member op_Explicit(PositiveInt n) = n
        static member op_Implicit(PositiveInt n) = float n
        member this.Value = let (PositiveInt n) = this in n
        static member ToInt(PositiveInt n) = n
        static member ToFloat(PositiveInt n) = float n
        static member ToFloat32(PositiveInt n) = float32 n

    // Arbitrary instance for PositiveInt
    type PositiveIntArbitrary() =
        static member PositiveInt() =
            Arb.fromGen (Gen.choose (1, 1000) |> Gen.map PositiveInt)

    // Property test helpers
    let forAllPointClouds (property: System.Numerics.Vector3 array -> bool) : bool =
        let gen = Gen.arrayOfLength 50 (Gen.choose (0, 100) |> Gen.map (fun n -> System.Numerics.Vector3(float32 n, float32 n, float32 n)))
        FsCheck.Check.QuickThrowOnFailure(Prop.forAll (Arb.fromGen gen) property)
        true

    let forAllSeeds (property: int -> bool) : bool =
        let gen = Gen.choose (0, 100000)
        FsCheck.Check.QuickThrowOnFailure(Prop.forAll (Arb.fromGen gen) property)
        true
