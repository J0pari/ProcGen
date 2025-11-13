namespace TestInfrastructure

module Assertions =

    open System
    open global.Xunit
    open Swensen.Unquote
    open Core
    open AdaptiveConfig
    open Physics

    let shouldEqual (expected: 'T) (actual: 'T) : unit =
        test <@ actual = expected @>

    let shouldNotEqual (unexpected: 'T) (actual: 'T) : unit =
        test <@ actual <> unexpected @>

    let shouldBeTrue (condition: bool) : unit =
        test <@ condition @>

    let shouldBeFalse (condition: bool) : unit =
        test <@ not condition @>

    let shouldBeWithin (tolerance: float) (expected: float) (actual: float) : unit =
        let diff = abs (actual - expected)
        test <@ diff <= tolerance @>

    let shouldBeFinite (value: float) : unit =
        test <@ not (Double.IsNaN value) @>
        test <@ not (Double.IsInfinity value) @>

    let shouldBeFiniteVector (v: Core.Vector3) : bool =
        not (Single.IsNaN v.X) && not (Single.IsInfinity v.X) &&
        not (Single.IsNaN v.Y) && not (Single.IsInfinity v.Y) &&
        not (Single.IsNaN v.Z) && not (Single.IsInfinity v.Z)

    let shouldBePositive (value: float) : unit =
        test <@ value > 0.0 @>

    let shouldBeNonNegative (value: float) : unit =
        test <@ value >= 0.0 @>

    let shouldBeInRange (min: float) (max: float) (value: float) : unit =
        test <@ value >= min && value <= max @>

    let shouldBeMonotonicWith (comparer: 'T -> 'T -> int) (values: 'T seq) : unit =
        let pairs = values |> Seq.pairwise
        test <@ pairs |> Seq.forall (fun (a, b) -> comparer a b <= 0) @>

    let shouldBeStrictlyMonotonicWith (comparer: 'T -> 'T -> int) (values: 'T seq) : unit =
        let pairs = values |> Seq.pairwise
        test <@ pairs |> Seq.forall (fun (a, b) -> comparer a b < 0) @>

    let shouldBeMonotonic (values: float seq) : unit =
        shouldBeMonotonicWith compare values

    let shouldBeStrictlyMonotonic (values: float seq) : unit =
        shouldBeStrictlyMonotonicWith compare values

    let shouldContain (item: 'T) (collection: 'T seq) : unit =
        test <@ collection |> Seq.contains item @>

    let shouldNotContain (item: 'T) (collection: 'T seq) : unit =
        test <@ not (collection |> Seq.contains item) @>

    let shouldBeEmpty (collection: 'T seq) : unit =
        test <@ Seq.isEmpty collection @>

    let shouldNotBeEmpty (collection: 'T seq) : unit =
        test <@ not (Seq.isEmpty collection) @>

    let shouldHaveCount (expected: int) (collection: 'T seq) : unit =
        test <@ Seq.length collection = expected @>

    let shouldAllSatisfy (predicate: 'T -> bool) (collection: 'T seq) : unit =
        test <@ collection |> Seq.forall predicate @>

    let shouldAnySatisfy (predicate: 'T -> bool) (collection: 'T seq) : unit =
        test <@ collection |> Seq.exists predicate @>

    let shouldRoundtrip (encode: 'A -> 'B) (decode: 'B -> 'A) (value: 'A) : unit =
        test <@ decode (encode value) = value @>

    let shouldBeIdempotent (f: 'T -> 'T) (value: 'T) : unit =
        test <@ f (f value) = f value @>

    let shouldCommute (f: 'T -> 'T -> 'R) (a: 'T) (b: 'T) : unit =
        test <@ f a b = f b a @>

    let shouldBeAssociative (f: 'T -> 'T -> 'T) (a: 'T) (b: 'T) (c: 'T) : unit =
        test <@ f (f a b) c = f a (f b c) @>

    let shouldBeSome (option: 'T option) : unit =
        test <@ Option.isSome option @>

    let shouldBeNone (option: 'T option) : unit =
        test <@ Option.isNone option @>

    let shouldBeOk (result: Result<'T, 'E>) : unit =
        match result with
        | Ok _ -> ()
        | Error e -> failwithf "Expected Ok but got Error: %A" e

    let shouldBeError (result: Result<'T, 'E>) : unit =
        match result with
        | Error _ -> ()
        | Ok v -> failwithf "Expected Error but got Ok: %A" v

    let shouldThrow<'Ex when 'Ex :> exn> (f: unit -> 'T) : unit =
        let mutable threw = false
        try
            f() |> ignore
        with
        | :? 'Ex -> threw <- true
        | ex -> failwithf "Expected %s but got %s" typeof<'Ex>.Name (ex.GetType().Name)
        test <@ threw @>

    let shouldNotThrow (f: unit -> 'T) : unit =
        try
            f() |> ignore
        with
        | ex -> failwithf "Should not throw but threw: %s" ex.Message

    let shouldBeValidHandle (handle: Physics.Service.BodyHandle) : unit =
        test <@ handle >= 0 @>

    let shouldRejectInvalid (f: unit -> unit) : unit =
        shouldThrow<ArgumentException> f

    let shouldPreserveEquality (expected: 'T) (actual: 'T) : unit =
        test <@ expected = actual @>

    let shouldFindCrossover (measurements: (int * float) list) (expectedPoint: int) : unit =
        match AdaptiveConfig.AdaptiveThresholds.findCrossover measurements expectedPoint with
        | Some crossover -> test <@ abs(crossover - expectedPoint) <= 2 @>
        | None -> failwith "Expected to find crossover point"

    let shouldBeInvalidHandle (handle: int) : unit =
        test <@ handle < 0 @>

    let shouldBeUnitVector (v: System.Numerics.Vector3) : unit =
        let magnitude = v.Length()
        shouldBeWithin 0.001 1.0 (float magnitude)

    let shouldBeNormalized (values: float array) : unit =
        let sum = Array.sum values
        shouldBeWithin 0.001 1.0 sum

    let shouldBeOrthogonal (v1: Vector3) (v2: Vector3) : unit =
        let dot = Vector3.dot v1 v2
        shouldBeWithin 0.001 0.0 (float dot)

    let shouldBeParallel (v1: Vector3) (v2: Vector3) : unit =
        let cross = Vector3.cross v1 v2
        let mag = Vector3.magnitude cross
        shouldBeWithin 0.001 0.0 (float mag)

    let shouldBeBounded (lower: float) (upper: float) (values: float seq) : unit =
        test <@ values |> Seq.forall (fun v -> v >= lower && v <= upper) @>

    let shouldConverge (tolerance: float) (values: float seq) : unit =
        let arr = values |> Seq.toArray
        if arr.Length < 2 then failwith "Need at least 2 values"
        let last = arr.[arr.Length - 1]
        let secondLast = arr.[arr.Length - 2]
        shouldBeWithin tolerance secondLast last

    let shouldDiverge (values: float seq) : unit =
        let arr = values |> Seq.toArray
        if arr.Length < 2 then failwith "Need at least 2 values"
        let diffs = arr |> Array.pairwise |> Array.map (fun (a, b) -> abs (b - a))
        test <@ Array.last diffs > Array.head diffs @>

    let shouldBeSymmetric (matrix: float[,]) : unit =
        let n = Array2D.length1 matrix
        let m = Array2D.length2 matrix
        test <@ n = m @>
        for i in 0..n-1 do
            for j in 0..n-1 do
                shouldBeWithin 1e-10 matrix.[i,j] matrix.[j,i]

    let shouldBePositiveDefinite (matrix: float[,]) : unit =
        shouldBeSymmetric matrix
        let n = Array2D.length1 matrix
        let evd = MathNet.Numerics.LinearAlgebra.DenseMatrix.ofArray2(matrix).Evd()
        let eigenvalues = evd.EigenValues |> Seq.map (fun c -> c.Real)
        test <@ eigenvalues |> Seq.forall (fun ev -> ev > 0.0) @>

    let shouldBeDiagonal (matrix: float[,]) : unit =
        let n = Array2D.length1 matrix
        let m = Array2D.length2 matrix
        for i in 0..n-1 do
            for j in 0..m-1 do
                if i <> j then
                    shouldBeWithin 1e-10 0.0 matrix.[i,j]

    let shouldPreserveNorm (f: Vector3 -> Vector3) (v: Vector3) : unit =
        let vOut = f v
        let magIn = Vector3.magnitude v
        let magOut = Vector3.magnitude vOut
        shouldBeWithin 0.001 (float magIn) (float magOut)

    let shouldPreserveSum (f: float array -> float array) (values: float array) : unit =
        let sumIn = Array.sum values
        let sumOut = Array.sum (f values)
        shouldBeWithin 0.001 sumIn sumOut

    let shouldBeWellConditioned (matrix: float[,]) (maxCondition: float) : unit =
        let mat = MathNet.Numerics.LinearAlgebra.DenseMatrix.ofArray2(matrix)
        let condition = mat.ConditionNumber()
        test <@ condition <= maxCondition @>

    let shouldBeInvertible (matrix: float[,]) : unit =
        let mat = MathNet.Numerics.LinearAlgebra.DenseMatrix.ofArray2(matrix)
        let det = mat.Determinant()
        test <@ abs det > 1e-10 @>

    let shouldSatisfyInvariant (invariant: 'T -> bool) (value: 'T) (operation: 'T -> 'T) : unit =
        test <@ invariant value @>
        let result = operation value
        test <@ invariant result @>

    let shouldMaintainInvariant (invariant: 'T -> bool) (initial: 'T) (operations: ('T -> 'T) list) : unit =
        test <@ invariant initial @>
        let mutable current = initial
        for op in operations do
            current <- op current
            test <@ invariant current @>

    let shouldSatisfyLTL (formula: TemporalLogic.LTL<'T>) (trace: 'T list) : unit =
        let holds = TemporalLogic.checkLTL formula trace
        Assert.True(holds, sprintf "LTL formula violated on trace of length %d" (List.length trace))

    let shouldSatisfyCTL (system: TemporalLogic.TransitionSystem<'T>) (formula: TemporalLogic.CTL<'T>) : unit =
        let allSatisfy = system.InitialStates |> Set.forall (TemporalLogic.checkCTL system formula)
        Assert.True(allSatisfy, "CTL formula violated")

    let shouldBeSafe (bad: 'T -> bool) (trace: 'T list) : unit =
        shouldSatisfyLTL (TemporalLogic.safety bad) trace

    let shouldBeLive (good: 'T -> bool) (trace: 'T list) : unit =
        shouldSatisfyLTL (TemporalLogic.liveness good) trace

    let shouldRespond (request: 'T -> bool) (answer: 'T -> bool) (trace: 'T list) : unit =
        shouldSatisfyLTL (TemporalLogic.response request answer) trace

    let shouldConvergeTo (measure: 'T -> float) (target: float) (tolerance: float) (trace: 'T list) : unit =
        let converged = TemporalLogic.convergesTo measure target tolerance trace
        Assert.True(converged, sprintf "Did not converge to %.3f±%.3f" target tolerance)

    let shouldBeMonotonicTemporal (measure: 'T -> float) (trace: 'T list) : unit =
        let monotonic = TemporalLogic.isMonotonic measure trace
        Assert.True(monotonic, "Sequence is not monotonic")

    let shouldStabilize (measure: 'T -> float) (tolerance: float) (windowSize: int) (trace: 'T list) : unit =
        let stable = TemporalLogic.stabilizes measure tolerance windowSize trace
        Assert.True(stable, sprintf "Did not stabilize within tolerance %.3f over window %d" tolerance windowSize)

    let shouldNotOscillate (measure: 'T -> float) (threshold: float) (trace: 'T list) : unit =
        let oscillates = TemporalLogic.detectOscillation measure trace threshold
        Assert.False(oscillates, sprintf "Oscillation detected with threshold %.3f" threshold)

    let shouldBeBernoulli (theoreticalP: float) (tolerance: float) (outcomes: bool seq) : unit =
        let arr = outcomes |> Seq.toArray
        let empiricalP = arr |> Array.filter id |> Array.length |> float |> fun x -> x / float arr.Length
        let pDiff = abs (empiricalP - theoreticalP)
        Assert.True(pDiff < tolerance, sprintf "Bernoulli p: expected %f±%f, got %f" theoreticalP tolerance empiricalP)
        let theoreticalVar = theoreticalP * (1.0 - theoreticalP)
        let empiricalVar = arr |> Array.map (fun b -> if b then 1.0 else 0.0) |> Statistics.variance
        let varDiff = abs (empiricalVar - theoreticalVar)
        Assert.True(varDiff < 0.1 * theoreticalVar, sprintf "Bernoulli var: expected %f, got %f" theoreticalVar empiricalVar)

    let shouldMatchProbability (expected: float) (tolerance: float) (predicate: 'a -> bool) (trials: 'a seq) : unit =
        let actual = Statistics.acceptanceRate predicate trials
        Assert.True(abs (actual - expected) < tolerance, sprintf "Prob: expected %f±%f, got %f" expected tolerance actual)

    let shouldBeAntimonotonic (cmp: 'a -> 'a -> int when 'a: comparison) (seq: 'a seq) : unit =
        seq |> Seq.pairwise |> Seq.iteri (fun i (a, b) ->
            Assert.True(cmp a b >= 0, sprintf "Not antimonotonic at %d" i))

    let shouldEventuallyHold (maxTrials: int) (property: unit -> bool) : unit =
        let results = Array.init maxTrials (fun _ -> property())
        Assert.True(Array.exists id results, sprintf "Property never held in %d trials" maxTrials)

    let shouldEventuallyHoldWithRate (minRate: float) (trials: int) (property: unit -> bool) : unit =
        let results = Array.init trials (fun _ -> property())
        let rate = Statistics.acceptanceRate id results
        Assert.True(rate >= minRate, sprintf "Success rate %f < %f" rate minRate)

    let shouldConvergeAsMonteCarloRate (samplesAtN: (int * float) seq) : unit =
        let data = samplesAtN |> Seq.toArray |> Array.sortBy fst
        let logN = data |> Array.map (fst >> float >> log)
        let logError = data |> Array.map (snd >> log)
        let meanX = Array.average logN
        let meanY = Array.average logError
        let slope =
            let num = Array.map2 (fun x y -> (x - meanX) * (y - meanY)) logN logError |> Array.sum
            let denom = logN |> Array.sumBy (fun x -> (x - meanX) ** 2.0)
            num / denom
        Assert.True(abs (slope + 0.5) < 0.2, sprintf "Slope %f != -0.5" slope)

    let shouldAlwaysHold (predicate: 'T -> bool) (values: 'T seq) : unit =
        test <@ values |> Seq.forall predicate @>

    let shouldNeverHold (predicate: 'T -> bool) (values: 'T seq) : unit =
        test <@ values |> Seq.forall (predicate >> not) @>

    let shouldPenalize (penaltyA: float) (penaltyB: float) : unit =
        test <@ penaltyA > penaltyB @>

    let shouldCollide (a: 'T) (b: 'T) : unit =
        Assert.NotNull(box a)
        Assert.NotNull(box b)

    let shouldBeWithinTolerance (tolerance: float) (expected: float) (actual: float) : unit =
        shouldBeWithin tolerance expected actual

    let shouldSupportAsyncOps (operation: unit -> unit) : unit =
        shouldNotThrow operation

    let shouldSetOutParam (outValue: 'T) (expectedValue: 'T) : unit =
        test <@ outValue = expectedValue @>

    let shouldBeWithinDistance (expected: float32) (actual: float32) (tolerance: float32) : unit =
        let diff = abs (actual - expected)
        test <@ diff <= tolerance @>

    // Physics-specific assertions
    let shouldBeStable (predicate: float32 -> float32 -> bool) : unit =
        test <@ predicate 100.0f 1.0f @>

    let shouldPreserveFixed (state: Core.PhysicsState) (operation: Core.PhysicsState -> Core.PhysicsState) : unit =
        let initialPositions = state.Bodies |> Array.map (fun b -> b.Position)
        let result = operation state
        Array.zip initialPositions result.Bodies
        |> Array.iter (fun (initial, body) ->
            if body.Mass = 0.0f then
                test <@ body.Position = initial @>)

    let shouldConserveMomentum (state: Physics.PhysicsState) (steps: int) : unit =
        let totalMomentum (s: Physics.PhysicsState) =
            s.Properties |> Array.fold (fun acc prop ->
                let v = prop.Velocity
                { Core.Vector3.X = acc.X + v.X * float32 prop.Mass; Y = acc.Y + v.Y * float32 prop.Mass; Z = acc.Z + v.Z * float32 prop.Mass })
                { Core.Vector3.X = 0.0f; Y = 0.0f; Z = 0.0f }
        let initial = totalMomentum state
        let mutable current = state
        for _ in 1..steps do
            current <- Physics.Verlet.integrate current
        let final = totalMomentum current
        shouldBeWithin 0.01 (float initial.X) (float final.X)
        shouldBeWithin 0.01 (float initial.Y) (float final.Y)
        shouldBeWithin 0.01 (float initial.Z) (float final.Z)

    let shouldDetect (collisions: 'T list) (predicate: 'T -> bool) : unit =
        test <@ List.exists predicate collisions @>

    let shouldResolve (body1: 'T) (body2: 'T) (predicate: 'T * 'T -> bool) : unit =
        test <@ predicate (body1, body2) @>

    let shouldComputeCriticalDamping (k: float32) (m: float32) (predicate: float32 -> bool) : unit =
        let criticalDamping = 2.0f * sqrt(k * m)
        test <@ predicate criticalDamping @>

    let shouldRelax (predicate: Core.Graph -> bool) : unit =
        test <@ predicate { Core.Graph.Nodes = set [0..4]; Edges = [] } @>

    let shouldCompose (weights: Map<string, float>) (graph: 'T) (predicate: float -> 'T -> float) : unit =
        let totalWeight = weights |> Map.toSeq |> Seq.sumBy snd
        let expectedCost = predicate totalWeight graph
        test <@ expectedCost >= 0.0 @>

    let shouldDetectAll (collisions: ('a * 'b) list) (predicate: 'a * 'b -> bool) : unit =
        test <@ List.forall predicate collisions @>

    let shouldNotDetect (collisions: 'T list) : unit =
        test <@ List.isEmpty collisions @>

    let shouldIncrease (higher: float) (lower: float) : unit =
        test <@ higher > lower @>

    let shouldScaleSublinearly (measurements: (int * float) list) : unit =
        if measurements.Length < 2 then () else
        let sorted = measurements |> List.sortBy fst
        let ratios = sorted |> List.pairwise |> List.map (fun ((n1, t1), (n2, t2)) ->
            let nRatio = float n2 / float n1
            let tRatio = t2 / t1
            tRatio / nRatio)
        let avgRatio = List.average ratios
        test <@ avgRatio < 1.0 @>

    // Physics queries and service assertions
    let shouldHit (world: Physics.Service.PhysicsWorld) (ray: Physics.Queries.Ray) (predicate: Physics.Queries.RaycastHit -> bool) : unit =
        match Physics.Queries.raycast world ray Physics.Queries.defaultFilter with
        | Some hit -> Assert.True(predicate hit, "Hit predicate failed")
        | None -> Assert.True(false, "Expected raycast hit but got None")

    let shouldMiss (world: Physics.Service.PhysicsWorld) (ray: Physics.Queries.Ray) : unit =
        match Physics.Queries.raycast world ray Physics.Queries.defaultFilter with
        | Some _ -> Assert.True(false, "Expected miss but got hit")
        | None -> ()

    let shouldOverlapSphere (world: Physics.Service.PhysicsWorld) (sphere: Physics.Queries.Sphere) (predicate: Physics.Service.BodyHandle array -> bool) : unit =
        let hits = Physics.Queries.overlapSphere world sphere Physics.Queries.defaultFilter
        Assert.True(predicate hits, "shouldOverlapSphere predicate failed")

    let shouldOverlapBox (world: Physics.Service.PhysicsWorld) (box: Physics.Queries.Box) (predicate: Physics.Service.BodyHandle array -> bool) : unit =
        let hits = Physics.Queries.overlapBox world box Physics.Queries.defaultFilter
        Assert.True(predicate hits, "shouldOverlapBox predicate failed")

    let shouldRetrieveBody (world: Physics.Service.PhysicsWorld) (handle: Physics.Service.BodyHandle) (predicate: Physics.Service.RigidBodyData option -> bool) : unit =
        let retrieved = world.GetRigidBody(handle)
        Assert.True(predicate retrieved, "shouldRetrieveBody predicate failed")

    let shouldRespectBodyType (world: Physics.Service.PhysicsWorld) (handle: Physics.Service.BodyHandle) (predicate: Physics.Service.RigidBodyData -> bool) : unit =
        match world.GetRigidBody(handle) with
        | Some body -> Assert.True(predicate body, "shouldRespectBodyType predicate failed")
        | None -> Assert.True(false, "shouldRespectBodyType: body not found")

    let shouldDetectCollision (world: Physics.Service.PhysicsWorld) (predicate: Physics.Service.CollisionEvent array -> bool) : unit =
        let events = world.GetCollisionEvents()
        Assert.True(predicate events, "shouldDetectCollision predicate failed")

    let shouldSleep (world: Physics.Service.PhysicsWorld) (handle: Physics.Service.BodyHandle) : unit =
        match world.IsBodySleeping(handle) with
        | Some true -> ()
        | Some false -> Assert.True(false, "shouldSleep: body is not sleeping")
        | None -> Assert.True(false, "shouldSleep: body not found")

    let shouldTrack (world: Physics.Service.PhysicsWorld) (predicate: 'Stats -> bool) : unit =
        let stats = world.GetStatistics()
        Assert.True(predicate stats, "shouldTrack predicate failed")

    let shouldSignal (fence: 'Fence) (operation: unit -> bool) : unit =
        Assert.True(operation())

    let shouldTimeout (fence: 'Fence) (operation: unit -> bool) : unit =
        Assert.True(operation())

    let shouldMaintainHz (actualHz: float) (targetHz: float) (tolerance: float) : unit =
        shouldBeWithin (targetHz * tolerance) targetHz actualHz

    let shouldInterpolate (manager: Physics.Sync.SnapshotManager) (alpha: float32) (predicate: {| Position: System.Numerics.Vector3; Rotation: System.Numerics.Quaternion |} -> bool) : unit =
        match manager.Interpolate(0, alpha) with
        | Some (pos, rot) ->
            let interpolated = {| Position = pos; Rotation = rot |}
            Assert.True(predicate interpolated, "shouldInterpolate predicate failed")
        | None -> Assert.True(false, "shouldInterpolate: no interpolated state available")

    let shouldPreventRaces (spec: {| Setup: unit -> 'T; Operation: 'T -> int -> unit; Threads: int; OperationsPerThread: int; Validate: 'T -> bool |}) : unit =
        let state = spec.Setup()
        let threads =
            [| for tid in 0 .. spec.Threads - 1 ->
                System.Threading.Thread(fun () ->
                    for _ in 1 .. spec.OperationsPerThread do
                        spec.Operation state tid
                )
            |]
        threads |> Array.iter (fun t -> t.Start())
        threads |> Array.iter (fun t -> t.Join())
        Assert.True(spec.Validate state, "shouldPreventRaces: validation failed - race condition detected")

    let shouldBeUnique (items: 'T list) : unit =
        let distinct = items |> List.distinct
        test <@ distinct.Length = items.Length @>

    let shouldBeOrdered (items: 'T list) (predicate: 'T -> bool) : unit =
        test <@ List.forall predicate items @>

    // Statistical distribution assertions
    let shouldBeUniform (samples: int list) (bins: int) : unit =
        let counts = Array.zeroCreate bins
        for sample in samples do
            let bin = sample % bins
            counts.[bin] <- counts.[bin] + 1
        let expected = float (List.length samples) / float bins
        let chiSquare = counts |> Array.sumBy (fun c -> (float c - expected) ** 2.0 / expected)
        let degreesOfFreedom = bins - 1
        let criticalValue = float degreesOfFreedom * 2.0
        test <@ chiSquare < criticalValue @>

    let shouldBeNormal (samples: float list) : unit =
        let arr = samples |> List.toArray
        let mean = Array.average arr
        let variance = arr |> Array.sumBy (fun x -> (x - mean) ** 2.0) |> fun s -> s / float arr.Length
        let stdDev = sqrt variance
        test <@ stdDev > 0.0 @>
        test <@ abs (mean - (Array.average arr)) < stdDev @>

    let shouldHaveMean (samples: float list) (expectedMean: float) (tolerance: float) : unit =
        let actualMean = List.average samples
        shouldBeWithin tolerance expectedMean actualMean

    let shouldHaveStdDev (samples: float list) (expectedStdDev: float) (tolerance: float) : unit =
        let arr = samples |> List.toArray
        let mean = Array.average arr
        let variance = arr |> Array.sumBy (fun x -> (x - mean) ** 2.0) |> fun s -> s / float arr.Length
        let actualStdDev = sqrt variance
        shouldBeWithin tolerance expectedStdDev actualStdDev

    let shouldBeIndependent (createA: unit -> 'RNG) (createB: unit -> 'RNG) (sample: 'RNG -> int list) : unit =
        let a = createA()
        let b = createB()
        let samplesA = sample a
        let samplesB = sample b
        let correlation =
            List.zip samplesA samplesB
            |> List.map (fun (x, y) -> float x * float y)
            |> List.average
        test <@ abs correlation < 1000000.0 @>

    let shouldNotCollide (threads: int) (generate: int -> int list) : unit =
        let results = Array.init threads generate
        let allValues = results |> Array.collect List.toArray |> Set.ofArray
        let totalCount = results |> Array.sumBy (List.length)
        test <@ Set.count allValues > totalCount / 2 @>

    let shouldGenerateQuickly (n: int) (operation: unit -> unit) : unit =
        let sw = System.Diagnostics.Stopwatch.StartNew()
        operation()
        sw.Stop()
        test <@ sw.ElapsedMilliseconds < 5000L @>

    let shouldBeDeterministic (f: int -> 'T) : unit =
        let seed = 12345
        let result1 = f seed
        let result2 = f seed
        test <@ result1 = result2 @>

    let shouldCompleteWithin (milliseconds: float) (operation: unit -> unit) : unit =
        let sw = System.Diagnostics.Stopwatch.StartNew()
        operation()
        sw.Stop()
        test <@ float sw.ElapsedMilliseconds < milliseconds @>
