namespace ParallelTempering.Core

open System
open System.Numerics

/// Spatial hashing for efficient collision detection
module SpatialHash =

    /// Performance tuning config
    type Config = {
        BruteForceThreshold: int  // Switch to spatial hash above this
        MinDistanceEpsilon: float // Ignore distances below this
    }

    /// Initial guess - will be replaced by auto-calibration from actual measurements
    let defaultConfig = {
        BruteForceThreshold = 200  // Arbitrary starting point - system will measure and adjust
        MinDistanceEpsilon = 0.001
    }

    /// Adaptive threshold based on typical workload
    let mutable private config = defaultConfig

    /// Performance measurement tracking
    open System.Diagnostics
    open System.Collections.Concurrent

    type AlgorithmPerformance = {
        ProblemSize: int
        BruteForceTimeMs: float
        SpatialHashTimeMs: float
        Timestamp: System.DateTime
    }

    let private performanceHistory = ConcurrentQueue<AlgorithmPerformance>()
    let mutable private lastCalibration = System.DateTime.UtcNow

    /// Record performance for adaptive calibration
    let recordPerformance (problemSize: int) (bruteForceTime: float) (spatialHashTime: float) : unit =
        let measurement = {
            ProblemSize = problemSize
            BruteForceTimeMs = bruteForceTime
            SpatialHashTimeMs = spatialHashTime
            Timestamp = System.DateTime.UtcNow
        }
        performanceHistory.Enqueue(measurement)

        // Keep only last 100 measurements
        while performanceHistory.Count > 100 do
            performanceHistory.TryDequeue() |> ignore

    /// Find optimal threshold where spatial hash becomes faster
    let findOptimalBruteForceThreshold () : int option =
        let measurements = performanceHistory.ToArray()

        if measurements.Length < 10 then None
        else
            let grouped =
                measurements
                |> Array.groupBy (fun m -> m.ProblemSize)
                |> Array.map (fun (size, ms) ->
                    let avgBrute = ms |> Array.averageBy (fun m -> m.BruteForceTimeMs)
                    let avgSpatial = ms |> Array.averageBy (fun m -> m.SpatialHashTimeMs)
                    (size, avgBrute, avgSpatial)
                )
                |> Array.sortBy (fun (size, _, _) -> size)

            // Find crossover where spatial hash becomes faster (with 20% margin)
            grouped
            |> Array.tryFind (fun (_, brute, spatial) -> spatial * 1.2 < brute)
            |> Option.map (fun (size, _, _) -> size)

    /// Auto-calibrate threshold from measurements (runs daily)
    let autoCalibrate () : unit =
        let hoursSinceLastCalibration = (System.DateTime.UtcNow - lastCalibration).TotalHours

        if hoursSinceLastCalibration >= 24.0 && performanceHistory.Count >= 10 then
            match findOptimalBruteForceThreshold() with
            | Some optimalThreshold ->
                config <- { config with BruteForceThreshold = max 50 (min 1000 optimalThreshold) }
                lastCalibration <- System.DateTime.UtcNow
            | None -> ()

    /// Update performance config at runtime
    let updateConfig (newConfig: Config) : unit =
        config <- newConfig

    /// Brute force collision detection - O(n²)
    let private bruteForceCollisions (minDistance: float) (positions: Vector3 array) : (int * int) list =
        let n = positions.Length
        let mutable collisions = []

        for i in 0 .. n - 2 do
            for j in i + 1 .. n - 1 do
                let delta = positions.[j] - positions.[i]
                let dist = delta.Length() |> float

                if dist < minDistance && dist > config.MinDistanceEpsilon then
                    collisions <- (i, j) :: collisions

        collisions

    /// Spatial hash collision detection - O(27n) average case
    let private spatialHashCollisions (minDistance: float) (positions: Vector3 array) : (int * int) list =
        let n = positions.Length
        let mutable collisions = []

        // Spatial hashing for large systems
        // Cell size = minDistance ensures nearby pairs checked
        let cellSize = minDistance
        let grid = System.Collections.Generic.Dictionary<int*int*int, int list>()

        let getCell (pos: Vector3) =
            (int (float pos.X / cellSize),
             int (float pos.Y / cellSize),
             int (float pos.Z / cellSize))

        // Build spatial grid
        positions |> Array.iteri (fun i pos ->
            let cell = getCell pos
            if grid.ContainsKey(cell) then
                grid.[cell] <- i :: grid.[cell]
            else
                grid.[cell] <- [i]
        )

        // Check only neighboring cells
        for i in 0 .. n - 1 do
            let cell = getCell positions.[i]
            let (cx, cy, cz) = cell

            // Check 27 neighboring cells
            for dx in -1 .. 1 do
                for dy in -1 .. 1 do
                    for dz in -1 .. 1 do
                        let neighborCell = (cx + dx, cy + dy, cz + dz)
                        if grid.ContainsKey(neighborCell) then
                            for j in grid.[neighborCell] do
                                if i < j then
                                    let delta = positions.[j] - positions.[i]
                                    let dist = delta.Length() |> float

                                    if dist < minDistance && dist > config.MinDistanceEpsilon then
                                        collisions <- (i, j) :: collisions

        collisions

    /// Detect collisions using adaptive algorithm with auto-calibration
    /// O(n²) for n < threshold
    /// Spatial hashing for n ≥ threshold: O(27n) average, O(n²) worst case (all nodes in same cell)
    let detectCollisions (minDistance: float) (positions: Vector3 array) : (int * int) list =
        // Try auto-calibration
        autoCalibrate()

        let n = positions.Length

        // 5% of the time, measure both algorithms to keep calibration data fresh
        let rng = Random(System.Environment.TickCount)
        let shouldMeasureBoth = rng.Next(100) < 5

        if shouldMeasureBoth && n >= 50 && n <= 500 then
            // Measure both in the sweet spot where difference matters
            let sw = Stopwatch()

            sw.Restart()
            let bruteResult = bruteForceCollisions minDistance positions
            let bruteTime = sw.Elapsed.TotalMilliseconds

            sw.Restart()
            let spatialResult = spatialHashCollisions minDistance positions
            let spatialTime = sw.Elapsed.TotalMilliseconds

            recordPerformance n bruteTime spatialTime

            // Return the brute force result (both should be identical)
            bruteResult
        elif n < config.BruteForceThreshold then
            bruteForceCollisions minDistance positions
        else
            spatialHashCollisions minDistance positions

    /// Compute collision penalty for cost function
    /// Quadratic penalty increases as nodes get closer
    let collisionPenalty (minDistance: float) (positions: Vector3 array) : float =
        let collisions = detectCollisions minDistance positions
        collisions
        |> List.sumBy (fun (i, j) ->
            let dist = float (Vector3.Distance(positions.[i], positions.[j]))
            let overlap = minDistance - dist
            overlap * overlap * 100.0
        )
