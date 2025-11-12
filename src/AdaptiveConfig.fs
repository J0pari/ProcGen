namespace AdaptiveConfig

open System
open System.Numerics
open System.Threading
open System.Collections.Concurrent

/// GPU Configuration with device management and compute parameters
type GPUConfigData = {
    DeviceId: int
    MaxThreadsPerBlock: int
    SharedMemorySize: int
}

/// Physics simulation configuration with validation
type PhysicsConfigData = {
    Gravity: Vector3
    SafetyMargin: float
}

/// Spatial hash configuration for collision detection optimization
type SpatialHashConfigData = {
    CellSize: float
    MaxObjects: int
    LoadFactor: float
}

/// Thread-safe GPU configuration manager with validation
module GPUConfig =
    let private lockObj = obj()
    let mutable private current = {
        DeviceId = 0
        MaxThreadsPerBlock = 256
        SharedMemorySize = 48 * 1024
    }

    let get() =
        lock lockObj (fun () -> current)

    let update cfg =
        lock lockObj (fun () ->
            if cfg.DeviceId < 0 then
                raise (ArgumentException("DeviceId must be non-negative"))
            if cfg.MaxThreadsPerBlock <= 0 || cfg.MaxThreadsPerBlock > 1024 then
                raise (ArgumentException("MaxThreadsPerBlock must be between 1 and 1024"))
            if cfg.SharedMemorySize <= 0 then
                raise (ArgumentException("SharedMemorySize must be positive"))
            current <- cfg)

    let default' = { DeviceId = 0; MaxThreadsPerBlock = 256; SharedMemorySize = 48 * 1024 }

/// Thread-safe physics configuration manager with range validation
module PhysicsConfig =
    let private lockObj = obj()
    let mutable private current = {
        Gravity = Vector3(0.0f, -9.81f, 0.0f)
        SafetyMargin = 0.1
    }

    let get() =
        lock lockObj (fun () -> current)

    let update cfg =
        lock lockObj (fun () ->
            if cfg.SafetyMargin < 0.0 || cfg.SafetyMargin > 1.0 then
                raise (ArgumentException("SafetyMargin must be between 0 and 1"))
            if Single.IsNaN(cfg.Gravity.X) || Single.IsNaN(cfg.Gravity.Y) || Single.IsNaN(cfg.Gravity.Z) then
                raise (ArgumentException("Gravity components cannot be NaN"))
            current <- cfg)

    let default' = { Gravity = Vector3(0.0f, -9.81f, 0.0f); SafetyMargin = 0.1 }

/// Thread-safe spatial hash configuration manager
module SpatialHashConfig =
    let private lockObj = obj()
    let mutable private current = {
        CellSize = 1.0
        MaxObjects = 10000
        LoadFactor = 0.75
    }

    let get() =
        lock lockObj (fun () -> current)

    let update cfg =
        lock lockObj (fun () ->
            if cfg.CellSize <= 0.0 then
                raise (ArgumentException("CellSize must be positive"))
            if cfg.MaxObjects <= 0 then
                raise (ArgumentException("MaxObjects must be positive"))
            if cfg.LoadFactor <= 0.0 || cfg.LoadFactor > 1.0 then
                raise (ArgumentException("LoadFactor must be between 0 and 1"))
            current <- cfg)

    let default' = { CellSize = 1.0; MaxObjects = 10000; LoadFactor = 0.75 }

/// Test lifecycle management for configuration isolation
module Lifecycle =
    /// Execute action with configuration isolation - restores original config after
    let withConfig (getter: unit -> 'T) (setter: 'T -> unit) (action: 'T -> unit) =
        let original = getter()
        try
            action original
        finally
            setter original

    /// Execute action with multiple configuration subsystems isolated
    let withMultipleConfigs (configs: ((unit -> 'T) * ('T -> unit)) list) (action: 'T list -> unit) =
        let originals = configs |> List.map (fun (getter, _) -> getter())
        try
            action originals
        finally
            List.zip originals configs
            |> List.iter (fun (original, (_, setter)) -> setter original)

    /// Execute action with exception safety and cleanup
    let withCleanup (setup: unit -> 'T) (cleanup: 'T -> unit) (action: 'T -> unit) =
        let resource = setup()
        try
            action resource
        finally
            cleanup resource

    /// Execute action with timeout
    let withTimeout (timeoutMs: int) (action: unit -> 'T) : 'T option =
        let cts = new CancellationTokenSource(timeoutMs)
        let task = System.Threading.Tasks.Task.Run(action, cts.Token)
        try
            if task.Wait(timeoutMs) then Some task.Result
            else None
        with
        | :? OperationCanceledException -> None

/// Performance-based adaptive threshold learning
module AdaptiveThresholds =

    /// Detect computational complexity from measurements using linear regression
    let detectComplexity (measurements: (int * float) list) : string * float =
        if measurements.Length < 2 then
            raise (InvalidOperationException("Need at least 2 measurements for complexity detection"))

        let n = float measurements.Length
        let xs = measurements |> List.map (fst >> float)
        let ys = measurements |> List.map snd

        let meanX = (List.sum xs) / n
        let meanY = (List.sum ys) / n

        let ssXY = List.zip xs ys |> List.sumBy (fun (x, y) -> (x - meanX) * (y - meanY))
        let ssXX = xs |> List.sumBy (fun x -> (x - meanX) * (x - meanX))
        let ssYY = ys |> List.sumBy (fun y -> (y - meanY) * (y - meanY))

        let r2 = if ssXX > 0.0 && ssYY > 0.0 then
                    let r = ssXY / sqrt(ssXX * ssYY)
                    r * r
                 else 0.0

        // Classify complexity based on slope
        let slope = if ssXX > 0.0 then ssXY / ssXX else 0.0
        let complexity =
            if slope < 1.0 then "O(1)"
            elif slope < 2.0 then "O(log n)"
            elif slope < float n then "O(n)"
            else "O(n^2)"

        (complexity, r2)

    /// Find performance crossover point between two algorithms
    let findCrossover (measurements: (int * float) list) (expectedPoint: int) : int option =
        if measurements.Length < 2 then
            raise (InvalidOperationException("Need at least 2 measurements to find crossover"))

        measurements
        |> List.pairwise
        |> List.tryFindIndex (fun ((n1, t1), (n2, t2)) ->
            // Detect where second derivative changes sign (inflection point)
            let slope1 = if n1 > 0 then t1 / float n1 else 0.0
            let slope2 = if n2 > 0 then t2 / float n2 else 0.0
            abs(slope2 - slope1) > 0.1 && n2 >= expectedPoint)
        |> Option.map (fun idx -> fst measurements.[idx + 1])

    /// Learn optimal threshold from performance profile
    let learnThreshold (measurements: (int * float) list) (targetComplexity: string) : int =
        match findCrossover measurements 1 with
        | Some crossover -> crossover
        | None ->
            // Fallback: use median input size
            let sizes = measurements |> List.map fst |> List.sort
            sizes.[sizes.Length / 2]

    /// Adaptive batch size optimization
    let optimizeBatchSize (initialSize: int) (performanceHistory: float list) : int =
        if performanceHistory.Length < 3 then initialSize
        else
            let recent = performanceHistory |> List.skip (performanceHistory.Length - 3)
            let avgRecent = (List.sum recent) / 3.0
            let trend = recent.[2] - recent.[0]

            if trend < 0.0 && avgRecent > 0.0 then
                // Performance improving, increase batch size
                min (initialSize * 2) 10000
            elif trend > 0.0 then
                // Performance degrading, decrease batch size
                max (initialSize / 2) 1
            else
                initialSize

/// Configuration persistence and versioning
module ConfigPersistence =

    type ConfigSnapshot = {
        Timestamp: DateTime
        GPUConfig: GPUConfigData
        PhysicsConfig: PhysicsConfigData
        SpatialHashConfig: SpatialHashConfigData
    }

    let private history = ConcurrentQueue<ConfigSnapshot>()
    let private maxHistorySize = 100

    /// Capture current configuration state
    let snapshot() : ConfigSnapshot =
        {
            Timestamp = DateTime.UtcNow
            GPUConfig = GPUConfig.get()
            PhysicsConfig = PhysicsConfig.get()
            SpatialHashConfig = SpatialHashConfig.get()
        }

    /// Save current configuration to history
    let save() =
        let snap = snapshot()
        history.Enqueue(snap)
        while history.Count > maxHistorySize do
            history.TryDequeue() |> ignore

    /// Restore configuration from snapshot
    let restore (snap: ConfigSnapshot) =
        GPUConfig.update snap.GPUConfig
        PhysicsConfig.update snap.PhysicsConfig
        SpatialHashConfig.update snap.SpatialHashConfig

    /// Get configuration history
    let getHistory() : ConfigSnapshot list =
        history.ToArray() |> Array.toList

    /// Rollback to previous configuration
    let rollback() : bool =
        if history.Count > 0 then
            let mutable previous = Unchecked.defaultof<ConfigSnapshot>
            if history.TryDequeue(&previous) then
                restore previous
                true
            else false
        else false
