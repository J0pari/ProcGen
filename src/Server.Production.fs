namespace Server.Production

open System
open System.Threading
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Giraffe
open ProceduralGeneration
open ParallelTempering.Core
open Cache
open AsyncGeneration
open Observability

module Validation =

    type ValidationRule<'T> = {
        Predicate: 'T -> bool
        ErrorMessage: string
    }

    let inline validate (rules: ValidationRule<'T> list) (value: 'T) : Result<'T, string> =
        rules
        |> List.tryFind (fun rule -> not (rule.Predicate value))
        |> Option.map (fun rule -> Error rule.ErrorMessage)
        |> Option.defaultValue (Ok value)

    let validateNodeCount (n: int option) : Result<int, string> =
        let value = n |> Option.defaultValue 20
        validate [
            { Predicate = (fun c -> c >= 2); ErrorMessage = "NodeCount must be at least 2" }
            { Predicate = (fun c -> c <= 1000); ErrorMessage = "NodeCount cannot exceed 1000" }
        ] value

    let validateIterations (i: int option) : Result<int, string> =
        let value = i |> Option.defaultValue 500
        validate [
            { Predicate = (fun iter -> iter >= 10); ErrorMessage = "Iterations must be at least 10" }
            { Predicate = (fun iter -> iter <= 10000); ErrorMessage = "Iterations cannot exceed 10000" }
        ] value

    let validateSeed (s: string option) : Result<string option, string> =
        match s with
        | None -> Ok None
        | Some seed ->
            validate [
                { Predicate = (not << String.IsNullOrWhiteSpace); ErrorMessage = "Seed cannot be empty" }
                { Predicate = (fun s -> s.Length <= 256); ErrorMessage = "Seed cannot exceed 256 characters" }
            ] seed
            |> Result.map Some

module ProductionHandlers =
    open System.Text.Json
    open Deterministic
    open Physics
    open Validation

    type GenerationRequest = {
        NodeCount: int option
        Iterations: int option
        ConstraintWeights: Map<string, float> option
        Seed: string option
        Async: bool option
    }

    type CacheState = {
        Capacity: int
        Cache: LRUCache<string, Export.SerializedGraph>
        CancellationTokenSource: CancellationTokenSource
    }

    let mutable private state = {
        Capacity = 100
        Cache = LRUCache<string, Export.SerializedGraph>(100)
        CancellationTokenSource = new CancellationTokenSource()
    }

    let private stateLock = obj()

    /// Update server cache capacity with atomic state transition
    let updateCacheCapacity (newCapacity: int) : unit =
        if newCapacity > 0 && newCapacity <> state.Capacity then
            lock stateLock (fun () ->
                let newCache = LRUCache<string, Export.SerializedGraph>(newCapacity)
                state <- { state with Capacity = newCapacity; Cache = newCache }
            )

    let private errorResponse (statusCode: int) (message: string) : HttpHandler =
        setStatusCode statusCode >=> json {| Error = message |}

    let private badRequest msg = errorResponse 400 msg
    
    /// POST /api/graphs - with caching, async, metrics, validation
    let createGraph : HttpHandler =
        fun next ctx ->
            task {
                Metrics.counter "api.graphs.requests" None

                let! request = ctx.BindJsonAsync<GenerationRequest>()

                // Validate inputs with applicative composition
                let validationResult =
                    match validateNodeCount request.NodeCount with
                    | Error msg -> Error msg
                    | Ok nodeCount ->
                        match validateIterations request.Iterations with
                        | Error msg -> Error msg
                        | Ok iterations ->
                            match validateSeed request.Seed with
                            | Error msg -> Error msg
                            | Ok seed -> Ok (nodeCount, iterations, seed)

                match validationResult with
                | Error msg ->
                    Metrics.counter "api.graphs.validation_error" None
                    return! badRequest msg next ctx
                | Ok (nodeCount, iterations, seed) ->

                let useAsync = request.Async |> Option.defaultValue false

                let config = {
                    Generator.defaultConfig with
                        NodeCount = nodeCount
                        Iterations = iterations
                        ConstraintWeights =
                            request.ConstraintWeights
                            |> Option.defaultValue Generator.defaultConfig.ConstraintWeights
                }

                // Check cache first
                let cacheKey = LRUCache<_,_>.HashKey(
                    nodeCount,
                    iterations,
                    seed,
                    config.ConstraintWeights
                )

                let deserializeGraph (cached: Export.SerializedGraph) : SpatialGraph<NodeType> =
                    let nodes = cached.Nodes |> Array.map (fun n ->
                        match n.NodeType with
                        | "origin" -> Origin
                        | "terminal" -> Terminal
                        | _ -> Standard n.Value
                    )
                    let positions = cached.Nodes |> Array.map (fun n ->
                        GPU.Marshaling.floatTripleToVector3 n.Position
                    )
                    let edges = cached.Edges |> Array.map (fun e -> (e.From, e.To, e.Weight)) |> Array.toList
                    { Nodes = nodes; Positions = positions; Edges = edges }

                let! graph =
                    match state.Cache.TryGet(cacheKey) with
                    | Some cachedGraph ->
                        Metrics.counter "api.graphs.cache_hit" None
                        task { return deserializeGraph cachedGraph }
                    | None ->
                        Metrics.counter "api.graphs.cache_miss" None

                        if useAsync then
                            // Async generation with progress tracking
                            let progress (info: ProgressInfo) =
                                Metrics.gauge "generation.progress" (float info.Step / float info.TotalSteps) None
                                Metrics.gauge "generation.cost" info.BestCost None
                                System.Threading.Tasks.Task.CompletedTask

                            task {
                                let! result = AsyncGenerator.generateWithProgress config progress state.CancellationTokenSource.Token
                                let serialized = Export.serialize result
                                let cost = EnvironmentConstraints.build config.ConstraintWeights result
                                state.Cache.Set(cacheKey, serialized, cost)
                                return result
                            }
                        else
                            // Synchronous generation with telemetry
                            task {
                                let result = Instrumentation.timed "generation.sync" (fun () ->
                                    match seed with
                                    | Some s -> DeterministicGeneration.generate s config
                                    | None -> Generator.generate config
                                )

                                let serialized = Export.serialize result
                                let cost = EnvironmentConstraints.build config.ConstraintWeights result
                                state.Cache.Set(cacheKey, serialized, cost)

                                Metrics.histogram "generation.nodes" (float nodeCount) None
                                Metrics.histogram "generation.cost" cost None

                                return result
                            }

                let serialized = Export.serialize graph
                let id = Guid.NewGuid().ToString("N").[..7]

                let resource = {|
                    Id = id
                    Href = sprintf "/api/graphs/%s" id
                    Nodes = serialized.Nodes
                    Edges = serialized.Edges
                    Metadata = serialized.Metadata |> Map.add "id" id
                |}

                Metrics.counter "api.graphs.success" None

                return!
                    (setStatusCode 201
                     >=> setHttpHeader "Location" resource.Href
                     >=> json resource) next ctx
            }
    
    /// GET /api/metrics - Prometheus format
    let metrics : HttpHandler =
        fun next ctx ->
            let snapshot = Metrics.snapshot()
            let lines = 
                snapshot
                |> Map.toSeq
                |> Seq.map (fun (name, value) -> sprintf "%s %f" name value)
                |> String.concat "\n"
            
            text lines next ctx
    
    /// GET /api/health - Health check with details and cache metrics
    let health : HttpHandler =
        fun next ctx ->
            let stats = state.Cache.Statistics()
            Health.registerStandardChecks (fun () ->
                {| Count = stats.Count; Capacity = stats.Capacity |}
            )

            let overall = Health.overall()
            let checks = Health.all()

            let statusCode =
                match overall with
                | Healthy -> 200
                | Degraded -> 200
                | Unhealthy -> 503

            let response = {|
                Status = overall.ToString()
                Checks = checks |> Array.map (fun c ->
                    {|
                        Name = c.Name
                        Status = c.Status.ToString()
                        Message = c.Message
                        CheckedAt = c.CheckedAt
                    |}
                )
                Timestamp = DateTime.UtcNow
            |}

            (setStatusCode statusCode >=> json response) next ctx

    /// GET /api/cache/stats - Cache statistics with Pareto efficiency metrics
    let cacheStats : HttpHandler =
        fun next ctx ->
            let stats = state.Cache.Statistics()
            let response = {|
                Count = stats.Count
                Capacity = stats.Capacity
                TotalAccesses = stats.TotalAccesses
                AverageCost = stats.AverageCost
                OldestEntry = stats.OldestEntry
                Efficiency = if stats.TotalAccesses > 0 then float stats.Count / float stats.TotalAccesses else 0.0
            |}
            json response next ctx

    /// DELETE /api/cache - Clear cache with atomic operation
    let clearCache : HttpHandler =
        fun next ctx ->
            lock stateLock (fun () -> state.Cache.Clear())
            Metrics.counter "cache.cleared" None
            setStatusCode 204 next ctx

    /// POST /api/physics - Generate physics scene
    let createPhysics : HttpHandler =
        fun next ctx ->
            task {
                let! request = ctx.BindJsonAsync<GenerationRequest>()

                match validateNodeCount request.NodeCount with
                | Error msg -> return! badRequest msg next ctx
                | Ok nodeCount ->

                let config = { Generator.defaultConfig with NodeCount = nodeCount }
                let graph = Generator.generate config
                let physicsState = GraphPhysics.toPhysicsState graph
                let scene = PhysicsExport.exportScene physicsState

                let resource = {|
                    RigidBodies = scene.RigidBodies
                    Constraints = scene.Constraints
                    TimeStep = physicsState.TimeStep
                    Metadata = Map.ofList [
                        ("nodeCount", string nodeCount)
                        ("generatedAt", DateTime.UtcNow.ToString("o"))
                    ]
                |}

                Metrics.counter "api.physics.success" None

                return!
                    (setStatusCode 201
                     >=> json resource) next ctx
            }

module ProductionApp =
    
    let webApp =
        choose [
            POST >=> route "/api/graphs" >=> ProductionHandlers.createGraph
            POST >=> route "/api/physics" >=> ProductionHandlers.createPhysics
            GET >=> route "/api/health" >=> ProductionHandlers.health
            GET >=> route "/api/metrics" >=> ProductionHandlers.metrics
            GET >=> route "/api/cache/stats" >=> ProductionHandlers.cacheStats
            DELETE >=> route "/api/cache" >=> ProductionHandlers.clearCache
            RequestErrors.NOT_FOUND "Endpoint not found"
        ]
    
    let errorHandler (ex: Exception) (logger: Microsoft.Extensions.Logging.ILogger) =
        logger.Log(Microsoft.Extensions.Logging.LogLevel.Error, Microsoft.Extensions.Logging.EventId(0), ex, ex, fun _ _ -> ex.Message)
        Metrics.counter "errors.unhandled" None
        clearResponse 
        >=> setStatusCode 500 
        >=> json {| Error = "Internal server error" |}
    
    let configureApp (app: IApplicationBuilder) =
        app
            .UseRouting()
            .UseCors(fun policy ->
                policy
                    .AllowAnyOrigin()
                    .AllowAnyMethod()
                    .AllowAnyHeader() |> ignore
            )
            .UseGiraffeErrorHandler(errorHandler)
            .UseGiraffe(webApp)
    
    let configureServices (services: IServiceCollection) =
        services
            .AddCors()
            .AddGiraffe()
            .AddMemoryCache()
        |> ignore
    
    [<EntryPoint>]
    let main args =
        let builder = WebApplication.CreateBuilder(args)
        
        configureServices builder.Services
        
        let app = builder.Build()

        configureApp app

        printfn "Production API: http://localhost:5000"
        printfn ""
        printfn "  POST   /api/graphs        - Create graph (cached, async)"
        printfn "  GET    /api/health        - Health check"
        printfn "  GET    /api/metrics       - Prometheus metrics"
        printfn "  GET    /api/cache/stats   - Cache statistics"
        printfn "  DELETE /api/cache         - Clear cache"
        printfn ""
        printfn "Features:"
        printfn "  ✓ LRU caching (100 entries, disk persistence)"
        printfn "  ✓ Async generation with progress"
        printfn "  ✓ Thread-safe operations"
        printfn "  ✓ Metrics (Prometheus format)"
        printfn "  ✓ Health checks"
        printfn "  ✓ Distributed tracing"
        
        app.Run("http://localhost:5000")
        
        0
