namespace Observability

open System
open System.Diagnostics
open System.Collections.Concurrent

/// Metric types
type MetricType =
    | Counter
    | Gauge
    | Histogram

/// Metric data point
type Metric = {
    Name: string
    Type: MetricType
    Value: float
    Timestamp: DateTime
    Labels: Map<string, string>
}

/// Thread-safe metrics collector
type MetricsCollector() =
    let metrics = ConcurrentDictionary<string, ConcurrentQueue<Metric>>()
    let counters = ConcurrentDictionary<string, int64>()
    let gauges = ConcurrentDictionary<string, float>()
    
    member _.RecordCounter(name: string, ?labels: Map<string, string>) =
        let key = name
        counters.AddOrUpdate(key, 1L, fun _ v -> v + 1L) |> ignore
        
        let metric = {
            Name = name
            Type = Counter
            Value = float (counters.[key])
            Timestamp = DateTime.UtcNow
            Labels = labels |> Option.defaultValue Map.empty
        }
        
        let queue = metrics.GetOrAdd(name, fun _ -> ConcurrentQueue<Metric>())
        queue.Enqueue(metric)
        
        // Keep only last 1000 samples per metric
        let mutable count = queue.Count
        while count > 1000 do
            if queue.TryDequeue() |> fst then
                count <- count - 1
            else
                count <- 0  // Queue emptied by another thread
    
    member _.RecordGauge(name: string, value: float, ?labels: Map<string, string>) =
        gauges.[name] <- value
        
        let metric = {
            Name = name
            Type = Gauge
            Value = value
            Timestamp = DateTime.UtcNow
            Labels = labels |> Option.defaultValue Map.empty
        }
        
        let queue = metrics.GetOrAdd(name, fun _ -> ConcurrentQueue<Metric>())
        queue.Enqueue(metric)

        let mutable count = queue.Count
        while count > 1000 do
            if queue.TryDequeue() |> fst then
                count <- count - 1
            else
                count <- 0  // Queue emptied by another thread
    
    member _.RecordHistogram(name: string, value: float, ?labels: Map<string, string>) =
        let metric = {
            Name = name
            Type = Histogram
            Value = value
            Timestamp = DateTime.UtcNow
            Labels = labels |> Option.defaultValue Map.empty
        }
        
        let queue = metrics.GetOrAdd(name, fun _ -> ConcurrentQueue<Metric>())
        queue.Enqueue(metric)

        let mutable count = queue.Count
        while count > 10000 do  // More samples for histograms
            if queue.TryDequeue() |> fst then
                count <- count - 1
            else
                count <- 0  // Queue emptied by another thread
    
    member _.GetMetrics(name: string) : Metric array =
        match metrics.TryGetValue(name) with
        | true, queue -> queue.ToArray()
        | false, _ -> [||]
    
    member _.GetAllMetrics() : Map<string, Metric array> =
        metrics.Keys
        |> Seq.map (fun key -> (key, metrics.[key].ToArray()))
        |> Map.ofSeq
    
    member _.GetSnapshot() : Map<string, float> =
        Map.ofSeq (
            Seq.append
                (counters |> Seq.map (fun kvp -> (kvp.Key, float kvp.Value)))
                (gauges |> Seq.map (fun kvp -> (kvp.Key, kvp.Value)))
        )
    
    member _.Clear() =
        metrics.Clear()
        counters.Clear()
        gauges.Clear()

/// Global metrics instance
module Metrics =
    let private collector = MetricsCollector()

    let counter (name: string) (labels: Map<string,string> option) = collector.RecordCounter(name, ?labels=labels)
    let gauge (name: string) (value: float) (labels: Map<string,string> option) = collector.RecordGauge(name, value, ?labels=labels)
    let histogram (name: string) (value: float) (labels: Map<string,string> option) = collector.RecordHistogram(name, value, ?labels=labels)
    
    let get name = collector.GetMetrics(name)
    let all () = collector.GetAllMetrics()
    let snapshot () = collector.GetSnapshot()
    let clear () = collector.Clear()

/// Instrumented operations
module Instrumentation =
    
    /// Time an operation and record duration
    let timed<'T> (operation: string) (f: unit -> 'T) : 'T =
        let sw = Stopwatch.StartNew()
        try
            let result = f()
            sw.Stop()
            Metrics.histogram operation (float sw.ElapsedMilliseconds) None
            Metrics.counter (operation + ".success") None
            result
        with ex ->
            sw.Stop()
            Metrics.histogram operation (float sw.ElapsedMilliseconds) None
            Metrics.counter (operation + ".failure") None
            Metrics.counter "errors.total" (Some (Map.ofList [("operation", operation); ("error", ex.GetType().Name)]))
            reraise()
    
    /// Time async operation
    let timedAsync<'T> (operation: string) (f: Async<'T>) : Async<'T> =
        async {
            let sw = Stopwatch.StartNew()
            try
                let! result = f
                sw.Stop()
                Metrics.histogram operation (float sw.ElapsedMilliseconds) None
                Metrics.counter (operation + ".success") None
                return result
            with ex ->
                sw.Stop()
                Metrics.histogram operation (float sw.ElapsedMilliseconds) None
                Metrics.counter (operation + ".failure") None
                Metrics.counter "errors.total" (Some (Map.ofList [("operation", operation)]))
                return raise ex
        }
    
    /// Track operation in progress
    let trackInProgress (operation: string) (f: unit -> 'T) : 'T =
        Metrics.gauge (operation + ".in_progress") 1.0 None
        try
            f()
        finally
            Metrics.gauge (operation + ".in_progress") 0.0 None

/// Health check status
type HealthStatus =
    | Healthy
    | Degraded
    | Unhealthy

type HealthCheck = {
    Name: string
    Status: HealthStatus
    Message: string option
    CheckedAt: DateTime
}

/// Health monitoring
type HealthMonitor() =
    let checks = ConcurrentDictionary<string, HealthCheck>()
    
    member _.RegisterCheck(name: string, check: unit -> HealthStatus * string option) =
        let status, message = check()
        let healthCheck = {
            Name = name
            Status = status
            Message = message
            CheckedAt = DateTime.UtcNow
        }
        checks.[name] <- healthCheck
    
    member _.GetStatus(name: string) : HealthCheck option =
        match checks.TryGetValue(name) with
        | true, check -> Some check
        | false, _ -> None
    
    member _.GetAllStatuses() : HealthCheck array =
        checks.Values |> Seq.toArray
    
    member _.OverallStatus() : HealthStatus =
        let statuses = checks.Values |> Seq.map (fun c -> c.Status) |> Seq.toArray
        if Array.isEmpty statuses then Healthy
        elif Array.exists ((=) Unhealthy) statuses then Unhealthy
        elif Array.exists ((=) Degraded) statuses then Degraded
        else Healthy

/// Global health monitor
module Health =
    let private monitor = HealthMonitor()
    
    let register name check = monitor.RegisterCheck(name, check)
    let status name = monitor.GetStatus(name)
    let all () = monitor.GetAllStatuses()
    let overall () = monitor.OverallStatus()
    
    /// Standard health checks
    let registerStandardChecks (cacheStats: unit -> {| Count: int; Capacity: int |}) =
        // Memory usage
        register "memory" (fun () ->
            let proc = Process.GetCurrentProcess()
            let mb = proc.WorkingSet64 / 1024L / 1024L
            if mb > 8192L then (Unhealthy, Some (sprintf "Memory: %dMB (critical)" mb))
            elif mb > 4096L then (Degraded, Some (sprintf "Memory: %dMB (high)" mb))
            else (Healthy, Some (sprintf "Memory: %dMB" mb))
        )
        
        // Cache utilization
        register "cache" (fun () ->
            let stats = cacheStats()
            let util = float stats.Count / float stats.Capacity
            if util > 0.95 then (Degraded, Some "Cache nearly full")
            else (Healthy, Some (sprintf "Cache: %.0f%%" (util * 100.0)))
        )
        
        // Error rate (from counter snapshot)
        register "errors" (fun () ->
            let snapshot = Metrics.snapshot()
            let totalSuccess = snapshot.TryFind "api.graphs.success" |> Option.defaultValue 0.0
            let totalFailure = snapshot.TryFind "api.graphs.failure" |> Option.defaultValue 0.0
            let total = totalSuccess + totalFailure
            if total > 0.0 then
                let errorRate = totalFailure / total
                if errorRate > 0.1 then (Unhealthy, Some (sprintf "Error rate: %.1f%%" (errorRate * 100.0)))
                elif errorRate > 0.05 then (Degraded, Some (sprintf "Error rate: %.1f%%" (errorRate * 100.0)))
                else (Healthy, None)
            else (Healthy, None)
        )

/// Trace context for distributed tracing
type TraceContext = {
    TraceId: string
    SpanId: string
    ParentSpanId: string option
}

/// Span for tracing operations
type Span = {
    Context: TraceContext
    Operation: string
    StartTime: DateTime
    mutable EndTime: DateTime option
    mutable Tags: Map<string, string>
}

/// Simple tracing system
type Tracer() =
    let activeSpans = ConcurrentDictionary<string, Span>()
    
    member _.StartSpan(operation: string, ?parent: TraceContext) : TraceContext =
        let traceId = 
            match parent with
            | Some p -> p.TraceId
            | None -> Guid.NewGuid().ToString("N")
        
        let spanId = Guid.NewGuid().ToString("N").[..7]
        
        let context = {
            TraceId = traceId
            SpanId = spanId
            ParentSpanId = parent |> Option.map (fun p -> p.SpanId)
        }
        
        let span = {
            Context = context
            Operation = operation
            StartTime = DateTime.UtcNow
            EndTime = None
            Tags = Map.empty
        }
        
        activeSpans.[spanId] <- span
        context
    
    member _.EndSpan(context: TraceContext, ?tags: Map<string, string>) =
        match activeSpans.TryGetValue(context.SpanId) with
        | true, span ->
            span.EndTime <- Some DateTime.UtcNow
            span.Tags <- tags |> Option.defaultValue Map.empty
            
            let duration = 
                match span.EndTime with
                | Some endTime -> (endTime - span.StartTime).TotalMilliseconds
                | None -> 0.0
            
            Metrics.histogram (sprintf "span.%s.duration" span.Operation) duration None
            activeSpans.TryRemove(context.SpanId) |> ignore
        | false, _ -> ()
    
    member _.GetActiveSpans() = activeSpans.Values |> Seq.toArray

/// Global tracer
module Tracing =
    let private tracer = Tracer()
    
    let startSpan operation parent = tracer.StartSpan(operation, ?parent=parent)
    let endSpan context tags = tracer.EndSpan(context, ?tags=tags)
    let activeSpans () = tracer.GetActiveSpans()
    
    /// Execute with tracing
    let traced<'T> (operation: string) (f: TraceContext -> 'T) : 'T =
        let context = startSpan operation None
        try
            let result = f context
            endSpan context (Some Map.empty)
            result
        with ex ->
            endSpan context (Some (Map.ofList [("error", ex.Message)]))
            reraise()
