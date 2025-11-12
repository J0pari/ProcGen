namespace Cache

open System
open System.Collections.Concurrent
open System.Threading
open ProceduralGeneration

/// LRU cache entry with metadata
type CacheEntry<'T> = {
    Value: 'T
    Cost: float
    CreatedAt: DateTime
    AccessCount: int64
    LastAccessed: DateTime
}

/// Thread-safe LRU cache with size limit
type LRUCache<'K, 'V when 'K: equality>(capacity: int) =
    let cache = ConcurrentDictionary<'K, CacheEntry<'V>>()
    let accessOrder = ConcurrentQueue<'K>()
    let lockObj = obj()
    let mutable currentSize = 0
    
    /// Compute cache key from generation parameters
    static member HashKey(nodeCount: int, iterations: int, seed: string option, weights: Map<string, float>) : string =
        let weightStr = 
            weights 
            |> Map.toSeq 
            |> Seq.map (fun (k, v) -> sprintf "%s:%.3f" k v)
            |> String.concat ","
        
        let seedStr = seed |> Option.defaultValue "random"
        sprintf "%d_%d_%s_%s" nodeCount iterations seedStr weightStr
    
    member _.Capacity = capacity
    member _.Count = cache.Count
    member _.CurrentSize = currentSize
    
    /// Try get value from cache
    member _.TryGet(key: 'K) : 'V option =
        match cache.TryGetValue(key) with
        | true, entry ->
            // Update access statistics atomically
            let rec updateEntry () =
                match cache.TryGetValue(key) with
                | true, currentEntry ->
                    let updated = {
                        currentEntry with
                            AccessCount = currentEntry.AccessCount + 1L
                            LastAccessed = DateTime.UtcNow
                    }
                    if cache.TryUpdate(key, updated, currentEntry) then
                        ()
                    else
                        updateEntry()  // Retry if update failed
                | false, _ -> ()

            updateEntry()
            Some entry.Value
        | false, _ -> None
    
    /// Add or update value in cache
    member this.Set(key: 'K, value: 'V, cost: float) : unit =
        lock lockObj (fun () ->
            let entry = {
                Value = value
                Cost = cost
                CreatedAt = DateTime.UtcNow
                AccessCount = 1L
                LastAccessed = DateTime.UtcNow
            }

            // Check if key exists before updating size
            let isNewKey = not (cache.ContainsKey(key))
            cache.AddOrUpdate(key, entry, fun _ _ -> entry) |> ignore

            if isNewKey then
                accessOrder.Enqueue(key)
                currentSize <- currentSize + 1

                // Evict if over capacity
                while currentSize > capacity do
                    match accessOrder.TryDequeue() with
                    | true, oldKey ->
                        match cache.TryRemove(oldKey) with
                        | true, _ ->
                            currentSize <- currentSize - 1
                        | false, _ -> ()
                    | false, _ -> ()
        )
    
    /// Clear entire cache
    member _.Clear() : unit =
        lock lockObj (fun () ->
            cache.Clear()
            let mutable hasMore = true
            while hasMore do
                match accessOrder.TryDequeue() with
                | true, _ -> ()
                | false, _ -> hasMore <- false
            currentSize <- 0
        )
    
    /// Get cache statistics
    member _.Statistics() =
        let entries = cache.Values |> Seq.toArray
        {|
            Count = cache.Count
            Capacity = capacity
            TotalAccesses = entries |> Array.sumBy (fun e -> e.AccessCount)
            AverageCost = if entries.Length > 0 then entries |> Array.averageBy (fun e -> e.Cost) else 0.0
            OldestEntry = if entries.Length > 0 then entries |> Array.minBy (fun e -> e.CreatedAt) |> fun e -> e.CreatedAt else DateTime.MinValue
        |}

/// Cached graph generator with automatic memoization
module CachedGenerator =
    let mutable private cacheCapacity = 100
    let mutable private cache = LRUCache<string, Export.SerializedGraph>(capacity = cacheCapacity)
    let private generateLock = obj()

    /// Get current cache capacity
    let getCapacity () : int = cacheCapacity

    /// Update cache capacity (recreates cache if changed)
    let updateCapacity (newCapacity: int) : unit =
        if newCapacity > 0 && newCapacity <> cacheCapacity then
            lock generateLock (fun () ->
                cacheCapacity <- newCapacity
                cache <- LRUCache<string, Export.SerializedGraph>(capacity = newCapacity)
            )
    
    /// Statistics access
    let statistics() = cache.Statistics()
    
    /// Clear cache
    let clear() = cache.Clear()

    /// Add entry to cache
    let addToCache(key: string, value: Export.SerializedGraph, cost: float) =
        cache.Set(key, value, cost)

    /// Generate with caching
    let generate (config: Generator.Config) (seed: string option) : Export.SerializedGraph =
        let key = LRUCache<_,_>.HashKey(
            config.NodeCount,
            config.Iterations,
            seed,
            config.ConstraintWeights
        )
        
        match cache.TryGet(key) with
        | Some cached -> cached
        | None ->
            lock generateLock (fun () ->
                // Double-check after acquiring lock
                match cache.TryGet(key) with
                | Some cached -> cached
                | None ->
                    let graph = 
                        match seed with
                        | Some s -> Deterministic.DeterministicGeneration.generate s config
                        | None -> Generator.generate config
                    
                    let serialized = Export.serialize graph
                    let cost = 
                        let costFn = EnvironmentConstraints.build config.ConstraintWeights
                        costFn graph
                    
                    cache.Set(key, serialized, cost)
                    serialized
            )
    
    /// Async generation with caching
    let generateAsync (config: Generator.Config) (seed: string option) : Async<Export.SerializedGraph> =
        async {
            let key = LRUCache<_,_>.HashKey(
                config.NodeCount,
                config.Iterations,
                seed,
                config.ConstraintWeights
            )
            
            match cache.TryGet(key) with
            | Some cached -> return cached
            | None ->
                // Run generation on thread pool
                let! result = Async.AwaitTask(System.Threading.Tasks.Task.Run(fun () ->
                    lock generateLock (fun () ->
                        match cache.TryGet(key) with
                        | Some cached -> cached
                        | None ->
                            let graph = 
                                match seed with
                                | Some s -> Deterministic.DeterministicGeneration.generate s config
                                | None -> Generator.generate config
                            
                            let serialized = Export.serialize graph
                            let cost = 
                                let costFn = EnvironmentConstraints.build config.ConstraintWeights
                                costFn graph
                            
                            cache.Set(key, serialized, cost)
                            serialized
                    )
                ))
                return result
        }

/// Disk-backed persistent cache
module PersistentCache =
    open System.IO
    open System.Text.Json
    
    type CacheMetadata = {
        Key: string
        Cost: float
        Created: DateTime
        Accessed: DateTime
        AccessCount: int64
    }
    
    let private cacheDir = "cache"
    let private metadataFile = Path.Combine(cacheDir, "metadata.json")
    
    /// Ensure cache directory exists
    let private ensureDirectory() =
        if not (Directory.Exists(cacheDir)) then
            Directory.CreateDirectory(cacheDir) |> ignore
    
    /// Get file path for cache key
    let private getPath(key: string) : string =
        let hash = 
            use sha = System.Security.Cryptography.SHA256.Create()
            let bytes = System.Text.Encoding.UTF8.GetBytes(key)
            let hashBytes = sha.ComputeHash(bytes)
            BitConverter.ToString(hashBytes).Replace("-", "").Substring(0, 16).ToLower()
        Path.Combine(cacheDir, hash + ".json")
    
    /// Load metadata from disk
    let loadMetadata() : Map<string, CacheMetadata> =
        ensureDirectory()
        if File.Exists(metadataFile) then
            try
                let json = File.ReadAllText(metadataFile)
                let items = JsonSerializer.Deserialize<CacheMetadata array>(json)
                items |> Array.map (fun m -> (m.Key, m)) |> Map.ofArray
            with _ -> Map.empty
        else Map.empty
    
    /// Save metadata to disk
    let private saveMetadata(metadata: Map<string, CacheMetadata>) =
        ensureDirectory()
        let items = metadata |> Map.values |> Seq.toArray
        let json = JsonSerializer.Serialize(items, JsonSerializerOptions(WriteIndented = true))
        File.WriteAllText(metadataFile, json)
    
    /// Try load from disk cache
    let tryLoad(key: string) : Export.SerializedGraph option =
        let path = getPath key
        if File.Exists(path) then
            try
                let json = File.ReadAllText(path)
                let graph = JsonSerializer.Deserialize<Export.SerializedGraph>(json)
                
                // Update metadata
                let metadata = loadMetadata()
                match Map.tryFind key metadata with
                | Some meta ->
                    let updated = { meta with Accessed = DateTime.UtcNow; AccessCount = meta.AccessCount + 1L }
                    saveMetadata (Map.add key updated metadata)
                | None -> ()
                
                Some graph
            with _ -> None
        else None
    
    /// Save to disk cache
    let save(key: string, graph: Export.SerializedGraph, cost: float) =
        ensureDirectory()
        let path = getPath key
        let json = JsonSerializer.Serialize(graph, JsonSerializerOptions(WriteIndented = true))
        File.WriteAllText(path, json)
        
        // Update metadata
        let metadata = loadMetadata()
        let entry = {
            Key = key
            Cost = cost
            Created = DateTime.UtcNow
            Accessed = DateTime.UtcNow
            AccessCount = 1L
        }
        saveMetadata (Map.add key entry metadata)
    
    /// Clear disk cache
    let clear() =
        if Directory.Exists(cacheDir) then
            Directory.Delete(cacheDir, true)
    
    /// Get cache statistics
    let statistics() =
        let metadata = loadMetadata()
        let values = metadata |> Map.values |> Seq.toArray
        {|
            Count = values.Length
            TotalSize = 
                if Directory.Exists(cacheDir) then
                    Directory.GetFiles(cacheDir, "*.json")
                    |> Array.sumBy (fun f -> FileInfo(f).Length)
                else 0L
            OldestEntry = if values.Length > 0 then values |> Array.minBy (fun v -> v.Created) |> fun v -> v.Created else DateTime.MinValue
            MostAccessed = if values.Length > 0 then values |> Array.maxBy (fun v -> v.AccessCount) else Unchecked.defaultof<CacheMetadata>
        |}

/// Combined cache strategy: memory + disk
module HybridCache =
    let mutable private memCacheCapacity = 100
    let mutable private cache = LRUCache<string, Export.SerializedGraph>(memCacheCapacity)
    let private hybridLock = obj()

    /// Get current memory cache capacity
    let getMemoryCapacity () : int = memCacheCapacity

    /// Update memory cache capacity
    let updateMemoryCapacity (newCapacity: int) : unit =
        if newCapacity > 0 && newCapacity <> memCacheCapacity then
            lock hybridLock (fun () ->
                memCacheCapacity <- newCapacity
                cache <- LRUCache<string, Export.SerializedGraph>(newCapacity)
            )

    /// Try memory cache first, then disk, then generate
    let generate (config: Generator.Config) (seed: string option) : Export.SerializedGraph =
        let key = LRUCache<_,_>.HashKey(
            config.NodeCount,
            config.Iterations,
            seed,
            config.ConstraintWeights
        )

        // Try memory cache first (fast)
        match cache.TryGet(key) with
        | Some cached -> cached
        | None ->
            // Try disk cache second
            match PersistentCache.tryLoad key with
            | Some cached ->
                // Load into memory cache for next time
                let cost =
                    let graph =
                        match seed with
                        | Some s -> Deterministic.DeterministicGeneration.generate s config
                        | None -> Generator.generate config
                    let costFn = EnvironmentConstraints.build config.ConstraintWeights
                    costFn graph
                CachedGenerator.addToCache(key, cached, cost)
                cached
            | None ->
                // Generate and cache to both
                let result = CachedGenerator.generate config seed
                let cost =
                    let graph =
                        match seed with
                        | Some s -> Deterministic.DeterministicGeneration.generate s config
                        | None -> Generator.generate config
                    let costFn = EnvironmentConstraints.build config.ConstraintWeights
                    costFn graph
                PersistentCache.save(key, result, cost)
                result
    
    /// Try memory cache first, then disk, then generate (async)
    let generateAsync (config: Generator.Config) (seed: string option) : Async<Export.SerializedGraph> =
        async {
            let key = LRUCache<_,_>.HashKey(
                config.NodeCount,
                config.Iterations,
                seed,
                config.ConstraintWeights
            )

            // Try memory cache first
            match cache.TryGet(key) with
            | Some cached -> return cached
            | None ->
                // Try disk cache second
                match PersistentCache.tryLoad key with
                | Some cached ->
                    // Load into memory cache
                    let cost =
                        let graph =
                            match seed with
                            | Some s -> Deterministic.DeterministicGeneration.generate s config
                            | None -> Generator.generate config
                        let costFn = EnvironmentConstraints.build config.ConstraintWeights
                        costFn graph
                    CachedGenerator.addToCache(key, cached, cost)
                    return cached
                | None ->
                    // Generate async and cache to both
                    let! result = CachedGenerator.generateAsync config seed
                    let cost =
                        let graph =
                            match seed with
                            | Some s -> Deterministic.DeterministicGeneration.generate s config
                            | None -> Generator.generate config
                        let costFn = EnvironmentConstraints.build config.ConstraintWeights
                        costFn graph
                    PersistentCache.save(key, result, cost)
                    return result
        }
    
    /// Warm cache from disk
    let warmCache() =
        let metadata = PersistentCache.loadMetadata()
        metadata
        |> Map.iter (fun key meta ->
            match PersistentCache.tryLoad key with
            | Some graph ->
                CachedGenerator.addToCache(key, graph, meta.Cost)
            | None -> ()
        )
    
    /// Statistics from both caches
    let statistics() =
        let memStats = CachedGenerator.statistics()
        let diskStats = PersistentCache.statistics()
        {|
            Memory = memStats
            Disk = diskStats
        |}
