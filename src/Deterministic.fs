namespace Deterministic

open System
open System.Security.Cryptography
open System.Text
open System.Numerics
open ParallelTempering.Core
open ProceduralGeneration

/// Cryptographic hash-based random number generator
module HashRNG =
    
    /// Generate deterministic bytes from seed and index
    let generateBytes (seed: string) (index: int) : byte[] =
        use sha256 = SHA256.Create()
        let input = Encoding.UTF8.GetBytes(seed + index.ToString())
        sha256.ComputeHash(input)
    
    /// Convert bytes to float in [0, 1)
    let bytesToFloat (bytes: byte[]) : float =
        let value = BitConverter.ToUInt64(bytes, 0)
        float value / float UInt64.MaxValue
    
    /// Get deterministic float from seed and index
    let nextFloat (seed: string) (index: int) : float =
        let bytes = generateBytes seed index
        bytesToFloat bytes
    
    /// Get deterministic Vector3 from seed and index
    let nextVector3 (seed: string) (index: int) : Vector3 =
        let bytes = generateBytes seed (index * 3)
        let x = bytesToFloat bytes
        let y = bytesToFloat (generateBytes seed (index * 3 + 1))
        let z = bytesToFloat (generateBytes seed (index * 3 + 2))
        Vector3(float32 (x * 2.0 - 1.0), float32 (y * 2.0 - 1.0), float32 (z * 2.0 - 1.0))
    
    /// Get deterministic integer in range [0, max)
    let nextInt (seed: string) (index: int) (max: int) : int =
        let f = nextFloat seed index
        int (f * float max)

/// Deterministic fibonacci sphere generation
module DeterministicDistribution =
    let private phi = (1.0 + sqrt 5.0) / 2.0
    
    /// Generate fibonacci sphere with deterministic perturbation
    let fibonacciSphere (seed: string) (count: int) (perturbation: float) : Vector3 array =
        if count <= 0 then [||]
        elif count = 1 then [| Vector3.Zero |]
        else
            [| 0 .. count - 1 |]
            |> Array.map (fun i ->
                let y = 1.0 - (float i / float (count - 1)) * 2.0
                let radius = sqrt (1.0 - y * y)
                let theta = phi * float i * 2.0 * Math.PI

                let basePos = Vector3(
                    float32 (radius * cos theta),
                    float32 y,
                    float32 (radius * sin theta)
                )

                if perturbation > 0.0 then
                    let noise = HashRNG.nextVector3 seed i
                    basePos + noise * float32 perturbation
                else
                    basePos
            )
    
    /// Generate uniformly distributed points in sphere
    let uniformSphere (seed: string) (count: int) (radius: float) : Vector3 array =
        [| 0 .. count - 1 |]
        |> Array.map (fun i ->
            let u = HashRNG.nextFloat seed (i * 3)
            let v = HashRNG.nextFloat seed (i * 3 + 1)
            let w = HashRNG.nextFloat seed (i * 3 + 2)
            
            let theta = 2.0 * Math.PI * u
            let phi = Math.Acos(2.0 * v - 1.0)
            let r = Math.Pow(w, 1.0/3.0) * radius
            
            Vector3(
                float32 (r * sin phi * cos theta),
                float32 (r * sin phi * sin theta),
                float32 (r * cos phi)
            )
        )

/// Deterministic graph generation
module DeterministicGeneration =
    open ProceduralGeneration

    /// Hash 3D coordinates into deterministic seed
    let hashCoordinates (baseSeed: uint64) (x: int) (y: int) (z: int) : uint64 =
        let mutable h = baseSeed
        h <- h ^^^ (uint64 x * 0xff51afd7ed558ccdUL)
        h <- h ^^^ (h >>> 33)
        h <- h ^^^ (uint64 y * 0xc4ceb9fe1a85ec53UL)
        h <- h ^^^ (h >>> 33)
        h <- h ^^^ (uint64 z * 0xff51afd7ed558ccdUL)
        h <- h ^^^ (h >>> 33)
        h

    /// Generate initial graph from seed
    let createGraph (seed: string) (nodeCount: int) (perturbation: float) : SpatialGraph<NodeType> =
        let positions = DeterministicDistribution.fibonacciSphere seed nodeCount perturbation
        
        let nodes = Array.init nodeCount (fun i ->
            if i = 0 then Origin
            elif i = nodeCount - 1 then Terminal
            else
                let value = HashRNG.nextFloat seed (i + 1000)
                Standard value
        )
        
        { Nodes = nodes; Positions = positions; Edges = [] }
    
    /// Generate deterministic edges based on proximity and hash
    let generateEdges (seed: string) (graph: SpatialGraph<NodeType>) (avgDegree: float) : SpatialGraph<NodeType> =
        let targetEdges = int (float graph.Nodes.Length * avgDegree / 2.0)
        let mutable edges = []
        let mutable edgeCount = 0
        
        // Deterministic edge selection
        for i in 0 .. graph.Nodes.Length - 1 do
            for j in i + 1 .. graph.Nodes.Length - 1 do
                let dist = Vector3.Distance(graph.Positions.[i], graph.Positions.[j])
                let probability = 1.0 / (1.0 + float dist * float dist)
                let threshold = HashRNG.nextFloat seed (i * 1000 + j)
                
                if threshold < probability && edgeCount < targetEdges then
                    edges <- (i, j, float dist) :: edges
                    edgeCount <- edgeCount + 1
        
        { graph with Edges = edges }
    
    /// Complete deterministic generation pipeline
    let generate (seed: string) (config: ProceduralGeneration.Generator.Config) : SpatialGraph<NodeType> =
        let initialGraph = createGraph seed config.NodeCount 0.1
        let graphWithEdges = generateEdges seed initialGraph 3.0
        
        // Run optimization with deterministic mutations
        let deterministicMutate (graph: SpatialGraph<NodeType>) (temperature: float) : SpatialGraph<NodeType> =
            let stepSeed = seed + graph.GetHashCode().ToString()
            let choice = HashRNG.nextInt stepSeed 0 3
            
            match choice with
            | 0 ->
                let idx = HashRNG.nextInt stepSeed 1 graph.Nodes.Length
                let offset = HashRNG.nextVector3 stepSeed (idx + 100) * float32 temperature
                { graph with 
                    Positions = Array.mapi (fun i p -> if i = idx then p + offset else p) graph.Positions }
            | 1 ->
                let i = HashRNG.nextInt stepSeed 10 graph.Nodes.Length
                let j = HashRNG.nextInt stepSeed 11 graph.Nodes.Length
                if i <> j && temperature > 1.5 then
                    let weight = float (Vector3.Distance(graph.Positions.[i], graph.Positions.[j]))
                    { graph with Edges = (i, j, weight) :: graph.Edges }
                else graph
            | _ ->
                if graph.Edges.Length > graph.Nodes.Length && temperature > 1.5 then
                    let idx = HashRNG.nextInt stepSeed 20 graph.Edges.Length
                    { graph with Edges = graph.Edges |> List.indexed |> List.filter (fst >> (<>) idx) |> List.map snd }
                else graph
        
        let initialStates = Array.init config.Temperatures.Length (fun _ -> graphWithEdges)
        let energy = ProceduralGeneration.EnvironmentConstraints.build config.ConstraintWeights

        let finalChains =
            ParallelTempering.run
                config.Temperatures
                initialStates
                deterministicMutate
                energy
                config.Iterations
                config.SwapFrequency

        ParallelTempering.getBest finalChains

/// Hash chain for progressive generation
module HashChain =
    
    /// Generate chain of hashes
    let generate (seed: string) (length: int) : string array =
        let mutable current = seed
        Array.init length (fun i ->
            if i = 0 then seed
            else
                use sha256 = SHA256.Create()
                let bytes = Encoding.UTF8.GetBytes(current)
                let hash = sha256.ComputeHash(bytes)
                current <- BitConverter.ToString(hash).Replace("-", "").ToLower()
                current
        )
    
    /// Verify hash chain integrity
    let verify (chain: string array) : bool =
        if chain.Length < 2 then true
        else
            chain
            |> Array.pairwise
            |> Array.forall (fun (prev, next) ->
                use sha256 = SHA256.Create()
                let bytes = Encoding.UTF8.GetBytes(prev)
                let hash = sha256.ComputeHash(bytes)
                let computed = BitConverter.ToString(hash).Replace("-", "").ToLower()
                computed = next
            )
    
    /// Generate environment at specific chain depth
    let generateAtDepth (chain: string array) (depth: int) (config: ProceduralGeneration.Generator.Config) : Result<SpatialGraph<NodeType>, string> =
        if depth < 0 then
            Error "Chain depth cannot be negative"
        elif depth >= chain.Length then
            Error $"Chain depth {depth} exceeds chain length {chain.Length}"
        else
            Ok (DeterministicGeneration.generate chain.[depth] config)

/// Export for reproducibility
module Reproducibility =
    open System.Text.Json
    
    type GenerationManifest = {
        Seed: string
        NodeCount: int
        Iterations: int
        Temperatures: float array
        ConstraintWeights: Map<string, float>
        Timestamp: string
        HashChain: string array
    }
    
    let createManifest (seed: string) (config: ProceduralGeneration.Generator.Config) (chainLength: int) : GenerationManifest =
        {
            Seed = seed
            NodeCount = config.NodeCount
            Iterations = config.Iterations
            Temperatures = config.Temperatures
            ConstraintWeights = config.ConstraintWeights
            Timestamp = DateTime.UtcNow.ToString("o")
            HashChain = HashChain.generate seed chainLength
        }
    
    let toJson (manifest: GenerationManifest) : string =
        JsonSerializer.Serialize(manifest, JsonSerializerOptions(WriteIndented = true))
    
    let fromJson (json: string) : GenerationManifest =
        JsonSerializer.Deserialize<GenerationManifest>(json)
    
    /// Verify generation can be reproduced
    let verifyReproducible (manifest: GenerationManifest) (graph: SpatialGraph<NodeType>) : bool =
        HashChain.verify manifest.HashChain
