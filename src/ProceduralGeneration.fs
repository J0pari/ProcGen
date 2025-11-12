namespace ProceduralGeneration

open System
open System.Numerics
open ParallelTempering.Core

/// Node metadata
type NodeType =
    | Origin
    | Terminal
    | Standard of value: float

/// Uniform sphere distribution using golden angle
/// Minimizes clustering for any point count (Vogel 1979)
module Initialization =
    
    let uniformSphere (count: int) : Vector3 array =
        if count <= 0 then [||]
        elif count = 1 then [| Vector3.Zero |]
        else
            let goldenAngle = Math.PI * (3.0 - sqrt 5.0)

            [| 0 .. count - 1 |]
            |> Array.map (fun i ->
                let y = 1.0 - (float i / float (count - 1)) * 2.0
                let radius = sqrt (1.0 - y * y)
                let theta = float i * goldenAngle
                Vector3(
                    float32 (radius * cos theta),
                    float32 y,
                    float32 (radius * sin theta)
                )
            )
    
    let createGraph (nodeCount: int) : SpatialGraph<NodeType> =
        let positions = uniformSphere nodeCount
        let nodes = Array.init nodeCount (fun i ->
            if i = 0 then Origin
            elif i = nodeCount - 1 then Terminal
            else Standard (ThreadSafeRandom.nextDouble())
        )
        { Nodes = nodes; Positions = positions; Edges = [] }

/// Environment-specific constraints
module EnvironmentConstraints =
    
    /// Ensure origin and terminal are reachable from each other
    /// Hard constraint: high penalty if path doesn't exist
    let endpointConnectivity (graph: SpatialGraph<NodeType>) : float =
        match graph.Nodes |> Array.tryFindIndex ((=) Origin), graph.Nodes |> Array.tryFindIndex ((=) Terminal) with
        | Some originIdx, Some terminalIdx ->
            let reachableCount = Execution.Graph.countReachableFrom graph.Nodes.Length graph.Edges originIdx
            if reachableCount > terminalIdx then 0.0 else 10000.0
        | _ -> 10000.0  // Missing origin or terminal
    
    /// Prefer smooth value transitions between connected nodes
    /// Soft constraint: encourages gradual difficulty curves
    let smoothTransitions (graph: SpatialGraph<NodeType>) : float =
        ParallelTempering.Core.SpatialGraphUtils.sumEdgeNodePairs graph (fun nodeI nodeJ _ _ ->
            match nodeI, nodeJ with
            | Standard v1, Standard v2 -> abs (v1 - v2) * 10.0
            | _ -> 0.0
        )
    
    /// Build complete cost function with calibrated weights
    /// Weights determined empirically for balanced solutions:
    /// - connectivity=1.0: hard constraint, must be satisfied
    /// - endpoint=1.0: hard constraint for playability
    /// - edgeLength=0.1: soft preference for aesthetic spacing
    /// - collision=0.5: medium priority, prevents overlap
    /// - distribution=0.05: weak preference for uniform spread
    /// - transitions=0.1: soft preference for smooth difficulty
    let build (config: Map<string, float>) : CostFunction<NodeType> =
        Constraints.combine [
            config.["connectivity"], Constraints.connectivity
            config.["endpoint"], endpointConnectivity
            config.["edgeLength"], Constraints.edgeLength 2.0
            config.["collision"], Constraints.collision 0.5
            config.["distribution"], Constraints.distribution
            config.["transitions"], smoothTransitions
        ]

/// Environment generator
module Generator =
    
    type Config = {
        NodeCount: int
        Temperatures: float array
        Iterations: int
        SwapFrequency: int
        ConstraintWeights: Map<string, float>
    }
    
    /// Default configuration with empirically tuned parameters
    let defaultConfig = {
        NodeCount = 20
        Temperatures = ParallelTempering.geometricLadder 0.5 5.0 5
        Iterations = 500
        SwapFrequency = ParallelTempering.swapFrequency 20
        ConstraintWeights = Map.ofList [
            "connectivity", 1.0
            "endpoint", 1.0
            "edgeLength", 0.1
            "collision", 0.5
            "distribution", 0.05
            "transitions", 0.1
        ]
    }
    
    let generate (config: Config) : SpatialGraph<NodeType> =
        let initialStates = 
            Array.init config.Temperatures.Length (fun _ -> 
                Initialization.createGraph config.NodeCount
            )
        
        let cost = EnvironmentConstraints.build config.ConstraintWeights
        let mutate = Mutations.adaptive

        let finalChains =
            ParallelTempering.run
                config.Temperatures
                initialStates
                mutate
                cost
                config.Iterations
                config.SwapFrequency

        ParallelTempering.getBest finalChains
    
    let generateWithProgress (config: Config) (onProgress: int -> Chain<NodeType> array -> unit) : SpatialGraph<NodeType> =
        let initialStates =
            Array.init config.Temperatures.Length (fun _ ->
                Initialization.createGraph config.NodeCount
            )

        let cost = EnvironmentConstraints.build config.ConstraintWeights
        let mutate = Mutations.adaptive

        let finalChains =
            ParallelTempering.runWithProgress
                config.Temperatures
                initialStates
                mutate
                cost
                config.Iterations
                config.SwapFrequency
                50  // Progress frequency
                onProgress

        ParallelTempering.getBest finalChains

/// Export formats
module Export =
    open System.Text.Json
    
    type SerializedNode = {
        NodeType: string
        Value: float
        Position: float array
    }
    
    type SerializedEdge = {
        From: int
        To: int
        Weight: float
    }
    
    type SerializedGraph = {
        Nodes: SerializedNode array
        Edges: SerializedEdge array
        Metadata: Map<string, string>
    }
    
    let serialize (graph: SpatialGraph<NodeType>) : SerializedGraph =
        let nodes = 
            Array.map2 (fun node pos ->
                let (typeName, value) =
                    match node with
                    | Origin -> ("origin", 0.0)
                    | Terminal -> ("terminal", 1.0)
                    | Standard v -> ("standard", v)
                {
                    NodeType = typeName
                    Value = value
                    Position = [| float (pos:Vector3).X; float pos.Y; float pos.Z |]
                }
            ) graph.Nodes graph.Positions
        
        let edges =
            graph.Edges
            |> List.map (fun (i, j, w) ->
                { From = i; To = j; Weight = w }
            )
            |> List.toArray
        
        {
            Nodes = nodes
            Edges = edges
            Metadata = Map.ofList [
                ("nodeCount", string graph.Nodes.Length)
                ("edgeCount", string graph.Edges.Length)
                ("generated", DateTime.UtcNow.ToString("o"))
            ]
        }
    
    let toJson (graph: SpatialGraph<NodeType>) : string =
        let serialized = serialize graph
        JsonSerializer.Serialize(serialized, JsonSerializerOptions(WriteIndented = true))
