namespace ParallelTempering.Core

open System
open System.Numerics
open System.Collections.Generic

/// Spatial graph structure
type SpatialGraph<'T> = {
    Nodes: 'T array
    Positions: Vector3 array
    Edges: (int * int * float) list
}

/// Cost function returns weighted sum of constraint violations
/// Lower cost = better solution. Minimized during optimization.
type CostFunction<'T> = SpatialGraph<'T> -> float

/// State mutation strategy
type MutationStrategy<'T> = SpatialGraph<'T> -> float -> SpatialGraph<'T>

/// Chain state at single temperature
type Chain<'T> = {
    State: SpatialGraph<'T>
    Cost: float
    Temperature: float
    AcceptanceRate: float
}

/// Update Acceptance module to use thread-safe random
module Acceptance =

    let acceptMove (currentCost: float) (proposedCost: float) (temperature: float) : bool =
        let delta = proposedCost - currentCost
        delta < 0.0 || ThreadSafeRandom.nextDouble() < exp(-delta / temperature)

/// Update Mutations module to use thread-safe random
module Mutations =

    let perturbPosition (index: int) (graph: SpatialGraph<'T>) (temperature: float) : SpatialGraph<'T> =
        let scale = float32 (temperature * 0.5)
        let offset = System.Numerics.Vector3(
            (ThreadSafeRandom.nextSingle() * 2.0f - 1.0f) * scale,
            (ThreadSafeRandom.nextSingle() * 2.0f - 1.0f) * scale,
            (ThreadSafeRandom.nextSingle() * 2.0f - 1.0f) * scale
        )
        { graph with
            Positions = Array.mapi (fun i p -> if i = index then p + offset else p) graph.Positions }

    let addEdge (graph: SpatialGraph<'T>) : SpatialGraph<'T> =
        let n = graph.Nodes.Length
        if n < 2 then graph
        else
            let i = ThreadSafeRandom.nextInt(n)
            let j = ThreadSafeRandom.nextInt(n)
            if i <> j && not (graph.Edges |> List.exists (fun (a, b, _) -> (a = i && b = j) || (a = j && b = i))) then
                let weight = float (System.Numerics.Vector3.Distance(graph.Positions.[i], graph.Positions.[j]))
                { graph with Edges = (i, j, weight) :: graph.Edges }
            else graph

    let removeEdge (graph: SpatialGraph<'T>) : SpatialGraph<'T> =
        let minEdges = graph.Nodes.Length - 1
        if graph.Edges.Length > minEdges then
            let idx = ThreadSafeRandom.nextInt(graph.Edges.Length)
            { graph with Edges = graph.Edges |> List.mapi (fun i e -> (i, e)) |> List.filter (fst >> (<>) idx) |> List.map snd }
        else graph

    let adaptive (graph: SpatialGraph<'T>) (temperature: float) : SpatialGraph<'T> =
        let explorationThreshold = 2.0

        if temperature > explorationThreshold then
            match ThreadSafeRandom.nextInt(5) with
            | 0 -> perturbPosition (ThreadSafeRandom.nextInt(graph.Nodes.Length)) graph temperature
            | 1 | 2 -> addEdge graph
            | _ -> removeEdge graph
        else
            match ThreadSafeRandom.nextInt(10) with
            | 0 -> addEdge graph
            | 1 -> removeEdge graph
            | _ -> perturbPosition (ThreadSafeRandom.nextInt(graph.Nodes.Length)) graph temperature

/// Parallel tempering implementation
module ParallelTempering =
    /// Generate temperature ladder with geometric spacing
    /// Ensures roughly constant swap acceptance rate (~23-40% is optimal)
    /// Based on Kone & Kofke 2005, Predescu et al. 2004
    ///
    /// Example: geometricLadder 0.5 5.0 4 -> [|5.0; 2.15; 0.93; 0.5|]
    let geometricLadder (tMin: float) (tMax: float) (numTemperatures: int) : float array =
        if numTemperatures < 2 then [| tMax |]
        elif tMin <= 0.0 || tMax <= 0.0 || tMin > tMax then [| tMax |]
        else
            let ratio = tMax / tMin
            Array.init numTemperatures (fun i ->
                tMax * (ratio ** (-float i / float (numTemperatures - 1)))
            )
    
    /// Swap acceptance probability between adjacent temperatures
    /// Metropolis criterion: P = exp(ΔC·(β₁-β₂)) where β=1/T
    ///
    /// Example: T₁=5.0 (C=100), T₂=1.0 (C=50)
    ///   β₁=0.2, β₂=1.0, ΔC=-50
    ///   P = exp(-50·(-0.8)) = exp(40) ≈ 1.0 (accept)
    let swapProbability (chain1: Chain<'T>) (chain2: Chain<'T>) : float =
        if chain1.Temperature <= 0.0 || chain2.Temperature <= 0.0 then 0.0
        else
            let beta1 = 1.0 / chain1.Temperature
            let beta2 = 1.0 / chain2.Temperature
            let deltaC = chain2.Cost - chain1.Cost
            exp (deltaC * (beta1 - beta2))
    
    /// Attempt swap between adjacent temperature chains
    let attemptSwap (chain1: Chain<'T>) (chain2: Chain<'T>) : (Chain<'T> * Chain<'T> * bool) =
        let p = min 1.0 (swapProbability chain1 chain2)
        if ThreadSafeRandom.nextDouble() < p then
            let swapped1 = { chain1 with State = chain2.State; Cost = chain2.Cost }
            let swapped2 = { chain2 with State = chain1.State; Cost = chain1.Cost }
            (swapped1, swapped2, true)
        else
            (chain1, chain2, false)
    
    /// Single optimization step
    let optimizationStep 
        (mutate: MutationStrategy<'T>)
        (cost: CostFunction<'T>)
        (chain: Chain<'T>) : Chain<'T> =
        
        let proposed = mutate chain.State chain.Temperature
        let proposedCost = cost proposed
        
        if Acceptance.acceptMove chain.Cost proposedCost chain.Temperature then
            { chain with State = proposed; Cost = proposedCost }
        else
            chain
    
    /// Compute swap frequency based on system size
    /// Heuristic: autocorrelation time scales with sqrt(N) for spatial systems
    /// Swap less frequently for larger systems to reduce overhead
    let swapFrequency (nodeCount: int) : int =
        max 5 (int (sqrt (float nodeCount) * 2.0))
    
    /// Track and update acceptance statistics
    let updateAcceptanceRate (chain: Chain<'T>) (accepted: bool) (windowSize: int) : Chain<'T> =
        let currentRate = chain.AcceptanceRate
        let alpha = 1.0 / float windowSize
        let newRate = (1.0 - alpha) * currentRate + alpha * (if accepted then 1.0 else 0.0)
        { chain with AcceptanceRate = newRate }
    
    /// Run parallel tempering optimization
    let run
        (temperatures: float array)
        (initialStates: SpatialGraph<'T> array)
        (mutate: MutationStrategy<'T>)
        (cost: CostFunction<'T>)
        (iterations: int)
        (swapFreq: int) : Chain<'T> array =

        let chains =
            Array.map2 (fun temp state ->
                { State = state; Cost = cost state; Temperature = temp; AcceptanceRate = 0.5 }
            ) temperatures initialStates

        let rec iterate step (chains: Chain<'T> array) =
            if step >= iterations then chains
            else
                let chains = Execution.Parallel.map (optimizationStep mutate cost) chains
                
                let chains =
                    if step % swapFreq = 0 then
                        let mutable chains = chains
                        for i in 0 .. chains.Length - 2 do
                            let (c1, c2, _) = attemptSwap chains.[i] chains.[i+1]
                            chains.[i] <- c1
                            chains.[i+1] <- c2
                        chains
                    else chains
                
                iterate (step + 1) chains
        
        iterate 0 chains

    /// Run parallel tempering with progress callback
    let runWithProgress
        (temperatures: float array)
        (initialStates: SpatialGraph<'T> array)
        (mutate: MutationStrategy<'T>)
        (cost: CostFunction<'T>)
        (iterations: int)
        (swapFreq: int)
        (progressFreq: int)
        (onProgress: int -> Chain<'T> array -> unit) : Chain<'T> array =

        let chains =
            Array.map2 (fun temp state ->
                { State = state; Cost = cost state; Temperature = temp; AcceptanceRate = 0.5 }
            ) temperatures initialStates

        let rec iterate step (chains: Chain<'T> array) =
            if step >= iterations then chains
            else
                let chains = Execution.Parallel.map (optimizationStep mutate cost) chains

                let chains =
                    if step % swapFreq = 0 then
                        let mutable chains = chains
                        for i in 0 .. chains.Length - 2 do
                            let (c1, c2, _) = attemptSwap chains.[i] chains.[i+1]
                            chains.[i] <- c1
                            chains.[i+1] <- c2
                        chains
                    else chains

                if step % progressFreq = 0 then onProgress step chains
                iterate (step + 1) chains

        iterate 0 chains

    /// Extract lowest cost state
    let getBest (chains: Chain<'T> array) : SpatialGraph<'T> =
        chains
        |> Array.minBy (fun c -> c.Cost)
        |> fun c -> c.State

/// Spatial graph utilities
module SpatialGraphUtils =

    /// Calculate centroid of positions
    let centroid (positions: Vector3 array) : Vector3 =
        if positions.Length = 0 then Vector3.Zero
        else
            positions
            |> Array.fold (fun acc p -> acc + p) Vector3.Zero
            |> fun sum -> sum / float32 positions.Length

    /// Apply function to each edge's node pair, sum results
    let sumEdgeNodePairs (graph: SpatialGraph<'T>) (f: 'T -> 'T -> Vector3 -> Vector3 -> float) : float =
        graph.Edges
        |> List.sumBy (fun (i, j, _) ->
            f graph.Nodes.[i] graph.Nodes.[j] graph.Positions.[i] graph.Positions.[j]
        )

/// Constraint functions for spatial graphs
module Constraints =

    /// Returns penalty for unreachable nodes
    /// Time complexity: O(V + E)
    ///
    /// Example: 5 nodes, 3 unreachable -> penalty = 3000.0
    let connectivity (graph: SpatialGraph<'T>) : float =
        let n = graph.Nodes.Length
        if n <= 1 then 0.0
        else
            let reachable = Execution.Graph.countReachableFrom n graph.Edges 0
            let unreachable = n - reachable
            float unreachable * 1000.0

    /// Penalize edges deviating from target length
    /// Quadratic penalty: cost = Σ(actual - target)²
    let edgeLength (targetLength: float) (graph: SpatialGraph<'T>) : float =
        SpatialGraphUtils.sumEdgeNodePairs graph (fun _ _ posI posJ ->
            let dist = Vector3.Distance(posI, posJ)
            let deviation = float dist - targetLength
            deviation * deviation
        )

    /// Collision detection and penalty
    /// Uses brute force O(n²) for n < 200, spatial hashing for n >= 200
    /// Penalty increases quadratically as nodes get closer
    ///
    /// Example: 2 nodes at distance 0.3, minDistance=0.5
    ///   overlap = 0.2 -> penalty = 0.04
    let collision (minDistance: float) (graph: SpatialGraph<'T>) : float =
        SpatialHash.collisionPenalty minDistance graph.Positions

    /// Spatial distribution - penalize clustering
    /// Measures deviation from uniform distribution around centroid
    let distribution (graph: SpatialGraph<'T>) : float =
        if graph.Positions.Length = 0 then 0.0
        else
            let center = SpatialGraphUtils.centroid graph.Positions

            let avgDist =
                graph.Positions
                |> Array.averageBy (fun p -> float (Vector3.Distance(p, center)))

            graph.Positions
            |> Array.sumBy (fun p ->
                let dist = float (Vector3.Distance(p, center))
                let deviation = dist - avgDist
                deviation * deviation * 0.01
            )
    
    /// Boundary constraint - penalize nodes outside a specified radius
    /// Quadratic penalty that increases as nodes move further from origin
    let boundary (maxRadius: float) (graph: SpatialGraph<'T>) : float =
        graph.Positions
        |> Array.sumBy (fun p ->
            let dist = float (p.Length())
            if dist > maxRadius then
                let overflow = dist - maxRadius
                overflow * overflow
            else
                0.0)

    /// Combine multiple constraints with specified weights
    /// Returns weighted sum of individual constraint violations
    let combine (constraints: (float * CostFunction<'T>) list) : CostFunction<'T> =
        fun graph ->
            constraints
            |> List.sumBy (fun (weight, fn) -> weight * fn graph)
