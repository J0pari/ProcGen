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

/// Convergence diagnostics with GPU-aware implementation
module Convergence =

    /// Convergence metrics computed incrementally
    type ConvergenceMetrics = {
        GelmanRubin: float option          // R̂ < 1.1 indicates convergence
        EffectiveSampleSize: float option  // ESS > 50 indicates good mixing
        CostStabilization: float           // CV < 0.1 indicates plateau
        SwapAcceptanceRate: float          // 0.2-0.4 is optimal range
        HasConverged: bool
        Iteration: int
    }

    /// Convergence history for incremental computation
    type ConvergenceHistory = {
        CostHistory: ResizeArray<float> array  // Per-chain circular buffer
        SwapHistory: ResizeArray<bool>         // Recent swap outcomes
        WindowSize: int
        LastCheckIteration: int
    }

    /// Initialize convergence tracking
    let initHistory (numChains: int) (windowSize: int) : ConvergenceHistory =
        {
            CostHistory = Array.init numChains (fun _ -> ResizeArray<float>())
            SwapHistory = ResizeArray<bool>()
            WindowSize = windowSize
            LastCheckIteration = 0
        }

    /// Update history with new data (O(1) amortized)
    let updateHistory (history: ConvergenceHistory) (chains: Chain<'T> array) (swapped: bool list) : ConvergenceHistory =
        // Update cost history with circular buffer behavior
        chains |> Array.iteri (fun i chain ->
            let costs = history.CostHistory.[i]
            costs.Add(chain.Cost)
            if costs.Count > history.WindowSize then
                costs.RemoveAt(0)
        )

        // Update swap history
        swapped |> List.iter history.SwapHistory.Add
        while history.SwapHistory.Count > history.WindowSize do
            history.SwapHistory.RemoveAt(0)

        history

    /// Compute Gelman-Rubin R̂ statistic
    /// CPU implementation - GPU version in convergence.cu for large-scale
    let private gelmanRubin (costHistory: ResizeArray<float> array) : float option =
        let m = float (Array.length costHistory)
        if m < 2.0 then None
        else
            let minLength = costHistory |> Array.map (fun c -> c.Count) |> Array.min
            let n = float minLength
            if n < 10.0 then None  // Need sufficient history
            else
                // Take last n samples from each chain
                let samples = costHistory |> Array.map (fun costs ->
                    costs |> Seq.skip (costs.Count - int n) |> Seq.toArray
                )

                // Chain means
                let chainMeans = samples |> Array.map Array.average

                // Grand mean
                let grandMean = Array.average chainMeans

                // Between-chain variance
                let B = (n / (m - 1.0)) * (chainMeans |> Array.sumBy (fun mean -> (mean - grandMean) ** 2.0))

                // Within-chain variance
                let chainVars = samples |> Array.map (fun chain ->
                    let mean = Array.average chain
                    (1.0 / (n - 1.0)) * (chain |> Array.sumBy (fun x -> (x - mean) ** 2.0))
                )
                let W = Array.average chainVars

                // Pooled variance and R̂
                if W > 1e-10 then
                    let varPlus = ((n - 1.0) / n) * W + (1.0 / n) * B
                    Some (sqrt (varPlus / W))
                else
                    Some 1.0  // Perfect convergence

    /// Compute effective sample size from autocorrelation
    let private effectiveSampleSize (costSeries: ResizeArray<float>) : float option =
        let n = costSeries.Count
        if n < 20 then None
        else
            let series = costSeries |> Seq.toArray
            let mean = Array.average series
            let variance = series |> Array.sumBy (fun x -> (x - mean) ** 2.0) |> fun v -> v / float n

            if variance < 1e-10 then Some (float n)
            else
                let maxLag = min (n / 3) 50

                // Compute autocorrelation function
                let autocorr lag =
                    let sum = Array.init (n - lag) (fun i -> (series.[i] - mean) * (series.[i + lag] - mean)) |> Array.sum
                    sum / (float (n - lag) * variance)

                // Sum significant positive correlations
                let mutable rhoSum = 0.0
                let mutable k = 1
                while k < maxLag do
                    let rho = autocorr k
                    if rho > 0.05 then
                        rhoSum <- rhoSum + rho
                        k <- k + 1
                    else
                        k <- maxLag  // Stop early

                let ess = float n / (1.0 + 2.0 * rhoSum)
                Some (max 1.0 ess)

    /// Coefficient of variation for recent costs
    let private coefficientOfVariation (costs: float seq) : float =
        let costArray = Seq.toArray costs
        if Array.isEmpty costArray then 1.0
        else
            let mean = Array.average costArray
            if abs mean < 1e-10 then 0.0
            else
                let variance = costArray |> Array.sumBy (fun x -> (x - mean) ** 2.0) |> fun v -> v / float costArray.Length
                let stdDev = sqrt variance
                stdDev / abs mean

    /// Check convergence with configurable strictness
    /// strictness ∈ [0,1]: 0=lenient, 1=strict
    let checkConvergence (history: ConvergenceHistory) (iteration: int) (strictness: float) : ConvergenceMetrics =
        // Compute diagnostics
        let rHat = gelmanRubin history.CostHistory

        // ESS for coldest chain (most important for final solution)
        let ess =
            if history.CostHistory.Length > 0 && history.CostHistory.[0].Count > 0 then
                effectiveSampleSize history.CostHistory.[0]
            else None

        // Recent cost stability across all chains
        let recentWindow = min 50 (history.WindowSize / 5)
        let recentCosts =
            history.CostHistory
            |> Array.collect (fun costs ->
                if costs.Count >= recentWindow then
                    costs |> Seq.skip (costs.Count - recentWindow) |> Seq.toArray
                else
                    Seq.toArray costs
            )
        let costCV = coefficientOfVariation recentCosts

        // Swap acceptance rate
        let swapRate =
            if history.SwapHistory.Count > 0 then
                float (history.SwapHistory |> Seq.filter id |> Seq.length) / float history.SwapHistory.Count
            else 0.0

        let swapQuality = swapRate >= 0.2 && swapRate <= 0.4

        // Convergence criteria with strictness scaling
        let rHatThreshold = 1.2 - strictness * 0.19      // [1.01, 1.2]
        let cvThreshold = 0.1 - strictness * 0.09         // [0.01, 0.1]

        let hasConverged =
            match rHat, ess with
            | Some r, Some e ->
                r < rHatThreshold &&
                costCV < cvThreshold &&
                e > 50.0 &&
                swapQuality
            | _ -> false

        {
            GelmanRubin = rHat
            EffectiveSampleSize = ess
            CostStabilization = costCV
            SwapAcceptanceRate = swapRate
            HasConverged = hasConverged
            Iteration = iteration
        }

    /// Run parallel tempering with convergence monitoring
    /// Returns early if convergence detected before maxIterations
    let runUntilConvergence
        (temperatures: float array)
        (initialStates: SpatialGraph<'T> array)
        (mutate: MutationStrategy<'T>)
        (cost: CostFunction<'T>)
        (maxIterations: int)
        (swapFreq: int)
        (checkFreq: int)
        (strictness: float)
        (onProgress: int -> Chain<'T> array -> ConvergenceMetrics -> unit) : (Chain<'T> array * ConvergenceMetrics) =

        let chains =
            Array.map2 (fun temp state ->
                { State = state; Cost = cost state; Temperature = temp; AcceptanceRate = 0.5 }
            ) temperatures initialStates

        let windowSize = max 200 (maxIterations / 10)  // Larger for better statistics
        let mutable history = initHistory chains.Length windowSize

        let rec iterate step (chains: Chain<'T> array) =
            if step >= maxIterations then
                let finalStatus = checkConvergence history step strictness
                onProgress step chains finalStatus
                (chains, finalStatus)
            else
                let updatedChains = Execution.Parallel.map (ParallelTempering.optimizationStep mutate cost) chains

                let mutable swapOutcomes = []
                let swappedChains =
                    if step % swapFreq = 0 then
                        let mutable chains = updatedChains
                        for i in 0 .. chains.Length - 2 do
                            let (c1, c2, accepted) = ParallelTempering.attemptSwap chains.[i] chains.[i+1]
                            chains.[i] <- c1
                            chains.[i+1] <- c2
                            swapOutcomes <- accepted :: swapOutcomes
                        chains
                    else updatedChains

                // Update convergence history
                history <- updateHistory history swappedChains swapOutcomes

                // Periodic convergence check
                if step % checkFreq = 0 && step > windowSize / 2 then
                    let status = checkConvergence history step strictness
                    onProgress step swappedChains status

                    if status.HasConverged then
                        (swappedChains, status)
                    else
                        iterate (step + 1) swappedChains
                else
                    iterate (step + 1) swappedChains

        iterate 0 chains
