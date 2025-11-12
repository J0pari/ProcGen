namespace Terrain

open System.Numerics
open ParallelTempering.Core
open Voxel

/// Terrain-specific constraints reusing existing PT infrastructure
module Constraints =

    /// Traversability: reuses connectivity constraint logic
    /// Steep slopes or disconnected regions = high cost
    let traversability (chunk: VoxelChunk) : float =
        let graph = chunk.Graph

        // Build traversability graph: voxels connected if slope < 45°
        let traversableEdges =
            graph.Edges
            |> List.filter (fun (i, j, _) ->
                let posI = graph.Positions.[i]
                let posJ = graph.Positions.[j]
                let heightDiff = abs (posI.Y - posJ.Y)
                let horizontalDist = Vector2.Distance(Vector2(posI.X, posI.Z), Vector2(posJ.X, posJ.Z))

                // Slope < 45° = traversable
                heightDiff / horizontalDist < 1.0f
            )

        let traversableGraph = { graph with Edges = traversableEdges }

        // Reuse existing connectivity BFS
        ParallelTempering.Core.Constraints.connectivity traversableGraph

    /// Geological plausibility: smooth density transitions
    /// Reuses edgeLength constraint - abrupt changes = high cost
    let geologicalPlausibility (chunk: VoxelChunk) : float =
        let graph = chunk.Graph

        ParallelTempering.Core.SpatialGraphUtils.sumEdgeNodePairs graph (fun nodeI nodeJ _ _ ->
            let diff = abs (nodeI.Density - nodeJ.Density)
            float (diff * diff)
        )

    /// Overhang stability: penalize floating voxels
    /// Reuses collision detection logic
    let overhangPenalty (chunk: VoxelChunk) : float =
        let graph = chunk.Graph
        let res = chunk.Resolution

        let mutable penalty = 0.0

        VoxelIterators.iterate2D res (fun x z ->
            for y in 1 .. res - 1 do
                let idx = Coordinates.voxelIndex x y z res
                let below = Coordinates.voxelIndex x (y - 1) z res

                let voxel = graph.Nodes.[idx]
                let supporting = graph.Nodes.[below]

                // Solid voxel with air below = overhang
                if voxel.Density > 0.5f && supporting.Density < 0.5f then
                    penalty <- penalty + float voxel.Density * 10.0
        )

        penalty

    /// Erosion realism: exposed surfaces should show weathering
    let erosionRealism (chunk: VoxelChunk) : float =
        let graph = chunk.Graph
        let res = chunk.Resolution

        let mutable cost = 0.0

        for z in 1 .. res - 2 do
            for y in 1 .. res - 2 do
                for x in 1 .. res - 2 do
                    let idx = Coordinates.voxelIndex x y z res
                    let voxel = graph.Nodes.[idx]

                    if voxel.Density > 0.5f then
                        // Count air-adjacent faces
                        let neighbors = [
                            Coordinates.voxelIndex (x - 1) y z res
                            Coordinates.voxelIndex (x + 1) y z res
                            Coordinates.voxelIndex x (y - 1) z res
                            Coordinates.voxelIndex x (y + 1) z res
                            Coordinates.voxelIndex x y (z - 1) res
                            Coordinates.voxelIndex x y (z + 1) res
                        ]

                        let exposedFaces =
                            neighbors
                            |> List.filter (fun n -> graph.Nodes.[n].Density < 0.5f)
                            |> List.length

                        // Exposed surfaces should have higher moisture
                        if exposedFaces > 0 && voxel.Moisture < 0.3f then
                            cost <- cost + float exposedFaces * 0.5

        cost

    /// Aesthetic quality: rule of thirds, interesting silhouettes
    /// Reuses distribution constraint
    let aestheticQuality (chunk: VoxelChunk) : float =
        let graph = chunk.Graph

        // Penalize uniform distribution - want interesting peaks/valleys
        let solidPositions =
            graph.Nodes
            |> Array.zip graph.Positions
            |> Array.filter (fun (_, voxel) -> voxel.Density > 0.5f)
            |> Array.map fst

        if solidPositions.Length = 0 then
            0.0
        else
            // Calculate variance in height distribution
            let heights = solidPositions |> Array.map (fun p -> p.Y)
            let avgHeight = Array.average heights
            let variance =
                heights
                |> Array.averageBy (fun h -> (h - avgHeight) ** 2.0f)

            // Low variance = boring flat terrain
            if variance < 1.0f then
                10.0 * float (1.0f - variance)
            else
                0.0

    /// Cave network connectivity: ensures explorable underground
    let caveConnectivity (chunk: VoxelChunk) : float =
        let graph = chunk.Graph
        let res = chunk.Resolution

        // Find air pockets below ground level
        let undergroundAir =
            VoxelIterators.collect3D res (fun x y z ->
                if y <= res / 2 then  // Below midpoint
                    let idx = Coordinates.voxelIndex x y z res
                    if graph.Nodes.[idx].Density < 0.5f then [idx] else []
                else []
            ) |> List.concat

        if undergroundAir.IsEmpty then
            0.0  // No caves = acceptable
        else
            // Build air-only connectivity graph
            let airEdges =
                graph.Edges
                |> List.filter (fun (i, j, _) ->
                    graph.Nodes.[i].Density < 0.5f &&
                    graph.Nodes.[j].Density < 0.5f
                )

            let airGraph = { graph with Edges = airEdges }

            // Penalize disconnected cave systems
            ParallelTempering.Core.Constraints.connectivity airGraph * 0.5

    /// Performance profile: polygon count estimation
    let performanceCost (chunk: VoxelChunk) : float =
        let graph = chunk.Graph
        let res = chunk.Resolution

        // Estimate triangle count from surface voxels
        let mutable surfaceVoxels = 0

        for z in 1 .. res - 2 do
            for y in 1 .. res - 2 do
                for x in 1 .. res - 2 do
                    let idx = Coordinates.voxelIndex x y z res
                    let voxel = graph.Nodes.[idx]

                    if voxel.Density > 0.5f then
                        // Check if any neighbor is air (surface voxel)
                        let neighbors = [
                            Coordinates.voxelIndex (x - 1) y z res
                            Coordinates.voxelIndex (x + 1) y z res
                            Coordinates.voxelIndex x (y - 1) z res
                            Coordinates.voxelIndex x (y + 1) z res
                            Coordinates.voxelIndex x y (z - 1) res
                            Coordinates.voxelIndex x y (z + 1) res
                        ]

                        let hasAirNeighbor =
                            neighbors
                            |> List.exists (fun n -> graph.Nodes.[n].Density < 0.5f)

                        if hasAirNeighbor then
                            surfaceVoxels <- surfaceVoxels + 1

        // ~2 triangles per surface voxel face
        // Penalize if exceeds budget (e.g., 10k triangles per chunk)
        let estimatedTriangles = surfaceVoxels * 12
        let triangleBudget = 10000

        if estimatedTriangles > triangleBudget then
            float (estimatedTriangles - triangleBudget) * 0.001
        else
            0.0

    /// Biome transition smoothness: material boundaries should blend
    let biomeTransitionCost (chunk: VoxelChunk) : float =
        let graph = chunk.Graph

        ParallelTempering.Core.SpatialGraphUtils.sumEdgeNodePairs graph (fun nodeI nodeJ _ _ ->
            if nodeI.Material <> nodeJ.Material then
                if nodeI.Density > 0.5f && nodeJ.Density > 0.5f then 1.0
                else 0.1
            else 0.0
        )

    /// Build complete terrain cost function
    let build (weights: Map<string, float>) : SpatialGraph<VoxelData> -> float =
        fun graph ->
            // Wrap graph in chunk structure for constraint evaluation
            let chunk = {
                Position = { X = 0; Y = 0; Z = 0 }
                Resolution = ChunkSize.resolution
                Graph = graph
                LOD = 0
                Mesh = None
            }

            let costs = [
                weights.["traversability"], traversability chunk
                weights.["geological"], geologicalPlausibility chunk
                weights.["overhang"], overhangPenalty chunk
                weights.["erosion"], erosionRealism chunk
                weights.["aesthetics"], aestheticQuality chunk
                weights.["caves"], caveConnectivity chunk
                weights.["performance"], performanceCost chunk
                weights.["biomeTransition"], biomeTransitionCost chunk
            ]

            costs |> List.sumBy (fun (w, c) -> w * c)

    /// Default terrain constraint weights
    let defaultWeights = Map.ofList [
        "traversability", 8.0      // High priority: playable terrain
        "geological", 3.0          // Medium: believable geology
        "overhang", 12.0           // Critical: no floating islands
        "erosion", 2.0             // Low: nice to have
        "aesthetics", 6.0          // High: memorable landscapes
        "caves", 4.0               // Medium: exploration value
        "performance", 10.0        // Critical: maintain framerate
        "biomeTransition", 2.0     // Low: polish
    ]
