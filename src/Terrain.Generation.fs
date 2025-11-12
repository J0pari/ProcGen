namespace Terrain

open System.Numerics
open ParallelTempering.Core
open Voxel
open Noise

/// PT-driven terrain chunk generation
module Generation =

    type PTConfig = {
        MinTemperature: float
        MaxTemperature: float
        TemperatureCount: int
        SwapFrequency: int
    }

    let defaultPTConfig = {
        MinTemperature = 0.1
        MaxTemperature = 5.0
        TemperatureCount = 6
        SwapFrequency = 50
    }

    let mutable private ptConfig = defaultPTConfig

    let updatePTConfig (newConfig: PTConfig) : unit =
        if newConfig.MinTemperature > 0.0 && newConfig.MaxTemperature > newConfig.MinTemperature &&
           newConfig.TemperatureCount > 0 && newConfig.SwapFrequency > 0 then
            ptConfig <- newConfig

    let getPTConfig () : PTConfig = ptConfig

    type NoiseGenerationConfig = {
        MountainContribution: float32  // Amplitude of mountain noise
        HeightScale: float32           // Multiplier for world height
        CaveThreshold: float32         // Cave noise threshold
        MinCaveDepth: float32          // Minimum depth for caves
        TempRange: float32             // Temperature range multiplier
        TempOffset: float32            // Temperature offset (base)
    }

    let defaultNoiseConfig = {
        MountainContribution = 0.3f
        HeightScale = 50.0f
        CaveThreshold = 0.6f
        MinCaveDepth = 5.0f
        TempRange = 40.0f
        TempOffset = -10.0f
    }

    let mutable private noiseGenConfig = defaultNoiseConfig

    let updateNoiseGenConfig (newConfig: NoiseGenerationConfig) : unit =
        if newConfig.MountainContribution >= 0.0f && newConfig.HeightScale > 0.0f &&
           newConfig.CaveThreshold >= 0.0f && newConfig.MinCaveDepth >= 0.0f &&
           newConfig.TempRange > 0.0f then
            noiseGenConfig <- newConfig

    let getNoiseGenConfig () : NoiseGenerationConfig = noiseGenConfig

    type MutationConfig = {
        DensityMutationScale: float32   // Temperature multiplier for mutation strength
        RefinementProbability: float    // Probability of small vs large mutation
        SolidToAirThreshold: float32    // Density above which to flip to air in large mutation
    }

    let defaultMutationConfig = {
        DensityMutationScale = 0.1f
        RefinementProbability = 0.7
        SolidToAirThreshold = 0.5f
    }

    let mutable private mutationConfig = defaultMutationConfig

    let updateMutationConfig (newConfig: MutationConfig) : unit =
        if newConfig.DensityMutationScale >= 0.0f &&
           newConfig.RefinementProbability >= 0.0 && newConfig.RefinementProbability <= 1.0 &&
           newConfig.SolidToAirThreshold >= 0.0f && newConfig.SolidToAirThreshold <= 1.0f then
            mutationConfig <- newConfig

    let getMutationConfig () : MutationConfig = mutationConfig

    /// Initialize chunk with noise-based density field
    let initializeFromNoise (pos: ChunkPosition) (seed: uint64) (config: Presets.NoiseConfig) : VoxelChunk =
        let chunk = ChunkInit.empty ChunkSize.resolution pos |> ChunkInit.withConnectivity
        let worldPos = Coordinates.chunkToWorld pos
        let res = chunk.Resolution

        let nodes = Array.init (res * res * res) (fun i ->
            let localPos = chunk.Graph.Positions.[i]
            let globalPos = worldPos + localPos

            // Sample multiple noise functions
            let baseHeight = Terrain.baseHeight globalPos seed config
            let mountainContribution = Terrain.mountains globalPos seed noiseGenConfig.MountainContribution
            let totalHeight = baseHeight + mountainContribution

            // Convert height to density (below terrain = solid)
            let surfaceHeight = totalHeight * noiseGenConfig.HeightScale
            let density = if globalPos.Y < surfaceHeight then 1.0f else 0.0f

            // Add caves
            let inCave = Terrain.caves globalPos (seed + 1UL) noiseGenConfig.CaveThreshold
            let finalDensity = if inCave && globalPos.Y < surfaceHeight - noiseGenConfig.MinCaveDepth then 0.0f else density

            // Environmental data
            let temperature = Terrain.temperature globalPos (seed + 2UL) * noiseGenConfig.TempRange + noiseGenConfig.TempOffset
            let moisture = Terrain.moisture globalPos (seed + 3UL)

            {
                Density = finalDensity
                Material = Materials.Stone  // Will be refined later
                Temperature = temperature
                Moisture = moisture
            }
        )

        { chunk with Graph = { chunk.Graph with Nodes = nodes } }
        |> Material.assignMaterials

    /// Mutation strategy for terrain optimization
    let mutateVoxels : MutationStrategy<VoxelData> =
        fun graph temperature ->
            let res = ChunkSize.resolution
            let totalVoxels = res * res * res

            // Select random voxel
            let idx = ThreadSafeRandom.nextInt totalVoxels
            let voxel = graph.Nodes.[idx]

            // Temperature-dependent mutation strength
            let mutationAmount = float32 temperature * mutationConfig.DensityMutationScale

            // Mutate density with bias toward geological plausibility
            let newDensity =
                if ThreadSafeRandom.nextDouble() < mutationConfig.RefinementProbability then
                    // Small perturbation (refinement)
                    voxel.Density + (ThreadSafeRandom.nextSingle() - 0.5f) * mutationAmount
                else
                    // Large jump (exploration)
                    if voxel.Density > mutationConfig.SolidToAirThreshold then 0.0f else 1.0f

            let clampedDensity = max 0.0f (min 1.0f newDensity)

            // Update moisture/temperature occasionally
            let newTemp =
                if ThreadSafeRandom.nextDouble() < 0.1 then
                    voxel.Temperature + (ThreadSafeRandom.nextSingle() - 0.5f) * 5.0f
                else
                    voxel.Temperature

            let newMoisture =
                if ThreadSafeRandom.nextDouble() < 0.1 then
                    max 0.0f (min 1.0f (voxel.Moisture + (ThreadSafeRandom.nextSingle() - 0.5f) * 0.2f))
                else
                    voxel.Moisture

            let mutated = {
                voxel with
                    Density = clampedDensity
                    Temperature = newTemp
                    Moisture = newMoisture
            }

            let newNodes = Array.copy graph.Nodes
            newNodes.[idx] <- mutated

            { graph with Nodes = newNodes }

    /// Generate optimized terrain chunk using PT
    let generate (pos: ChunkPosition) (seed: uint64) (iterations: int) : VoxelChunk =
        // Derive chunk-specific seed deterministically
        let chunkSeed = Deterministic.DeterministicGeneration.hashCoordinates seed pos.X pos.Y pos.Z

        // Initialize with noise
        let noiseConfig = Presets.mountainous
        let initial = initializeFromNoise pos chunkSeed noiseConfig

        // PT configuration
        let temperatures = ParallelTempering.geometricLadder ptConfig.MinTemperature ptConfig.MaxTemperature ptConfig.TemperatureCount
        let initialStates = Array.init temperatures.Length (fun _ -> initial.Graph)

        let costFunction = Constraints.build Constraints.defaultWeights

        // Run PT optimization
        let optimizedChains =
            ParallelTempering.run
                temperatures
                initialStates
                mutateVoxels
                costFunction
                iterations
                ptConfig.SwapFrequency

        // Extract best result
        let bestGraph = ParallelTempering.getBest optimizedChains

        { initial with Graph = bestGraph }

    /// Generate with progress callback
    let generateWithProgress
        (pos: ChunkPosition)
        (seed: uint64)
        (iterations: int)
        (onProgress: int -> Chain<VoxelData> array -> unit) : VoxelChunk =

        let chunkSeed = Deterministic.DeterministicGeneration.hashCoordinates seed pos.X pos.Y pos.Z
        let noiseConfig = Presets.mountainous
        let initial = initializeFromNoise pos chunkSeed noiseConfig

        let temperatures = ParallelTempering.geometricLadder ptConfig.MinTemperature ptConfig.MaxTemperature ptConfig.TemperatureCount
        let initialStates = Array.init temperatures.Length (fun _ -> initial.Graph)

        let costFunction = Constraints.build Constraints.defaultWeights

        let optimizedChains =
            ParallelTempering.runWithProgress
                temperatures
                initialStates
                mutateVoxels
                costFunction
                iterations
                ptConfig.SwapFrequency
                100  // Progress frequency
                onProgress

        let bestGraph = ParallelTempering.getBest optimizedChains

        { initial with Graph = bestGraph }

    /// Fast generation for distant LODs (fewer iterations)
    let generateLOD (pos: ChunkPosition) (seed: uint64) (lod: int) : VoxelChunk =
        let iterations =
            match lod with
            | 0 -> 2000  // High detail
            | 1 -> 1000
            | 2 -> 500
            | 3 -> 200
            | _ -> 100   // Very distant

        let chunk = generate pos seed iterations
        { chunk with LOD = lod }

    /// Regenerate chunk with custom constraints
    let generateCustom
        (pos: ChunkPosition)
        (seed: uint64)
        (constraints: Map<string, float>)
        (iterations: int) : VoxelChunk =

        let chunkSeed = Deterministic.DeterministicGeneration.hashCoordinates seed pos.X pos.Y pos.Z
        let noiseConfig = Presets.mountainous
        let initial = initializeFromNoise pos chunkSeed noiseConfig

        let temperatures = ParallelTempering.geometricLadder ptConfig.MinTemperature ptConfig.MaxTemperature ptConfig.TemperatureCount
        let initialStates = Array.init temperatures.Length (fun _ -> initial.Graph)

        let costFunction = Constraints.build constraints

        let optimizedChains =
            ParallelTempering.run
                temperatures
                initialStates
                mutateVoxels
                costFunction
                iterations
                ptConfig.SwapFrequency

        let bestGraph = ParallelTempering.getBest optimizedChains

        { initial with Graph = bestGraph }

/// Biome-specific generation
module Biomes =

    type BiomeType =
        | Mountains
        | Plains
        | Desert
        | Forest
        | Tundra
        | Ocean

    /// Get noise configuration for biome
    let getNoiseConfig (biome: BiomeType) : Presets.NoiseConfig =
        match biome with
        | Mountains -> Presets.mountainous
        | Plains -> Presets.rolling
        | Desert -> Presets.plains
        | Forest -> Presets.rolling
        | Tundra -> Presets.plains
        | Ocean -> { Presets.rolling with Scale = 0.005f }

    /// Get constraint weights for biome
    let getConstraints (biome: BiomeType) : Map<string, float> =
        match biome with
        | Mountains ->
            Constraints.defaultWeights
            |> Map.add "aesthetics" 10.0  // Dramatic peaks
            |> Map.add "traversability" 3.0  // Cliffs acceptable

        | Plains ->
            Constraints.defaultWeights
            |> Map.add "traversability" 12.0  // Highly traversable
            |> Map.add "aesthetics" 3.0  // Gentle rolling

        | Desert ->
            Constraints.defaultWeights
            |> Map.add "caves" 8.0  // Canyon systems
            |> Map.add "erosion" 6.0  // Wind erosion

        | Forest ->
            Constraints.defaultWeights
            |> Map.add "traversability" 8.0
            |> Map.add "moisture" 10.0  // Wet climate

        | Tundra ->
            Constraints.defaultWeights
            |> Map.add "overhang" 5.0  // Permafrost caves

        | Ocean ->
            Constraints.defaultWeights
            |> Map.add "traversability" 1.0  // Underwater
            |> Map.add "caves" 12.0  // Underwater caves

    /// Generate biome-specific chunk
    let generate (biome: BiomeType) (pos: ChunkPosition) (seed: uint64) (iterations: int) : VoxelChunk =
        let constraints = getConstraints biome
        Generation.generateCustom pos seed constraints iterations

/// LOD management
module LOD =

    /// Compute appropriate LOD based on distance
    let computeLOD (chunkPos: ChunkPosition) (viewerPos: Vector3) : int =
        let chunkWorld = Coordinates.chunkToWorld chunkPos
        let chunkCenter = chunkWorld + Vector3(ChunkSize.worldUnits / 2.0f)
        let distance = Vector3.Distance(chunkCenter, viewerPos)

        if distance < 50.0f then 0
        elif distance < 150.0f then 1
        elif distance < 300.0f then 2
        elif distance < 500.0f then 3
        else 4

    /// Generate LOD chain for smooth transitions
    let generateChain (pos: ChunkPosition) (seed: uint64) : VoxelChunk array =
        [| 0 .. 4 |]
        |> Array.map (fun lod -> Generation.generateLOD pos seed lod)

    /// Morph between LOD levels (for transition)
    let morphFactor (distance: float32) (lodSwitchDist: float32) (transitionRange: float32) : float32 =
        let t = (distance - lodSwitchDist) / transitionRange
        max 0.0f (min 1.0f t)
