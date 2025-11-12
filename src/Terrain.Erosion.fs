namespace Terrain

open System
open System.Numerics
open Voxel
open Physics
open ParallelTempering.Core
open GPU

/// Hydraulic erosion simulation using Physics.fs + GPU acceleration
module Erosion =

    /// Water droplet particle data
    type WaterDroplet = {
        Position: Vector3
        Velocity: Vector3
        Sediment: float32
        Volume: float32
    }

    /// Erosion simulation parameters
    type ErosionConfig = {
        DropletCount: int
        Iterations: int
        Inertia: float32
        SedimentCapacity: float32
        MinSlope: float32
        ErodeSpeed: float32
        DepositSpeed: float32
        EvaporateSpeed: float32
        Gravity: float32
        MaxDropletLifetime: int
    }

    let defaultConfig = {
        DropletCount = 10000
        Iterations = 1
        Inertia = 0.3f
        SedimentCapacity = 4.0f
        MinSlope = 0.01f
        ErodeSpeed = 0.3f
        DepositSpeed = 0.3f
        EvaporateSpeed = 0.01f
        Gravity = 4.0f
        MaxDropletLifetime = 30
    }

    /// Extract heightmap from chunk top surface
    let extractHeightmap (chunk: VoxelChunk) : float32 array array =
        let res = chunk.Resolution
        Array.init res (fun z ->
            Array.init res (fun x ->
                // Find top solid voxel
                let mutable height = 0.0f
                for y = res - 1 downto 0 do
                    let idx = Coordinates.voxelIndex x y z res
                    if chunk.Graph.Nodes.[idx].Density > 0.5f then
                        height <- float32 y * ChunkSize.voxelSize
                height
            )
        )

    /// Apply heightmap back to chunk
    let applyHeightmap (chunk: VoxelChunk) (heightmap: float32 array array) : VoxelChunk =
        let res = chunk.Resolution
        let nodes = Array.copy chunk.Graph.Nodes

        VoxelIterators.iterate2D res (fun x z ->
            let targetHeight = heightmap.[z].[x]

            for y in 0 .. res - 1 do
                let idx = Coordinates.voxelIndex x y z res
                let voxelHeight = float32 y * ChunkSize.voxelSize

                // Set density based on height threshold
                let newDensity =
                    if voxelHeight < targetHeight then 1.0f
                    else 0.0f

                nodes.[idx] <- { nodes.[idx] with Density = newDensity }
        )

        { chunk with Graph = { chunk.Graph with Nodes = nodes } }

    /// GPU-accelerated erosion (for large chunks)
    let erodeGPU (chunk: VoxelChunk) (config: ErosionConfig) : VoxelChunk =
        if not (Wrappers.available()) then
            chunk
        else
            let heightmap = extractHeightmap chunk
            let erosionParams = {
                GPUErosion.TimeStep = 0.016
                GPUErosion.ErosionRate = 0.3
                GPUErosion.DepositionRate = 0.1
            }
            match GPUErosion.erodeHeightmap heightmap config.Iterations erosionParams with
            | Ok eroded -> applyHeightmap chunk eroded
            | Error _ -> chunk  // Fallback to original on error

    /// CPU droplet simulation (for smaller chunks or fallback)
    let erodeCPU (chunk: VoxelChunk) (config: ErosionConfig) : VoxelChunk =
        let res = chunk.Resolution
        let heightmap = extractHeightmap chunk

        // Convert to mutable for in-place updates
        let mutableHeightmap = heightmap |> Array.map Array.copy

        for _ in 1 .. config.DropletCount do
            // Random spawn position
            let startX = ThreadSafeRandom.nextInt res
            let startZ = ThreadSafeRandom.nextInt res

            let mutable pos = Vector2(float32 startX, float32 startZ)
            let mutable dir = Vector2.Zero
            let mutable speed = 1.0f
            let mutable water = 1.0f
            let mutable sediment = 0.0f

            for step in 0 .. config.MaxDropletLifetime - 1 do
                let x = int pos.X
                let z = int pos.Y

                // Boundary check
                if x < 0 || x >= res - 1 || z < 0 || z >= res - 1 then
                    ()
                else
                    // Calculate gradient
                    let heightNW = mutableHeightmap.[z].[x]
                    let heightNE = mutableHeightmap.[z].[x + 1]
                    let heightSW = mutableHeightmap.[z + 1].[x]
                    let heightSE = mutableHeightmap.[z + 1].[x + 1]

                    let gradX = (heightNE - heightNW + heightSE - heightSW) * 0.5f
                    let gradZ = (heightSW - heightNW + heightSE - heightNE) * 0.5f

                    // Update direction and speed
                    dir <- dir * config.Inertia - Vector2(gradX, gradZ) * (1.0f - config.Inertia)

                    if dir <> Vector2.Zero then
                        dir <- Vector2.Normalize(dir)

                    let newPos = pos + dir

                    // Calculate height difference
                    let oldHeight = heightNW
                    let newX = int newPos.X |> max 0 |> min (res - 1)
                    let newZ = int newPos.Y |> max 0 |> min (res - 1)
                    let newHeight = mutableHeightmap.[newZ].[newX]

                    let heightDiff = newHeight - oldHeight

                    // Calculate sediment capacity
                    let capacity = max (-heightDiff * speed * water * config.SedimentCapacity) config.MinSlope

                    // Erode or deposit
                    if sediment > capacity || heightDiff > 0.0f then
                        // Deposit
                        let amountToDeposit =
                            if heightDiff > 0.0f then
                                min heightDiff sediment
                            else
                                (sediment - capacity) * config.DepositSpeed

                        sediment <- sediment - amountToDeposit
                        mutableHeightmap.[z].[x] <- mutableHeightmap.[z].[x] + amountToDeposit
                    else
                        // Erode
                        let amountToErode = min ((capacity - sediment) * config.ErodeSpeed) (-heightDiff)

                        mutableHeightmap.[z].[x] <- mutableHeightmap.[z].[x] - amountToErode
                        sediment <- sediment + amountToErode

                    // Update position and evaporate
                    speed <- sqrt(speed * speed + heightDiff * config.Gravity)
                    water <- water * (1.0f - config.EvaporateSpeed)
                    pos <- newPos

                    if water < 0.01f then
                        ()

        applyHeightmap chunk mutableHeightmap

    /// Adaptive erosion dispatcher
    let erode (chunk: VoxelChunk) (config: ErosionConfig) : VoxelChunk =
        let nodeCount = chunk.Graph.Nodes.Length

        // Use adaptive GPU dispatch
        if Wrappers.shouldUseGPU nodeCount then
            erodeGPU chunk config
        else
            erodeCPU chunk config

    /// Thermal erosion (talus angle-based material slumping)
    let thermalErode (chunk: VoxelChunk) (talusAngle: float32) (iterations: int) : VoxelChunk =
        let res = chunk.Resolution
        let mutable heightmap = extractHeightmap chunk

        let maxHeightDiff = ChunkSize.voxelSize * tan talusAngle

        for _ in 1 .. iterations do
            let newHeightmap = heightmap |> Array.map Array.copy

            for z in 1 .. res - 2 do
                for x in 1 .. res - 2 do
                    let center = heightmap.[z].[x]

                    // Check 4 neighbors
                    let neighbors = [
                        heightmap.[z - 1].[x]
                        heightmap.[z + 1].[x]
                        heightmap.[z].[x - 1]
                        heightmap.[z].[x + 1]
                    ]

                    let mutable totalDiff = 0.0f
                    let mutable validNeighbors = 0

                    for neighborHeight in neighbors do
                        let diff = center - neighborHeight
                        if diff > maxHeightDiff then
                            totalDiff <- totalDiff + (diff - maxHeightDiff)
                            validNeighbors <- validNeighbors + 1

                    if validNeighbors > 0 then
                        let amountToMove = totalDiff * 0.5f / float32 validNeighbors
                        newHeightmap.[z].[x] <- center - amountToMove * float32 validNeighbors

                        // Distribute to neighbors
                        if center - heightmap.[z - 1].[x] > maxHeightDiff then
                            newHeightmap.[z - 1].[x] <- heightmap.[z - 1].[x] + amountToMove
                        if center - heightmap.[z + 1].[x] > maxHeightDiff then
                            newHeightmap.[z + 1].[x] <- heightmap.[z + 1].[x] + amountToMove
                        if center - heightmap.[z].[x - 1] > maxHeightDiff then
                            newHeightmap.[z].[x - 1] <- heightmap.[z].[x - 1] + amountToMove
                        if center - heightmap.[z].[x + 1] > maxHeightDiff then
                            newHeightmap.[z].[x + 1] <- heightmap.[z].[x + 1] + amountToMove

            heightmap <- newHeightmap

        applyHeightmap chunk heightmap

    /// Combined erosion pass (hydraulic + thermal)
    let erodeRealistic (chunk: VoxelChunk) : VoxelChunk =
        let talusAngle = float32 Math.PI / 4.0f  // 45° talus angle
        chunk
        |> fun c -> erode c defaultConfig
        |> fun c -> thermalErode c talusAngle 5

/// Integration with terrain generation
module ErosionIntegration =

    /// Apply erosion after PT optimization
    let postProcessChunk (chunk: VoxelChunk) : VoxelChunk =
        if chunk.LOD = 0 then
            // Only erode high-detail chunks
            Erosion.erodeRealistic chunk
        else
            chunk

    /// Generate chunk with erosion
    let generateEroded (pos: ChunkPosition) (seed: uint64) (iterations: int) : VoxelChunk =
        let baseChunk = Generation.generate pos seed iterations
        postProcessChunk baseChunk
