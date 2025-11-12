namespace Voxel

open System
open System.Numerics
open ParallelTempering.Core

/// 3D iteration helpers for voxel operations
module VoxelIterators =

    /// Iterate over 3D grid with callback (z, y, x) order
    let inline iterate3D (resolution: int) (f: int -> int -> int -> unit) : unit =
        for z in 0 .. resolution - 1 do
            for y in 0 .. resolution - 1 do
                for x in 0 .. resolution - 1 do
                    f x y z

    /// Iterate over 2D grid with callback (z, x) order
    let inline iterate2D (resolution: int) (f: int -> int -> unit) : unit =
        for z in 0 .. resolution - 1 do
            for x in 0 .. resolution - 1 do
                f x z

    /// Collect results from 3D grid iteration
    let inline collect3D (resolution: int) (f: int -> int -> int -> 'T) : 'T list =
        [ for z in 0 .. resolution - 1 do
            for y in 0 .. resolution - 1 do
                for x in 0 .. resolution - 1 do
                    yield f x y z ]

    /// Collect results from 2D grid iteration
    let inline collect2D (resolution: int) (f: int -> int -> 'T) : 'T list =
        [ for z in 0 .. resolution - 1 do
            for x in 0 .. resolution - 1 do
                yield f x z ]

/// Material identifier for voxel rendering
type MaterialId = byte

/// Voxel data with density and material classification
type VoxelData = {
    Density: float32      // 0.0 = air, 1.0 = solid
    Material: MaterialId  // Rock, soil, snow, etc.
    Temperature: float32  // For biome calculations
    Moisture: float32     // For vegetation/erosion
}

/// Voxel chunk position in world space
type ChunkPosition = {
    X: int
    Y: int
    Z: int
}

/// Mesh data for rendering
type MeshData = {
    Vertices: Vector3 array
    Normals: Vector3 array
    UVs: Vector2 array
    Indices: int array
    MaterialIds: MaterialId array
}

/// Voxel chunk with spatial graph representation
type VoxelChunk = {
    Position: ChunkPosition
    Resolution: int                      // Voxels per axis
    Graph: SpatialGraph<VoxelData>      // Reuse PT infrastructure
    LOD: int
    Mesh: MeshData option               // Cached geometry
}

/// Chunk size constants
module ChunkSize =
    let resolution = 32                  // 32^3 = 32,768 voxels per chunk
    let worldUnits = 16.0f              // Meters per chunk
    let voxelSize = worldUnits / float32 resolution

/// Material definitions
module Materials =
    let Air = 0uy
    let Stone = 1uy
    let Soil = 2uy
    let Sand = 3uy
    let Snow = 4uy
    let Ice = 5uy
    let Grass = 6uy
    let Water = 7uy

/// Convert between chunk coordinates and world positions
module Coordinates =

    let chunkToWorld (chunk: ChunkPosition) : Vector3 =
        Vector3(
            float32 chunk.X * ChunkSize.worldUnits,
            float32 chunk.Y * ChunkSize.worldUnits,
            float32 chunk.Z * ChunkSize.worldUnits
        )

    let worldToChunk (pos: Vector3) : ChunkPosition =
        {
            X = int (floor (float pos.X / float ChunkSize.worldUnits))
            Y = int (floor (float pos.Y / float ChunkSize.worldUnits))
            Z = int (floor (float pos.Z / float ChunkSize.worldUnits))
        }

    let voxelToLocal (x: int) (y: int) (z: int) : Vector3 =
        Vector3(
            float32 x * ChunkSize.voxelSize,
            float32 y * ChunkSize.voxelSize,
            float32 z * ChunkSize.voxelSize
        )

    let voxelIndex (x: int) (y: int) (z: int) (resolution: int) : int =
        x + y * resolution + z * resolution * resolution

/// Initialize empty voxel chunk
module ChunkInit =

    let empty (resolution: int) (pos: ChunkPosition) : VoxelChunk =
        let totalVoxels = resolution * resolution * resolution

        let nodes = Array.init totalVoxels (fun _ ->
            {
                Density = 0.0f
                Material = Materials.Air
                Temperature = 20.0f
                Moisture = 0.5f
            }
        )

        let positions = Array.init totalVoxels (fun i ->
            let z = i / (resolution * resolution)
            let y = (i / resolution) % resolution
            let x = i % resolution
            Coordinates.voxelToLocal x y z
        )

        {
            Position = pos
            Resolution = resolution
            Graph = { Nodes = nodes; Positions = positions; Edges = [] }
            LOD = 0
            Mesh = None
        }

    /// Build spatial connectivity for voxel neighbors
    let withConnectivity (chunk: VoxelChunk) : VoxelChunk =
        let res = chunk.Resolution
        let edges =
            VoxelIterators.collect3D res (fun x y z ->
                let idx = Coordinates.voxelIndex x y z res
                // Connect to 6 adjacent voxels (face neighbors)
                [ if x < res - 1 then
                      let neighbor = Coordinates.voxelIndex (x + 1) y z res
                      (idx, neighbor, float ChunkSize.voxelSize)
                  if y < res - 1 then
                      let neighbor = Coordinates.voxelIndex x (y + 1) z res
                      (idx, neighbor, float ChunkSize.voxelSize)
                  if z < res - 1 then
                      let neighbor = Coordinates.voxelIndex x y (z + 1) res
                      (idx, neighbor, float ChunkSize.voxelSize) ]
            ) |> List.concat

        { chunk with Graph = { chunk.Graph with Edges = edges } }

/// Voxel density queries
module Density =

    let sample (chunk: VoxelChunk) (localPos: Vector3) : float32 =
        let res = chunk.Resolution
        let x = int (localPos.X / ChunkSize.voxelSize) |> max 0 |> min (res - 1)
        let y = int (localPos.Y / ChunkSize.voxelSize) |> max 0 |> min (res - 1)
        let z = int (localPos.Z / ChunkSize.voxelSize) |> max 0 |> min (res - 1)

        let idx = Coordinates.voxelIndex x y z res
        chunk.Graph.Nodes.[idx].Density

    let sampleTrilinear (chunk: VoxelChunk) (localPos: Vector3) : float32 =
        let res = chunk.Resolution
        let fx = localPos.X / ChunkSize.voxelSize
        let fy = localPos.Y / ChunkSize.voxelSize
        let fz = localPos.Z / ChunkSize.voxelSize

        let x0 = int (floor fx) |> max 0 |> min (res - 2)
        let y0 = int (floor fy) |> max 0 |> min (res - 2)
        let z0 = int (floor fz) |> max 0 |> min (res - 2)

        let x1 = x0 + 1
        let y1 = y0 + 1
        let z1 = z0 + 1

        let tx = fx - floor fx
        let ty = fy - floor fy
        let tz = fz - floor fz

        // Trilinear interpolation
        let d000 = chunk.Graph.Nodes.[Coordinates.voxelIndex x0 y0 z0 res].Density
        let d100 = chunk.Graph.Nodes.[Coordinates.voxelIndex x1 y0 z0 res].Density
        let d010 = chunk.Graph.Nodes.[Coordinates.voxelIndex x0 y1 z0 res].Density
        let d110 = chunk.Graph.Nodes.[Coordinates.voxelIndex x1 y1 z0 res].Density
        let d001 = chunk.Graph.Nodes.[Coordinates.voxelIndex x0 y0 z1 res].Density
        let d101 = chunk.Graph.Nodes.[Coordinates.voxelIndex x1 y0 z1 res].Density
        let d011 = chunk.Graph.Nodes.[Coordinates.voxelIndex x0 y1 z1 res].Density
        let d111 = chunk.Graph.Nodes.[Coordinates.voxelIndex x1 y1 z1 res].Density

        let c00 = d000 * (1.0f - tx) + d100 * tx
        let c01 = d001 * (1.0f - tx) + d101 * tx
        let c10 = d010 * (1.0f - tx) + d110 * tx
        let c11 = d011 * (1.0f - tx) + d111 * tx

        let c0 = c00 * (1.0f - ty) + c10 * ty
        let c1 = c01 * (1.0f - ty) + c11 * ty

        c0 * (1.0f - tz) + c1 * tz

    let setDensity (chunk: VoxelChunk) (x: int) (y: int) (z: int) (density: float32) : VoxelChunk =
        let idx = Coordinates.voxelIndex x y z chunk.Resolution
        let nodes = Array.copy chunk.Graph.Nodes
        nodes.[idx] <- { nodes.[idx] with Density = density }
        { chunk with Graph = { chunk.Graph with Nodes = nodes } }

/// Material assignment
module Material =

    let determineMaterial (density: float32) (temperature: float32) (moisture: float32) (elevation: float32) : MaterialId =
        if density < 0.5f then
            Materials.Air
        elif temperature < 0.0f then
            if moisture > 0.7f then Materials.Ice else Materials.Snow
        elif elevation < 0.3f then
            if moisture > 0.6f then Materials.Soil else Materials.Sand
        elif elevation > 0.8f then
            Materials.Stone
        else
            if moisture > 0.5f then Materials.Grass else Materials.Soil

    let assignMaterials (chunk: VoxelChunk) : VoxelChunk =
        let worldPos = Coordinates.chunkToWorld chunk.Position

        let nodes =
            Array.mapi (fun i voxel ->
                let localPos = chunk.Graph.Positions.[i]
                let globalPos = worldPos + localPos
                let elevation = globalPos.Y / 100.0f

                let material = determineMaterial voxel.Density voxel.Temperature voxel.Moisture elevation
                { voxel with Material = material }
            ) chunk.Graph.Nodes

        { chunk with Graph = { chunk.Graph with Nodes = nodes } }
