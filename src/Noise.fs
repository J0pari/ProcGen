namespace Noise

open System
open System.Numerics

/// Fast 3D noise generation for terrain
module Field =

    /// Hash function for coordinate-based randomness
    let private hash (x: int) (y: int) (z: int) (seed: uint64) : uint64 =
        let mutable h = seed
        h <- h ^^^ (uint64 x * 0xff51afd7ed558ccdUL)
        h <- h ^^^ (h >>> 33)
        h <- h ^^^ (uint64 y * 0xc4ceb9fe1a85ec53UL)
        h <- h ^^^ (h >>> 33)
        h <- h ^^^ (uint64 z * 0xff51afd7ed558ccdUL)
        h <- h ^^^ (h >>> 33)
        h

    /// Smooth interpolation (smoothstep)
    let private smoothstep (t: float32) : float32 =
        t * t * (3.0f - 2.0f * t)

    /// Linear interpolation
    let private lerp (a: float32) (b: float32) (t: float32) : float32 =
        a + (b - a) * t

    /// Gradient vector from hash
    let private gradient (hash: uint64) : Vector3 =
        let h = int (hash &&& 15UL)
        let u = if h < 8 then 1.0f else -1.0f
        let v = if (h &&& 1) = 0 then 1.0f else -1.0f
        let w = if (h &&& 2) = 0 then 1.0f else -1.0f
        Vector3(u, v, w)

    /// Perlin-style noise
    let perlin (pos: Vector3) (seed: uint64) : float32 =
        let x0 = int (floor pos.X)
        let y0 = int (floor pos.Y)
        let z0 = int (floor pos.Z)

        let x1 = x0 + 1
        let y1 = y0 + 1
        let z1 = z0 + 1

        let fx = pos.X - floor pos.X
        let fy = pos.Y - floor pos.Y
        let fz = pos.Z - floor pos.Z

        let u = smoothstep fx
        let v = smoothstep fy
        let w = smoothstep fz

        // Corner gradients
        let g000 = gradient (hash x0 y0 z0 seed)
        let g100 = gradient (hash x1 y0 z0 seed)
        let g010 = gradient (hash x0 y1 z0 seed)
        let g110 = gradient (hash x1 y1 z0 seed)
        let g001 = gradient (hash x0 y0 z1 seed)
        let g101 = gradient (hash x1 y0 z1 seed)
        let g011 = gradient (hash x0 y1 z1 seed)
        let g111 = gradient (hash x1 y1 z1 seed)

        // Dot products with offset vectors
        let d000 = Vector3.Dot(g000, Vector3(fx, fy, fz))
        let d100 = Vector3.Dot(g100, Vector3(fx - 1.0f, fy, fz))
        let d010 = Vector3.Dot(g010, Vector3(fx, fy - 1.0f, fz))
        let d110 = Vector3.Dot(g110, Vector3(fx - 1.0f, fy - 1.0f, fz))
        let d001 = Vector3.Dot(g001, Vector3(fx, fy, fz - 1.0f))
        let d101 = Vector3.Dot(g101, Vector3(fx - 1.0f, fy, fz - 1.0f))
        let d011 = Vector3.Dot(g011, Vector3(fx, fy - 1.0f, fz - 1.0f))
        let d111 = Vector3.Dot(g111, Vector3(fx - 1.0f, fy - 1.0f, fz - 1.0f))

        // Trilinear interpolation
        let x00 = lerp d000 d100 u
        let x10 = lerp d010 d110 u
        let x01 = lerp d001 d101 u
        let x11 = lerp d011 d111 u

        let y0 = lerp x00 x10 v
        let y1 = lerp x01 x11 v

        lerp y0 y1 w

    /// Fractal Brownian Motion (multi-octave noise)
    let fbm (pos: Vector3) (seed: uint64) (octaves: int) (lacunarity: float32) (persistence: float32) : float32 =
        let mutable value = 0.0f
        let mutable amplitude = 1.0f
        let mutable frequency = 1.0f
        let mutable maxValue = 0.0f

        for _ in 1 .. octaves do
            let scaledPos = pos * frequency
            value <- value + perlin scaledPos seed * amplitude

            maxValue <- maxValue + amplitude
            amplitude <- amplitude * persistence
            frequency <- frequency * lacunarity

        value / maxValue

    /// Domain warping: warp coordinates with noise
    let warp (pos: Vector3) (seed: uint64) (amount: float32) : Vector3 =
        let offsetX = perlin pos seed * amount
        let offsetY = perlin (pos + Vector3(5.2f, 1.3f, 9.7f)) (seed + 1UL) * amount
        let offsetZ = perlin (pos + Vector3(7.1f, 4.8f, 2.6f)) (seed + 2UL) * amount

        pos + Vector3(offsetX, offsetY, offsetZ)

    /// Ridged noise for mountain ridges
    let ridged (pos: Vector3) (seed: uint64) : float32 =
        let n = perlin pos seed
        1.0f - abs n

    /// Billowy noise for clouds/rolling terrain
    let billowy (pos: Vector3) (seed: uint64) : float32 =
        abs (perlin pos seed)

/// Noise configuration presets
module Presets =

    type NoiseConfig = {
        Octaves: int
        Lacunarity: float32
        Persistence: float32
        Scale: float32
        WarpAmount: float32
    }

    let mountainous = {
        Octaves = 8
        Lacunarity = 2.2f
        Persistence = 0.5f
        Scale = 0.01f
        WarpAmount = 50.0f
    }

    let rolling = {
        Octaves = 5
        Lacunarity = 2.0f
        Persistence = 0.6f
        Scale = 0.02f
        WarpAmount = 20.0f
    }

    let plains = {
        Octaves = 3
        Lacunarity = 2.0f
        Persistence = 0.5f
        Scale = 0.03f
        WarpAmount = 5.0f
    }

    let chaotic = {
        Octaves = 10
        Lacunarity = 2.5f
        Persistence = 0.4f
        Scale = 0.008f
        WarpAmount = 100.0f
    }

/// Terrain-specific noise compositions
module Terrain =

    open Field
    open Presets

    /// Generate base terrain height
    let baseHeight (pos: Vector3) (seed: uint64) (config: NoiseConfig) : float32 =
        let warped = warp pos seed config.WarpAmount
        let scaled = warped * config.Scale
        fbm scaled seed config.Octaves config.Lacunarity config.Persistence

    /// Mountain peaks using ridged noise
    let mountains (pos: Vector3) (seed: uint64) (intensity: float32) : float32 =
        let scaled = pos * 0.005f
        let ridge = ridged scaled seed
        let detail = fbm (pos * 0.02f) (seed + 1UL) 3 2.0f 0.5f
        (ridge * 0.7f + detail * 0.3f) * intensity

    /// Erosion mask for realistic weathering
    let erosionMask (pos: Vector3) (seed: uint64) : float32 =
        // High-frequency noise for small-scale erosion
        let detail = fbm (pos * 0.1f) seed 4 2.0f 0.4f

        // Low-frequency for large drainage patterns
        let drainage = fbm (pos * 0.01f) (seed + 1UL) 2 2.0f 0.6f

        detail * 0.4f + drainage * 0.6f

    /// Cave generation using 3D worms
    let caves (pos: Vector3) (seed: uint64) (threshold: float32) : bool =
        let warpedPos = warp pos seed 10.0f
        let noise1 = perlin (warpedPos * 0.03f) seed
        let noise2 = perlin (warpedPos * 0.05f) (seed + 1UL)

        // Intersection creates worm-like caves
        let combined = noise1 * 0.6f + noise2 * 0.4f
        combined > threshold

    /// Temperature field for biome classification
    let temperature (pos: Vector3) (seed: uint64) : float32 =
        // Latitude-like gradient + noise variation
        let latitude = pos.Z * 0.01f
        let baseTemp = 1.0f - abs latitude

        let variation = fbm (pos * 0.005f) seed 3 2.0f 0.5f
        baseTemp * 0.7f + variation * 0.3f

    /// Moisture/precipitation field
    let moisture (pos: Vector3) (seed: uint64) : float32 =
        // Ocean proximity simulation + noise
        let distanceNoise = fbm (pos * 0.003f) seed 4 2.0f 0.5f
        let precipNoise = fbm (pos * 0.01f) (seed + 1UL) 3 2.0f 0.6f

        (distanceNoise * 0.5f + precipNoise * 0.5f + 1.0f) * 0.5f

/// Curl noise for vector fields (wind, water flow)
module VectorField =

    open Field

    /// Compute curl of noise field for divergence-free flow
    let curl (pos: Vector3) (seed: uint64) (epsilon: float32) : Vector3 =
        let ex = Vector3(epsilon, 0.0f, 0.0f)
        let ey = Vector3(0.0f, epsilon, 0.0f)
        let ez = Vector3(0.0f, 0.0f, epsilon)

        // Partial derivatives
        let px_y = perlin (pos + ey) seed
        let px_z = perlin (pos + ez) seed

        let py_x = perlin (pos + ex) seed
        let py_z = perlin (pos + ez) (seed + 1UL)

        let pz_x = perlin (pos + ex) (seed + 2UL)
        let pz_y = perlin (pos + ey) (seed + 2UL)

        let nx_y = perlin (pos - ey) seed
        let nx_z = perlin (pos - ez) seed

        let ny_x = perlin (pos - ex) seed
        let ny_z = perlin (pos - ez) (seed + 1UL)

        let nz_x = perlin (pos - ex) (seed + 2UL)
        let nz_y = perlin (pos - ey) (seed + 2UL)

        // Curl computation: ∇ × F
        let dx_dy = (px_y - nx_y) / (2.0f * epsilon)
        let dx_dz = (px_z - nx_z) / (2.0f * epsilon)
        let dy_dx = (py_x - ny_x) / (2.0f * epsilon)
        let dy_dz = (py_z - ny_z) / (2.0f * epsilon)
        let dz_dx = (pz_x - nz_x) / (2.0f * epsilon)
        let dz_dy = (pz_y - nz_y) / (2.0f * epsilon)

        Vector3(
            dz_dy - dy_dz,
            dx_dz - dz_dx,
            dy_dx - dx_dy
        )

    /// Generate flow field for erosion simulation
    let flowField (pos: Vector3) (seed: uint64) : Vector3 =
        let flow = curl (pos * 0.02f) seed 0.1f
        Vector3.Normalize(flow)
