# Parallel Tempering Engine

F# implementation of parallel tempering for spatial graph optimization, voxel terrain generation, and physics simulation. Includes CUDA acceleration using sheaf-theoretic memory management patterns.

## Quick Start

```bash
dotnet run --project src/Server.fsproj

curl -X POST http://localhost:5000/api/graphs \
  -H "Content-Type: application/json" \
  -d '{"nodeCount": 20}'
```

## What This Does

**Spatial Graphs**: Generates graphs by minimizing weighted constraint violations (connectivity via BFS, edge length deviation, collision penalties, spatial distribution) across multiple temperature chains. Hot chains explore broadly, cold chains refine solutions. Periodic replica exchanges share discoveries between temperatures.

**Voxel Terrain**: 32³ voxel chunks initialized with multi-octave Perlin noise (FBM with domain warping for mountains, ridged noise for peaks), then optimized via parallel tempering using terrain-specific constraints (traversability, geological plausibility, overhang stability, cave connectivity). Post-processing applies hydraulic erosion (droplet simulation or GPU acceleration) and thermal erosion (talus angle slumping).

**Physics**: Verlet integration with semi-implicit Euler (velocity update, then position). Spring stiffness auto-calibrated from edge length variance, critical damping prevents oscillation. Collision detection uses brute force O(n²) for small graphs, switches to spatial hashing O(n) at 200 nodes.

**GPU Acceleration**: CUDA kernels for parallel tempering steps, collision detection, marching cubes mesh extraction, Verlet integration, and hydraulic erosion. Sheaf templates (`Section1D/2D/3D`) manage shared memory tiling with automatic halo region loading and bank conflict avoidance. Adaptive dispatch based on problem size with runtime performance profiling.

## Core Structure

```
src/
  ParallelTempering.Core.fs     Generic SpatialGraph<'T>, temperature ladder, Metropolis acceptance
  ProceduralGeneration.fs        Graph initialization (golden angle sphere), constraints, mutation
  Physics.fs                     Verlet integration, collision response, spring calibration
  SpatialHash.fs                 Adaptive collision (brute force vs grid hashing)
  Terrain.Generation.fs          Voxel chunk PT optimization, biome variants, LOD
  Terrain.Erosion.fs             Hydraulic (droplet/GPU) and thermal erosion
  Noise.fs                       Perlin, FBM, domain warp, curl noise for flow fields
  Voxel.Core.fs                  Chunk structure, density sampling, material assignment
  Deterministic.fs               SHA-256 seeding, fibonacci sphere, hash chains
  GPU.Wrappers.fs                CUDA bindings, marshaling, adaptive threshold calibration
  Cache.fs                       LRU memory + disk persistence
  Observability.fs               Metrics, health checks, distributed tracing
  AsyncGeneration.fs             Task-based async with progress callbacks
  Server.Production.fs           HTTP API (Giraffe), validation, caching, error handling

gpu/
  sheaf_template.cuh             Section restrict/extend, vectorized loads, warp/block reduce
  parallel_tempering.cu          PT mutations, cost computation, replica exchange
  collision_detection.cu         Broad-phase spatial checks with atomic slot claiming
  marching_cubes.cu              Isosurface extraction with trilinear interpolation
  verlet_integration.cu          Grid-stride Verlet with force accumulation
  hydraulic_erosion.cu           Erosion + sediment transport with shared memory tiling

tests/
  Core.fs, Generators.fs, Builders.fs, Statistics.fs  Test infrastructure
  Assertions.fs, PropertyDSL.fs, AlgebraicLaws.fs     Property-based testing
  TestSpecifications.fs                                Consolidated test specs
```

## Implementation Details

**Adaptive Thresholds**:
- Spatial hash: 200 nodes (naive default crossover point, then auto-calibrated from runtime profiling)
- GPU dispatch: ~10k nodes (auto-calibrated from runtime profiling)
- Temperature count: 5 (geometric spacing 0.5 to 5.0)
- Swap frequency: `max(5, sqrt(n) * 2)` (autocorrelation heuristic)

**Thread Safety**:
- ThreadSafeRandom: thread-local instances, no shared state
- Mutations: copy-on-write for immutable updates
- CUDA: persistent allocations (static vars), async streams, pinned arrays

**Caching**:
- LRU memory cache (100 entries default)
- Disk persistence with JSON serialization
- Hash-based keying: `nodeCount_iterations_seed_weights`

**CUDA Memory Management**:
- Persistent allocations across calls (static device pointers)
- Async memcpy with stream synchronization
- Pinned host arrays (GCHandle) during transfers
- Shared memory sections with halo padding

**Sheaf Templates**:
- `Section1D/2D/3D`: restrict (global → shared), extend (shared → global)
- Halo loading for neighbor access in stencils
- Bank conflict padding with configurable offset
- Vectorized loads (float2/float4) when aligned


## License

MIT
