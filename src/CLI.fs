namespace CLI

open System
open System.IO
open ParallelTempering.Core
open ProceduralGeneration
open Deterministic
open Physics

module Commands =
    
    let generate (seed: string option) (nodes: int) (iterations: int) =
        let actualSeed = seed |> Option.defaultValue (Guid.NewGuid().ToString())
        
        printfn "Generating environment..."
        printfn "  Seed: %s" actualSeed
        printfn "  Nodes: %d" nodes
        printfn "  Iterations: %d" iterations
        
        let config = {
            Generator.defaultConfig with
                NodeCount = nodes
                Iterations = iterations
        }
        
        let graph = DeterministicGeneration.generate actualSeed config
        
        printfn "\nResults:"
        printfn "  Nodes: %d" graph.Nodes.Length
        printfn "  Edges: %d" graph.Edges.Length
        printfn "  Avg degree: %.2f" (float graph.Edges.Length * 2.0 / float graph.Nodes.Length)
        
        let energy = EnvironmentConstraints.build config.ConstraintWeights
        printfn "  Final energy: %.2f" (energy graph)
        
        graph
    
    let physicsRelax (graph: SpatialGraph<ProceduralGeneration.NodeType>) (steps: int) =
        printfn "\nRunning physics relaxation (%d steps)..." steps
        let relaxed = GraphPhysics.relax steps graph
        
        printfn "Physics simulation complete"
        
        relaxed
    
    let exportJson (graph: SpatialGraph<ProceduralGeneration.NodeType>) (filename: string) =
        let json = Export.toJson graph
        File.WriteAllText(filename, json)
        printfn "Exported to: %s" filename
    
    let exportPhysics (graph: SpatialGraph<ProceduralGeneration.NodeType>) (filename: string) =
        let physicsState = GraphPhysics.toPhysicsState graph
        let scene = PhysicsExport.exportScene physicsState
        let json = PhysicsExport.toJson scene
        File.WriteAllText(filename, json)
        printfn "Exported physics scene to: %s" filename
    
    let exportManifest (seed: string) (config: Generator.Config) (filename: string) =
        let manifest = Reproducibility.createManifest seed config 10
        let json = Reproducibility.toJson manifest
        File.WriteAllText(filename, json)
        printfn "Exported manifest to: %s" filename
    
    let benchmark (nodes: int) (iterations: int) (runs: int) =
        printfn "Benchmarking generation..."
        printfn "  Nodes: %d" nodes
        printfn "  Iterations: %d" iterations
        printfn "  Runs: %d" runs
        printfn ""
        
        let times = ResizeArray<float>()
        
        for run in 1 .. runs do
            let sw = System.Diagnostics.Stopwatch.StartNew()
            let config = { Generator.defaultConfig with NodeCount = nodes; Iterations = iterations }
            let _ = Generator.generate config
            sw.Stop()
            let ms = float sw.ElapsedMilliseconds
            times.Add(ms)
            printfn "  Run %d: %.0f ms" run ms
        
        let avg = if times.Count > 0 then times |> Seq.average else 0.0
        let min = if times.Count > 0 then times |> Seq.min else 0.0
        let max = if times.Count > 0 then times |> Seq.max else 0.0
        
        printfn "\nResults:"
        printfn "  Average: %.0f ms" avg
        printfn "  Min: %.0f ms" min
        printfn "  Max: %.0f ms" max
        printfn "  Iterations/sec: %.0f" (float iterations / (avg / 1000.0))

module Program =
    
    [<EntryPoint>]
    let main args =
        printfn "Procedural Environment Generator - CLI"
        printfn "======================================"
        printfn ""
        
        if args.Length = 0 then
            printfn "Usage:"
            printfn "  generate [--seed SEED] [--nodes N] [--iterations I] [--output FILE] [--physics] [--manifest]"
            printfn "  benchmark [--nodes N] [--iterations I] [--runs R]"
            printfn ""
            printfn "Examples:"
            printfn "  generate --nodes 30 --iterations 1000 --output graph.json"
            printfn "  generate --seed myseed --physics --output physics.json"
            printfn "  benchmark --nodes 50 --iterations 500 --runs 5"
            1
        else
            let command = args.[0]
            
            match command with
            | "generate" ->
                let mutable seed = None
                let mutable nodes = 20
                let mutable iterations = 500
                let mutable output = None
                let mutable usePhysics = false
                let mutable exportManifest = false
                
                let rec parseArgs i =
                    if i >= args.Length then ()
                    else
                        match args.[i] with
                        | "--seed" when i + 1 < args.Length ->
                            seed <- Some args.[i + 1]
                            parseArgs (i + 2)
                        | "--nodes" when i + 1 < args.Length ->
                            nodes <- int args.[i + 1]
                            parseArgs (i + 2)
                        | "--iterations" when i + 1 < args.Length ->
                            iterations <- int args.[i + 1]
                            parseArgs (i + 2)
                        | "--output" when i + 1 < args.Length ->
                            output <- Some args.[i + 1]
                            parseArgs (i + 2)
                        | "--physics" ->
                            usePhysics <- true
                            parseArgs (i + 1)
                        | "--manifest" ->
                            exportManifest <- true
                            parseArgs (i + 1)
                        | _ -> parseArgs (i + 1)
                
                parseArgs 1
                
                let actualSeed = seed |> Option.defaultValue (Guid.NewGuid().ToString())
                let mutable graph = Commands.generate (Some actualSeed) nodes iterations
                
                if usePhysics then
                    graph <- Commands.physicsRelax graph 100
                
                match output with
                | Some filename ->
                    if usePhysics then
                        Commands.exportPhysics graph filename
                    else
                        Commands.exportJson graph filename
                | None -> ()
                
                if exportManifest then
                    let config = { Generator.defaultConfig with NodeCount = nodes; Iterations = iterations }
                    Commands.exportManifest actualSeed config "manifest.json"
                
                0
            
            | "benchmark" ->
                let mutable nodes = 20
                let mutable iterations = 500
                let mutable runs = 3
                
                let rec parseArgs i =
                    if i >= args.Length then ()
                    else
                        match args.[i] with
                        | "--nodes" when i + 1 < args.Length ->
                            nodes <- int args.[i + 1]
                            parseArgs (i + 2)
                        | "--iterations" when i + 1 < args.Length ->
                            iterations <- int args.[i + 1]
                            parseArgs (i + 2)
                        | "--runs" when i + 1 < args.Length ->
                            runs <- int args.[i + 1]
                            parseArgs (i + 2)
                        | _ -> parseArgs (i + 1)
                
                parseArgs 1
                
                Commands.benchmark nodes iterations runs
                0
            
            | _ ->
                printfn "Unknown command: %s" command
                1
