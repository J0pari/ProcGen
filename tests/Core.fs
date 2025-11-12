namespace TestInfrastructure

module Core =

    open System
    open System.Numerics

    type Vector3 = {
        X: float32
        Y: float32
        Z: float32
    }

    module Vector3 =
        let zero = { X = 0.0f; Y = 0.0f; Z = 0.0f }
        let unitX = { X = 1.0f; Y = 0.0f; Z = 0.0f }
        let unitY = { X = 0.0f; Y = 1.0f; Z = 0.0f }
        let unitZ = { X = 0.0f; Y = 0.0f; Z = 1.0f }

        let magnitude v = sqrt (v.X * v.X + v.Y * v.Y + v.Z * v.Z)
        let normalize v =
            let mag = magnitude v
            if mag > 0.001f then { X = v.X / mag; Y = v.Y / mag; Z = v.Z / mag }
            else unitX

        let dot v1 v2 = v1.X * v2.X + v1.Y * v2.Y + v1.Z * v2.Z

        let cross v1 v2 = {
            X = v1.Y * v2.Z - v1.Z * v2.Y
            Y = v1.Z * v2.X - v1.X * v2.Z
            Z = v1.X * v2.Y - v1.Y * v2.X
        }

        let distanceTo v1 v2 =
            let dx = v1.X - v2.X
            let dy = v1.Y - v2.Y
            let dz = v1.Z - v2.Z
            sqrt (dx*dx + dy*dy + dz*dz)

    type GraphNode = int

    type Edge = {
        From: GraphNode
        To: GraphNode
        Weight: float
    }

    type Graph = {
        Nodes: Set<GraphNode>
        Edges: Edge list
    }

    module Graph =
        let countReachableFrom (nodeCount: int) (edges: (int * int * 'T) list) (startNode: int) : int =
            if nodeCount <= 0 || startNode < 0 || startNode >= nodeCount then 0
            else
                let visited = Array.create nodeCount false
                let queue = System.Collections.Generic.Queue<int>()
                queue.Enqueue(startNode)
                visited.[startNode] <- true

                while queue.Count > 0 do
                    let node = queue.Dequeue()
                    edges
                    |> List.iter (fun (a, b, _) ->
                        let neighbor = if a = node then b elif b = node then -1 else -1
                        if neighbor >= 0 && not visited.[neighbor] then
                            visited.[neighbor] <- true
                            queue.Enqueue(neighbor)
                    )

                visited |> Array.filter id |> Array.length

    type RigidBody = {
        Position: Vector3
        Velocity: Vector3
        Mass: float32
        Radius: float32
        Fixed: bool
    }

    type Spring = {
        BodyA: int
        BodyB: int
        RestLength: float32
        Stiffness: float32
        Damping: float32
    }

    type PhysicsState = {
        Bodies: RigidBody array
        Springs: Spring list
        TimeStep: float32
        Gravity: Vector3
    }

    type Config = {
        ThreadCount: int
        BufferSize: int
        Timeout: TimeSpan
        EnableCache: bool
    }

    type CacheEntry<'K, 'V when 'K: comparison> = {
        Key: 'K
        Value: 'V
        AccessCount: int
        LastAccess: DateTime
    }

    type Temperature = Temperature of float

    module Temperature =
        let value (Temperature t) = t
        let create t = if t > 0.0 then Temperature t else failwith "Temperature must be positive"
