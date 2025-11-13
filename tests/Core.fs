namespace TestInfrastructure

module Core =

    open System
    open System.Numerics

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
