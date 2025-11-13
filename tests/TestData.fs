namespace TestInfrastructure

module TestData =

    open System.Numerics
    open Core

    let acceptanceScenarios = [
        (100.0, 95.0, 10.0, 1.0, 0.05)
        (50.0, 40.0, 1.0, 1.0, 0.05)
        (100.0, 105.0, 10.0, 0.606530659712633, 0.05)
        (50.0, 60.0, 10.0, 0.367879441171442, 0.05)
        (100.0, 105.0, 5.0, 0.367879441171442, 0.05)
        (50.0, 60.0, 5.0, 0.135335283236613, 0.05)
        (100.0, 105.0, 1.0, 0.006737946999085, 0.01)
        (50.0, 60.0, 1.0, 0.000045399929762, 0.001)
        (100.0, 100.0, 1.0, 1.0, 0.05)
        (100.0, 105.0, 0.1, 1.0e-217, 0.001)
    ]

    let temperatureLadders = [
        (0.5, 5.0, 4)
        (1.0, 10.0, 8)
        (0.1, 100.0, 16)
    ]

    type CollisionCase = {
        MinDistance: float32
        Pos1: Core.Vector3
        Pos2: Core.Vector3
        ShouldCollide: bool
        ExpectedDistance: float32
    }

    let collisionCases = [
        { MinDistance = 1.0f; Pos1 = Vector3.Zero; Pos2 = Vector3(10.0f, 0.0f, 0.0f )
          ShouldCollide = false; ExpectedDistance = 10.0f }
        { MinDistance = 1.0f; Pos1 = Vector3.Zero; Pos2 = Vector3(0.5f, 0.0f, 0.0f )
          ShouldCollide = true; ExpectedDistance = 0.5f }
        { MinDistance = 1.0f; Pos1 = Vector3.Zero; Pos2 = Vector3(1.0f, 0.0f, 0.0f )
          ShouldCollide = false; ExpectedDistance = 1.0f }
        { MinDistance = 2.0f; Pos1 = Vector3.Zero; Pos2 = Vector3(1.0f, 1.0f, 1.0f )
          ShouldCollide = true; ExpectedDistance = 1.732f }
    ]
