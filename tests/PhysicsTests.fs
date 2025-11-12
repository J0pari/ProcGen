namespace Tests

module PhysicsTests =

    open System.Numerics
    open global.Xunit
    open FsCheck
    open FsCheck.Xunit
    open TestInfrastructure.Core
    open TestInfrastructure.Assertions
    open TestInfrastructure.PropertyDSL
    open TestInfrastructure.AlgebraicLaws
    open TestInfrastructure.Builders
    open TestInfrastructure.Generators
    open TestInfrastructure.TestSpecifications
    open Physics

    [<Fact>]
    let ``Stable timestep prevents instability`` () =
        shouldBeStable (fun k m ->
            let dt = stableTimeStep k m
            dt < 2.0f / sqrt(k / m))

    [<Fact>]
    let ``Stable timestep ordering by stiffness`` () =
        let dt1 = stableTimeStep 50.0f 1.0f
        let dt2 = stableTimeStep 100.0f 1.0f
        Assert.True(dt1 > dt2)

    [<Fact>]
    let ``Fixed particles dont move`` () =
        let state = (state()).WithBody((body()).At(0.0f, 0.0f, 0.0f).Fixed().Build()).WithTimeStep(0.016f).Build()
        shouldPreserveFixed state (fun s -> integrateVerlet 0.016f s)

    [<Fact>]
    let ``Spring force direction correct`` () =
        let spring = (spring()).Between(0, 1).WithStiffness(100.0f).WithRestLength(5.0f).Build()
        let b1 = (body()).At(0.0f, 0.0f, 0.0f).Build()
        let b2 = (body()).At(10.0f, 0.0f, 0.0f).Build()
        let force = springForce spring b1 b2
        Assert.True(force.X < 0.0f)

    [<Fact>]
    let ``Momentum conservation in closed system`` () =
        // This test needs to use real Physics types, not test types
        let props = [|
            { Physics.PhysicalProperties.Mass = 1.0<Physics.kg>; Velocity = System.Numerics.Vector3(1.0f, 0.0f, 0.0f); Acceleration = System.Numerics.Vector3.Zero; Fixed = false }
            { Physics.PhysicalProperties.Mass = 1.0<Physics.kg>; Velocity = System.Numerics.Vector3(-1.0f, 0.0f, 0.0f); Acceleration = System.Numerics.Vector3.Zero; Fixed = false }
        |]
        let positions = [| System.Numerics.Vector3(0.0f, 0.0f, 0.0f); System.Numerics.Vector3(10.0f, 0.0f, 0.0f) |]
        let physState: Physics.PhysicsState = { Properties = props; Positions = positions; Springs = []; TimeStep = 0.016<Physics.s> }
        shouldConserveMomentum physState 100

    [<Fact>]
    let ``Collision detection finds overlaps`` () =
        let positions = [| System.Numerics.Vector3.Zero; System.Numerics.Vector3(0.5f, 0.0f, 0.0f) |]
        let collisions = detectCollisions 2.0f positions
        shouldDetect collisions (fun _ -> collisions |> List.exists (fun (i, j) -> (i, j) = (0, 1)))

    [<Fact>]
    let ``Collision resolution separates nodes`` () =
        let b1 = body().At(0.0f, 0.0f, 0.0f).WithMass(1.0f).Build()
        let b2 = body().At(0.5f, 0.0f, 0.0f).WithMass(1.0f).Build()
        shouldResolve b1 b2 (fun (resolved1, resolved2) ->
            let dist1 = Vector3.distanceTo resolved1.Position resolved2.Position
            let dist2 = Vector3.distanceTo b1.Position b2.Position
            dist1 > dist2)

    [<Fact>]
    let ``Critical damping formula correct`` () =
        shouldComputeCriticalDamping 100.0f 1.0f (fun c ->
            abs(c - 2.0f * sqrt(100.0f * 1.0f)) < 0.001f)

    [<Fact>]
    let ``Graph to physics conversion`` () =
        let graph = linearGraph 5
        let state = toPhysicsState graph
        Assert.Equal(5, state.Bodies.Length)
        Assert.Equal(4, state.Springs.Length)

    [<Fact>]
    let ``Endpoint fixing during relaxation`` () =
        let graph = linearGraph 10
        let state = toPhysicsState graph
        let relaxed = relax state 100
        shouldBeWithin 0.01 (float state.Bodies.[0].Position.X) (float relaxed.Bodies.[0].Position.X)
        shouldBeWithin 0.01 (float state.Bodies.[9].Position.X) (float relaxed.Bodies.[9].Position.X)

    [<Fact>]
    let ``Energy minimization during relaxation`` () =
        shouldRelax (fun graph ->
            let state = toPhysicsState graph
            let relaxed = relax state 200
            physicsCost relaxed < physicsCost state)

    [<Property>]
    let ``All computations remain finite`` () =
        forAllPhysicsStates (fun state ->
            let integrated = integrateVerlet 0.016f state
            integrated.Bodies |> Array.forall (fun b ->
                shouldBeFiniteVector b.Position && shouldBeFiniteVector b.Velocity))

    [<Fact>]
    let ``Physics scene export structure correct`` () =
        let graph = linearGraph 8
        let exported = exportPhysicsScene graph
        Assert.Equal(8, exported.RigidBodies.Length)
        Assert.Equal(7, exported.Constraints.Length)
