namespace Tests

module PhysicsServiceTests =

    open System.Numerics
    open global.Xunit
    open FsCheck.Xunit
    open TestInfrastructure.Core
    open TestInfrastructure.Assertions
    open TestInfrastructure.Builders
    open TestInfrastructure.Statistics
    open TestInfrastructure.Generators
    open Physics.Service

    [<Fact>]
    let ``Add and remove bodies by handle`` () =
        TestInfrastructure.Lifecycle.withWorld (fun world ->
            let bodyData = body().At(Vector3.Zero).BuildData()
            let handle = world.AddRigidBody(bodyData)
            shouldBeValidHandle handle
            shouldRetrieveBody world handle (fun retrieved -> retrieved.IsSome)
            world.RemoveRigidBody(handle)
            shouldRetrieveBody world handle (fun retrieved -> retrieved.IsNone))

    [<Fact>]
    let ``Handle validity non-negative`` () =
        TestInfrastructure.Lifecycle.withWorld (fun world ->
            let handle = world.AddRigidBody(body().BuildData())
            Assert.True(handle >= 0))

    [<Fact>]
    let ``Gravity dynamic bodies fall static remain fixed`` () =
        TestInfrastructure.Lifecycle.withWorld (fun world ->
            let staticHandle = world.AddRigidBody(body().At(Vector3.Zero).Static().BuildData())
            let dynamicHandle = world.AddRigidBody(body().At(Vector3(5.0f, 0.0f, 0.0f)).Dynamic().BuildData())
            stepN world 10
            shouldRespectBodyType world staticHandle (fun body -> body.Position.Y = 0.0f)
            shouldRespectBodyType world dynamicHandle (fun body -> body.Velocity.Y < 0.0f))

    [<Fact>]
    let ``Static bodies never move`` () =
        TestInfrastructure.Lifecycle.withWorld (fun world ->
            let handle = world.AddRigidBody(body().At(Vector3.Zero).Static().BuildData())
            stepN world 100
            shouldRetrieveBody world handle (fun body ->
                match body with
                | Some b -> b.Position = Vector3.Zero
                | None -> false))

    [<Fact>]
    let ``Kinematic bodies dont respond to forces`` () =
        TestInfrastructure.Lifecycle.withWorld (fun world ->
            let handle = world.AddRigidBody(body().At(Vector3.Zero).Kinematic().BuildData())
            world.ApplyImpulse(handle, Vector3(100.0f, 0.0f, 0.0f))
            stepN world 10
            shouldRetrieveBody world handle (fun body ->
                match body with
                | Some b -> b.Velocity = Vector3.Zero
                | None -> false))

    [<Fact>]
    let ``ApplyImpulse changes velocity`` () =
        TestInfrastructure.Lifecycle.withWorld (fun world ->
            let body = body().At(Vector3.Zero).WithMass(2.0f).BuildData()
            let handle = world.AddRigidBody(body)
            let impulse = Vector3(10.0f, 0.0f, 0.0f)
            world.ApplyImpulse(handle, impulse)
            shouldRetrieveBody world handle (fun retrieved ->
                match retrieved with
                | Some b -> abs(b.Velocity.X - 5.0f) < 0.01f
                | None -> false))

    [<Fact>]
    let ``Collision detection returns collision pairs`` () =
        TestInfrastructure.Lifecycle.withWorld (fun world ->
            world.AddRigidBody(body().At(Vector3.Zero).BuildData()) |> ignore
            world.AddRigidBody(body().At(Vector3(0.5f, 0.0f, 0.0f)).BuildData()) |> ignore
            stepN world 1
            shouldDetectCollision world (fun events -> events.Length > 0))

    [<Fact>]
    let ``Sleeping low velocity bodies sleep`` () =
        TestInfrastructure.Lifecycle.withWorld (fun world ->
            let handle = world.AddRigidBody(body().At(Vector3.Zero).WithVelocity(Vector3(0.01f, 0.0f, 0.0f)).BuildData())
            stepN world 50
            shouldSleep world handle)

    [<Fact>]
    let ``Statistics tracked accurately`` () =
        TestInfrastructure.Lifecycle.withWorld (fun world ->
            world.AddRigidBody(body().BuildData()) |> ignore
            world.AddRigidBody(body().BuildData()) |> ignore
            stepN world 10
            shouldTrack world (fun stats ->
                stats.FrameCount = 10 && stats.BodyCount = 2))

    [<Fact>]
    let ``Clear removes all bodies resets frame count`` () =
        TestInfrastructure.Lifecycle.withWorld (fun world ->
            world.AddRigidBody(body().BuildData()) |> ignore
            stepN world 5
            world.Clear()
            shouldTrack world (fun stats -> stats.BodyCount = 0 && stats.FrameCount = 0))

    [<Property>]
    let ``Frame count equals step count`` (steps: PositiveInt) =
        let n = min 100 (int steps)
        TestInfrastructure.Lifecycle.withWorld (fun world ->
            stepN world n
            shouldTrack world (fun stats -> stats.FrameCount = n))
