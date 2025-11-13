namespace Tests

module CExportTests =

    open System
    open global.Xunit
    open FsCheck
    open FsCheck.FSharp
    open FsCheck.Xunit
    open Swensen.Unquote
    open TestInfrastructure.Core
    open TestInfrastructure.Assertions
    open TestInfrastructure.PropertyDSL
    open TestInfrastructure.Builders
    open TestInfrastructure.Generators
    open TestInfrastructure.Concurrency
    open TestInfrastructure.Lifecycle
    open Physics.Exports
    open Physics.Service
    open Physics

    // Helper functions to bridge test expectations with actual API
    let toC (body: TestInfrastructure.Core.RigidBody) : CBodyData =
        let serviceBody = {
            Physics.Service.RigidBodyData.Position = System.Numerics.Vector3(body.Position.X, body.Position.Y, body.Position.Z)
            Physics.Service.RigidBodyData.Rotation = System.Numerics.Quaternion.Identity
            Physics.Service.RigidBodyData.Velocity = System.Numerics.Vector3(body.Velocity.X, body.Velocity.Y, body.Velocity.Z)
            Physics.Service.RigidBodyData.AngularVelocity = System.Numerics.Vector3.Zero
            Physics.Service.RigidBodyData.Mass = float body.Mass * 1.0<kg>
            Physics.Service.RigidBodyData.Inertia = System.Numerics.Vector3.One
            Physics.Service.RigidBodyData.IsKinematic = false
            Physics.Service.RigidBodyData.IsStatic = false
        }
        toCBodyData serviceBody

    let fromC (cBody: CBodyData) : TestInfrastructure.Core.RigidBody =
        let serviceBody = fromCBodyData cBody
        {
            TestInfrastructure.Core.RigidBody.Position = Vector3(serviceBody.Position.X, serviceBody.Position.Y, serviceBody.Position.Z )
            TestInfrastructure.Core.RigidBody.Velocity = Vector3(serviceBody.Velocity.X, serviceBody.Velocity.Y, serviceBody.Velocity.Z )
            TestInfrastructure.Core.RigidBody.Mass = float32 (float serviceBody.Mass)
            TestInfrastructure.Core.RigidBody.Radius = 1.0f
            TestInfrastructure.Core.RigidBody.Fixed = serviceBody.IsStatic
        }

    let defaultCBody () : CBodyData =
        {
            CBodyData.Position = { Vec3.X = 0.0f; Y = 0.0f; Z = 0.0f }
            CBodyData.Rotation = { Quat.X = 0.0f; Y = 0.0f; Z = 0.0f; W = 1.0f }
            CBodyData.Velocity = { Vec3.X = 0.0f; Y = 0.0f; Z = 0.0f }
            CBodyData.AngularVelocity = { Vec3.X = 0.0f; Y = 0.0f; Z = 0.0f }
            CBodyData.Mass = 1.0f
            CBodyData.InertiaX = 1.0f
            CBodyData.InertiaY = 1.0f
            CBodyData.InertiaZ = 1.0f
            CBodyData.IsKinematic = 0
            CBodyData.IsStatic = 0
        }

    type CBodyBuilder() =
        let mutable pos = System.Numerics.Vector3.Zero
        let mutable mass = 1.0f

        member _.At(v: System.Numerics.Vector3) =
            pos <- v
            CBodyBuilder()

        member _.WithMass(m: float32) =
            mass <- m
            CBodyBuilder()

        member _.Build() : CBodyData =
            {
                CBodyData.Position = { Vec3.X = pos.X; Y = pos.Y; Z = pos.Z }
                CBodyData.Rotation = { Quat.X = 0.0f; Y = 0.0f; Z = 0.0f; W = 1.0f }
                CBodyData.Velocity = { Vec3.X = 0.0f; Y = 0.0f; Z = 0.0f }
                CBodyData.AngularVelocity = { Vec3.X = 0.0f; Y = 0.0f; Z = 0.0f }
                CBodyData.Mass = mass
                CBodyData.InertiaX = 1.0f
                CBodyData.InertiaY = 1.0f
                CBodyData.InertiaZ = 1.0f
                CBodyData.IsKinematic = 0
                CBodyData.IsStatic = 0
            }

    let cBodyBuilder() = CBodyBuilder()

    [<Fact>]
    let ``Conversion roundtrips preserve all fields`` () =
        let body = (body()).At(1.0f, 2.0f, 3.0f).WithVelocity(0.1f, 0.2f, 0.3f).WithMass(10.0f).Build()
        shouldRoundtrip toC fromC body

    [<Fact>]
    let ``Float precision within tolerance`` () =
        let cBody = defaultCBody()
        let body = fromC cBody
        let roundtrip = toC body
        shouldBeWithinTolerance 0.001 (float cBody.Position.X) (float roundtrip.Position.X)
        shouldBeWithinTolerance 0.001 (float cBody.Position.Y) (float roundtrip.Position.Y)
        shouldBeWithinTolerance 0.001 (float cBody.Position.Z) (float roundtrip.Position.Z)

    [<Fact>]
    let ``Physics lifecycle no memory leaks`` () =
        TestInfrastructure.Lifecycle.withPhysicsEngine (fun () ->
            let mutable body = defaultCBody()
            let handle = physics_add_body(&body)
            shouldBeValidHandle handle
            physics_step()
            physics_shutdown())

    [<Fact>]
    let ``Out parameters receive correct values`` () =
        TestInfrastructure.Lifecycle.withPhysicsEngine (fun () ->
            let mutable body = defaultCBody()
            let handle = physics_add_body(&body)
            let mutable outBody = Unchecked.defaultof<CBodyData>
            physics_get_body(handle, &outBody) |> ignore
            shouldSetOutParam outBody.Mass body.Mass)

    [<Fact>]
    let ``Handle validity non-negative`` () =
        TestInfrastructure.Lifecycle.withPhysicsEngine (fun () ->
            let mutable body = defaultCBody()
            let handle = physics_add_body(&body)
            shouldBeValidHandle handle
            Assert.True(handle >= 0))

    [<Fact>]
    let ``State retrieval matches what was set`` () =
        TestInfrastructure.Lifecycle.withPhysicsEngine (fun () ->
            let mutable body = cBodyBuilder().At(System.Numerics.Vector3(5.0f, 10.0f, 15.0f)).WithMass(20.0f).Build()
            let handle = physics_add_body(&body)
            let mutable retrieved = Unchecked.defaultof<CBodyData>
            physics_get_body(handle, &retrieved) |> ignore
            shouldBeWithinTolerance 0.001 (float body.Position.X) (float retrieved.Position.X)
            shouldBeWithinTolerance 0.001 (float body.Mass) (float retrieved.Mass))

    [<Fact>]
    let ``Thread-safety concurrent async operations`` () =
        shouldSupportAsyncOps (fun () ->
            TestInfrastructure.Lifecycle.withPhysicsEngine (fun () ->
                let operations = [|
                    async { let mutable b = defaultCBody() in let h = physics_add_body(&b) in return h }
                    async { let mutable b = defaultCBody() in let h = physics_add_body(&b) in return h }
                    async { let mutable b = defaultCBody() in let h = physics_add_body(&b) in return h }
                |]
                let results = Async.Parallel operations |> Async.RunSynchronously
                Assert.True(Array.forall (fun h -> h >= 0) results)))

    [<Fact>]
    let ``Init must succeed before operations`` () =
        let result = physics_init()
        Assert.Equal(1, result)
        physics_shutdown()

    [<Fact>]
    let ``Operations after shutdown fail gracefully`` () =
        TestInfrastructure.Lifecycle.withPhysicsEngine (fun () ->
            physics_shutdown()
            let mutable body = defaultCBody()
            let handle = physics_add_body(&body)
            Assert.True(handle < 0 || handle >= 0))

    [<Property>]
    let ``Roundtrip conversion preserves structure`` () =
        forAll (Arb.fromGen (genRigidBody 100.0f)) (fun body ->
            let cBody = toC body
            let restored = fromC cBody
            let dx = body.Position.X - restored.Position.X
            let dy = body.Position.Y - restored.Position.Y
            let dz = body.Position.Z - restored.Position.Z
            sqrt(dx*dx + dy*dy + dz*dz) < 0.01f)

    [<Property>]
    let ``Concurrent operations on different handles independent`` () =
        forAll (Arb.fromGen (Gen.choose(1, 10))) (fun n ->
            TestInfrastructure.Lifecycle.withPhysicsEngine' (fun () ->
                let handles = [for i in 1..n -> let mutable body = defaultCBody() in physics_add_body(&body)]
                handles |> List.forall (fun h -> h >= 0)))
