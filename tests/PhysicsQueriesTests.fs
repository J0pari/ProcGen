namespace Tests

module PhysicsQueriesTests =

    open System.Numerics
    open global.Xunit
    open FsCheck
    open FsCheck.Xunit
    open TestInfrastructure.Core
    open TestInfrastructure.Assertions
    open TestInfrastructure.Builders
    open Physics.Queries

    [<Fact>]
    let ``Raycast hits nearest body`` () =
        let world = (physicsWorld()).WithBody((body()).At(0.0f, 0.0f, 5.0f).Build()).WithBody((body()).At(0.0f, 0.0f, 10.0f).Build()).Build()
        let ray = (rayBuilder()).From(0.0f, 0.0f, 0.0f).Direction(0.0f, 0.0f, 1.0f).MaxDistance(20.0f).Build()
        shouldHit world ray (fun hit -> hit.Distance < 6.0)

    [<Fact>]
    let ``Miss detection returns None`` () =
        let world = physicsWorld().WithBody(body().At(Vector3(100.0f, 0.0f, 0.0f)).Build()).Build()
        let ray = rayBuilder().From(Vector3.Zero).Direction(Vector3.UnitZ).MaxDistance(10.0f).Build()
        shouldMiss world ray

    [<Fact>]
    let ``Hit information accurate`` () =
        let world = physicsWorld().WithBody(body().At(Vector3(0.0f, 0.0f, 5.0f)).Build()).Build()
        let ray = rayBuilder().From(Vector3.Zero).Direction(Vector3.UnitZ).MaxDistance(20.0f).Build()
        match raycast world ray with
        | Some hit ->
            shouldBeWithinDistance 5.0f (float32 hit.Distance) 0.1f
            shouldBeUnitVector hit.Normal
            shouldBeValidHandle hit.BodyHandle
        | None -> Assert.True(false, "Expected hit")

    [<Fact>]
    let ``Sphere overlap detects all bodies within radius`` () =
        let world = (physicsWorld()).WithBody((body()).At(1.0f, 0.0f, 0.0f).Build()).WithBody((body()).At(2.0f, 0.0f, 0.0f).Build()).WithBody((body()).At(10.0f, 0.0f, 0.0f).Build()).Build()
        let query = (sphereQuery()).At(0.0f, 0.0f, 0.0f).WithRadius(3.0f).Build()
        shouldHitAll world query (fun bodies -> bodies.Length = 2)

    [<Fact>]
    let ``Box overlap detects AABB intersections`` () =
        let world = (physicsWorld()).WithBody((body()).At(5.0f, 5.0f, 5.0f).Build()).Build()
        let query = (boxQuery()).Min(0.0f, 0.0f, 0.0f).Max(10.0f, 10.0f, 10.0f).Build()
        shouldHitAll world query (fun bodies -> bodies.Length = 1)

    [<Fact>]
    let ``Filtering respects body type filters`` () =
        let world = (physicsWorld()).WithBody((body()).At(1.0f, 0.0f, 0.0f).Static().Build()).WithBody((body()).At(2.0f, 0.0f, 0.0f).Dynamic().Build()).Build()
        let query = (sphereQuery()).At(0.0f, 0.0f, 0.0f).WithRadius(5.0f).WithFilter((withFilter()).ExcludeStatic().Build()).Build()
        shouldHitAll world query (fun bodies -> bodies.Length = 1)

    [<Fact>]
    let ``Hit distance within max distance`` () =
        let world = physicsWorld().WithBody(body().At(Vector3(0.0f, 0.0f, 5.0f)).Build()).Build()
        let ray = rayBuilder().From(Vector3.Zero).Direction(Vector3.UnitZ).MaxDistance(20.0f).Build()
        match raycast world ray with
        | Some hit -> Assert.True(hit.Distance >= 0.0 && hit.Distance <= 20.0)
        | None -> Assert.True(false, "Expected hit")

    [<Fact>]
    let ``Empty world all queries return None or empty`` () =
        let world = physicsWorld().Build()
        shouldMiss world (rayBuilder().From(Vector3.Zero).Direction(Vector3.UnitX).Build())
        shouldHitAll world (sphereQuery().At(Vector3.Zero).WithRadius(10.0f).Build()) (fun bodies -> bodies.Length = 0)

    [<Property>]
    let ``All hit normals are unit vectors`` (origin: Vector3) (direction: Vector3) =
        direction.Length() > 0.01f ==> lazy (
            let world = physicsWorld().WithBody(body().At(Vector3(0.0f, 0.0f, 5.0f)).Build()).Build()
            let ray = rayBuilder().From(origin).Direction(Vector3.Normalize(direction)).MaxDistance(100.0f).Build()
            match raycast world ray with
            | Some hit -> abs(Vector3.Length(hit.Normal) - 1.0f) < 0.01f
            | None -> true
        )
