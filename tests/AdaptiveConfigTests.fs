namespace Tests

module AdaptiveConfigTests =

    open System
    open global.Xunit
    open FsCheck
    open FsCheck.FSharp
    open FsCheck.Xunit
    open TestInfrastructure.Core
    open TestInfrastructure.Assertions
    open TestInfrastructure.PropertyDSL
    open TestInfrastructure.AlgebraicLaws
    open TestInfrastructure.Builders
    open TestInfrastructure.Generators
    open TestInfrastructure.Performance
    open TestInfrastructure.Statistics
    open AdaptiveConfig

    [<Fact>]
    let ``Config updates take effect immediately`` () =
        TestInfrastructure.Lifecycle.withConfig GPUConfig.get GPUConfig.update (fun original ->
            let modified = { original with DeviceId = original.DeviceId + 1 }
            GPUConfig.update modified
            shouldPreserveEquality modified (GPUConfig.get()))

    [<Fact>]
    let ``Original config restored after test isolation`` () =
        let before = GPUConfig.get()
        TestInfrastructure.Lifecycle.withConfig GPUConfig.get GPUConfig.update (fun _ ->
            GPUConfig.update { before with DeviceId = 999 })
        shouldPreserveEquality before (GPUConfig.get())

    [<Fact>]
    let ``Invalid configs rejected without changing state`` () =
        let before = PhysicsConfig.get()
        shouldRejectInvalid (fun () -> PhysicsConfig.update { before with SafetyMargin = -0.5 })
        shouldPreserveEquality before (PhysicsConfig.get())

    [<Fact>]
    let ``Multiple subsystems configured independently`` () =
        TestInfrastructure.Lifecycle.withConfig GPUConfig.get GPUConfig.update (fun gpu ->
            TestInfrastructure.Lifecycle.withConfig PhysicsConfig.get PhysicsConfig.update (fun physics ->
                GPUConfig.update { gpu with DeviceId = 5 }
                PhysicsConfig.update { physics with Gravity = System.Numerics.Vector3(0.0f, -20.0f, 0.0f) }
                Assert.Equal(5, (GPUConfig.get()).DeviceId)
                Assert.Equal(-20.0f, (PhysicsConfig.get()).Gravity.Y)))

    [<Fact>]
    let ``Performance-based threshold learning finds crossover`` () =
        let measurements = [for n in 1..20 -> (n, if n < 10 then 100.0 / float n else float n * 2.0)]
        let (complexity, r2) = detectComplexity measurements
        shouldFindCrossover measurements 10

    [<Fact>]
    let ``Getter setter symmetry`` () =
        roundTrip (Arb.fromGen (FsCheck.Arb.Default.Derive<SpatialHashConfigData>().Generator))
            (fun cfg -> SpatialHashConfig.update cfg; cfg)
            (fun _ -> SpatialHashConfig.get())

    [<Fact>]
    let ``Validation SafetyMargin in range 0 to 1`` () =
        let cfg = PhysicsConfig.get()
        shouldRejectInvalid (fun () -> PhysicsConfig.update { cfg with SafetyMargin = 1.5 })
        shouldRejectInvalid (fun () -> PhysicsConfig.update { cfg with SafetyMargin = -0.1 })

    [<Fact>]
    let ``Default configs always accessible`` () =
        let defaultCfg = { DeviceId = 0; MaxThreadsPerBlock = 256; SharedMemorySize = 48 * 1024 }
        checkMonoid (Arb.fromGen (FsCheck.Arb.Default.Derive<GPUConfigData>().Generator)) {
            Semigroup = { Append = fun a b -> defaultCfg }
            Empty = defaultCfg
        }

    [<Property>]
    let ``Roundtrip symmetry for all configs`` () =
        forAll (Arb.fromGen (FsCheck.Arb.Default.Derive<GPUConfigData>().Generator)) (fun cfg ->
            TestInfrastructure.Lifecycle.withConfig GPUConfig.get GPUConfig.update (fun _ ->
                GPUConfig.update cfg)
            cfg = GPUConfig.get())

    [<Fact>]
    let ``Threshold learning fails gracefully with insufficient data`` () =
        let measurements = [for n in 1..5 -> (n, float n)]
        Assert.Throws<InvalidOperationException>(fun () -> shouldFindCrossover measurements 10 |> ignore) |> ignore
