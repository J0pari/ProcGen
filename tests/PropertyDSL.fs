namespace TestInfrastructure

module PropertyDSL =

    open System
    open FsCheck
    open FsCheck.FSharp
    open global.Xunit
    open Core
    open Generators

    let forAll (arb: Arbitrary<'T>) (property: 'T -> bool) : unit =
        Check.QuickThrowOnFailure (Prop.forAll arb property)
    let forAllWith (config: FsCheck.Config) (arb: Arbitrary<'T>) (property: 'T -> bool) : unit =
        Check.One(config, Prop.forAll arb property)
    let forAllProbabilistic (arb: Arbitrary<'T>) (threshold: float) (property: 'T -> bool) : unit =
        let mutable successCount = 0
        let mutable totalCount = 0
        let prop (x: 'T) =
            totalCount <- totalCount + 1
            if property x then successCount <- successCount + 1
            true
        Check.Quick (Prop.forAll arb prop)
        let successRate = float successCount / float totalCount
        Assert.True(successRate >= threshold,
            sprintf "Property held for %.1f%% of cases, expected >= %.1f%%" (successRate * 100.0) (threshold * 100.0))
    let equivalence (arb: Arbitrary<'T>) (f1: 'T -> 'R) (f2: 'T -> 'R) : unit =
        forAll arb (fun x -> f1 x = f2 x)
    let idempotent = AlgebraicLaws.idempotenceUnary
    let roundTrip = AlgebraicLaws.roundTrip
    let commutative (arb: Arbitrary<'T>) (f: 'T -> 'T -> 'T) : unit =
        AlgebraicLaws.commutativity arb f
    let associative (arb: Arbitrary<'T>) (f: 'T -> 'T -> 'T) : unit =
        AlgebraicLaws.associativity arb f

    let forAllGraphs (property: ParallelTempering.Core.SpatialGraph<int> -> bool) : unit =
        let graphGen = Generators.genConnectedGraph 5 10 0.3 |> Gen.map Builders.toSpatialGraph
        forAll (Arb.fromGen graphGen) property

    let forAllPhysicsStates (property: Core.PhysicsState -> bool) : unit =
        let stateGen = Generators.genTestPhysicsState 2 10 20
        forAll (Arb.fromGen stateGen) property

    let forAllSeeds (property: int -> bool) : unit =
        let seedGen = Gen.choose (0, 100000)
        forAll (Arb.fromGen seedGen) property
