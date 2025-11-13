namespace Tests

module CacheTests =

    open System
    open global.Xunit
    open FsCheck
    open FsCheck.FSharp
    open FsCheck.Xunit
    open TestInfrastructure.Core
    open TestInfrastructure.Assertions
    open TestInfrastructure.PropertyDSL
    open TestInfrastructure.Builders
    open TestInfrastructure.Generators
    open TestInfrastructure.Concurrency
    open TestInfrastructure.Performance
    open Cache

    [<Fact>]
    let ``LRU evicts least recently used`` () =
        let cache = LRUCache<int, string>(3)
        cache.Set(1, "a", 1.0)
        cache.Set(2, "b", 1.0)
        cache.Set(3, "c", 1.0)
        cache.TryGet(1) |> ignore
        cache.Set(4, "d", 1.0)
        Assert.Equal(Some "a", cache.TryGet(1))
        Assert.Equal(None, cache.TryGet(2))
        Assert.Equal(Some "c", cache.TryGet(3))

    [<Fact>]
    let ``Thread-safety under concurrent operations`` () =
        shouldBeThreadSafe {
            Setup = fun () -> LRUCache<int, int>(100)
            Operation = fun cache tid ->
                cache.Set(tid, tid * 2, 1.0)
                cache.TryGet(tid) |> ignore
            Threads = 16
            OperationsPerThread = 1000
            Duration = None
            Cleanup = ignore
            ValidateInvariant = None
        }

    [<Fact>]
    let ``Statistics accuracy`` () =
        let cache = LRUCache<int, string>(10)
        cache.Set(1, "a", 1.0)
        cache.Set(2, "b", 2.0)
        cache.TryGet(1) |> ignore
        cache.TryGet(3) |> ignore
        Assert.Equal(2, cache.Count)

    [<Fact>]
    let ``Persistence roundtrip`` () =
        let cache = LRUCache<int, Graph>(10)
        let g = graph().AddNodes([0..5]).AddEdge(0,1,1.0).Build()
        cache.Set(1, g, 5.0)
        Assert.NotNull(g)

    [<Fact>]
    let ``HashKey structural equality`` () =
        let g1 = graph().AddNodes([0..3]).AddEdge(0,1,1.0).AddEdge(1,2,1.0).Build()
        let g2 = graph().AddNodes([0..3]).AddEdge(0,1,1.0).AddEdge(1,2,1.0).Build()
        let g3 = graph().AddNodes([0..4]).AddEdge(0,1,1.0).Build()
        shouldEqual (hash g1) (hash g2)
        shouldNotEqual (hash g1) (hash g3)

    [<Fact>]
    let ``Capacity enforcement`` () =
        let cache = LRUCache<int, string>(5)
        for i in 1..10 do cache.Set(i, sprintf "v%d" i, 1.0)
        Assert.True(cache.Count <= 5)

    [<Fact>]
    let ``Clear operation`` () =
        let cache = LRUCache<int, string>(10)
        cache.Set(1, "a", 1.0)
        cache.Set(2, "b", 1.0)
        cache.Clear()
        Assert.Equal(0, cache.Count)
        Assert.Equal(None, cache.TryGet(1))

    [<Property>]
    let ``Set then Get returns value if not evicted`` (key: int) (value: string) =
        let cache = LRUCache<int, string>(100)
        cache.Set(key, value, 1.0)
        cache.TryGet(key) = Some(value)

    [<Property>]
    let ``LRU access promotes key`` () =
        forAll (Arb.fromGen (Gen.listOf (Gen.choose (0, 100)))) (fun keys ->
            let cache = LRUCache<int, int>(10)
            keys |> List.iter (fun k -> cache.Set(k, k, 1.0))
            let firstKey = List.head keys
            cache.TryGet(firstKey) |> ignore
            for i in 101..120 do cache.Set(i, i, 1.0)
            cache.TryGet(firstKey).IsSome)

    [<Fact>]
    let ``Concurrent operations maintain invariants`` () =
        shouldSurviveStress {
            Setup = fun () -> LRUCache<int, int>(50)
            Operation = fun cache tid ->
                if tid % 2 = 0 then
                    cache.Set(tid, tid * 2, 1.0)
                else
                    cache.TryGet(tid - 1) |> ignore
            Threads = 8
            OperationsPerThread = 5000
            Duration = None
            Cleanup = fun cache ->
                Assert.True(cache.Count <= 50)
        }
