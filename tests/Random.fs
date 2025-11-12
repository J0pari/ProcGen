namespace TestInfrastructure

module Random =

    open System
    open System.Threading

    type ThreadSafeRandom(seed: int) =
        let rng = System.Random(seed)
        let lockObj = obj()

        new() = ThreadSafeRandom(int DateTime.Now.Ticks)

        member this.Next(minValue: int, maxValue: int) : int =
            lock lockObj (fun () -> rng.Next(minValue, maxValue))

        member this.NextDouble() : float =
            lock lockObj (fun () -> rng.NextDouble())

        member this.NextGaussian(mean: float, stdDev: float) : float =
            lock lockObj (fun () ->
                // Box-Muller transform
                let u1 = rng.NextDouble()
                let u2 = rng.NextDouble()
                let randStdNormal = sqrt(-2.0 * log u1) * sin(2.0 * Math.PI * u2)
                mean + stdDev * randStdNormal)

    type RandomBuilder() =
        let mutable seed = int DateTime.Now.Ticks

        member this.WithSeed(s: int) =
            seed <- s
            this

        member this.Build() =
            ThreadSafeRandom(seed)

    let randomBuilder() = RandomBuilder()
