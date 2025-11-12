namespace ParallelTempering.Core

open System

/// Thread-safe random number generator using ThreadLocal
module ThreadSafeRandom =

    let mutable private nextSeed = 0L

    let private threadLocal = new System.Threading.ThreadLocal<Random>(fun () ->
        let seed = System.Threading.Interlocked.Increment(&nextSeed)
        Random(int seed)
    )

    /// Get thread-local Random instance
    let instance () : Random =
        threadLocal.Value

    let nextDouble () = instance().NextDouble()
    let nextInt (max: int) = instance().Next(max)
    let nextSingle () = instance().NextSingle()
