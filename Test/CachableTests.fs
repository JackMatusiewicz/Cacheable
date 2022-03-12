namespace Cacheable.Test

open NUnit.Framework
open Cacheable

[<TestFixture>]
module CacheableTest =

    [<Test>]
    let ``Caching reset works correctly - single function`` () =
        let mutable ctr = 0

        let add a b =
            ctr <- ctr + 1
            a + b

        let ev = Event<unit> ()
        
        let memoisedAdd =
            Cacheable.func add ev.Publish CachingStrategy.All
            |> Cacheable.convert

        memoisedAdd 1 2 |> ignore
        memoisedAdd 1 2 |> ignore

        Assert.That(ctr, Is.EqualTo 1)

        ev.Trigger ()
        memoisedAdd 1 2 |> ignore
        memoisedAdd 1 2 |> ignore

        Assert.That(ctr, Is.EqualTo 2)

    [<Test>]
    let ``Caching reset works correctly - higher order function`` () =
        let mutable ctr = 0

        let add a b =
            ctr <- ctr + 1
            a + b

        let mutable ctr2 = 0

        let double (f : int -> int -> int) a =
            ctr2 <- ctr2 + 1
            f a a

        let ev = Event<unit> ()

        let liftedDouble =
            Cacheable.lift
                double
                CachingStrategy.All

        let memoisedAdd =
            Cacheable.func add ev.Publish CachingStrategy.All
            |> fun f -> Cacheable.applyArg f liftedDouble
            |> Cacheable.convert

        memoisedAdd 1 |> ignore
        memoisedAdd 1 |> ignore
        
        Assert.That(ctr, Is.EqualTo 1)
        Assert.That(ctr2, Is.EqualTo 1)

        ev.Trigger ()
        memoisedAdd 1 |> ignore
        memoisedAdd 1 |> ignore

        Assert.That(ctr, Is.EqualTo 2)
        Assert.That(ctr2, Is.EqualTo 2)

    [<Test>]
    let ``Caching reset works correctly - two input functions`` () =
        let mutable ctr1 = 0

        let add a b =
            ctr1 <- ctr1 + 1
            a + b

        let mutable ctr2 = 0
        let mul a b =
            ctr2 <- ctr2 + 1
            a * b

        let mutable ctr3 = 0
        let combiner
            (f : int -> int -> int)
            (g : int -> int -> int)
            (a : int)
            : int * int
            =
            ctr3 <- ctr3 + 1
            f a a, g a a

        let addEv = Event<unit> ()
        let mulEv = Event<unit> ()

        let addCacheable = Cacheable.func add addEv.Publish CachingStrategy.All
        let mulCacheable = Cacheable.func mul mulEv.Publish CachingStrategy.All

        let combinerCached =
            Cacheable.lift combiner CachingStrategy.All
            |> Cacheable.applyArg addCacheable
            |> Cacheable.applyArg mulCacheable
            |> Cacheable.convert

        let firstResult = combinerCached 3

        Assert.That(ctr1, Is.EqualTo 1)
        Assert.That(ctr2, Is.EqualTo 1)
        Assert.That(ctr3, Is.EqualTo 1)

        addEv.Trigger ()

        let secondResult = combinerCached 3

        Assert.That(firstResult, Is.EqualTo secondResult)
        Assert.That(ctr1, Is.EqualTo 2)
        Assert.That(ctr2, Is.EqualTo 1)
        Assert.That(ctr3, Is.EqualTo 2)

        mulEv.Trigger ()

        let thirdResult = combinerCached 3

        Assert.That(thirdResult, Is.EqualTo secondResult)
        Assert.That(ctr1, Is.EqualTo 2)
        Assert.That(ctr2, Is.EqualTo 2)
        Assert.That(ctr3, Is.EqualTo 3)

        
