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
