namespace Cacheable

module Example =

    let addNumbers (a : int) (b : int) : int = a + b

    let applyTwice (f : int -> int -> int) a = f a a

    let addNumbersCacheable =
        let ev = Event<unit> ()
        ev.Trigger, Cacheable.func addNumbers ev.Publish CachingStrategy.All

    let applyTwiceCacheable =
        let ev = Event<unit> ()
        ev.Trigger, Cacheable.func applyTwice ev.Publish CachingStrategy.All

    let mergedCacheable =
        let (_, add) = addNumbersCacheable
        let (_, big) = applyTwiceCacheable
        let merged = Cacheable.applyArg big add
        merged

