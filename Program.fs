open System
open TypeEquality

/// The caching strategy is inherited by cacheables that merge others.
/// In these scenarioes LRU will override All.
[<Struct>]
type CachingStrategy =
    | All
    | LeastRecentlyUsed

type Cacheable<'b,'a> =
    | Pure of ('b -> 'a) * CachingStrategy
    | Func of ('b -> 'a) * IEvent<unit> * CachingStrategy
    | Map of CacheableMap<'b, 'a>
    | Apply of CacheableApply<'b, 'a>
    | Contramap of CacheableContramap<'b, 'a>
    | Bind of CacheableBind<'b, 'a>
    | PartialApplication of CacheablePartialApplication<'b, 'a>

and CacheableMap<'b, 'a> =
    abstract Apply<'r> : CacheableMapEval<'b, 'a, 'r> -> 'r
and CacheableMapEval<'b, 'a, 'r> =
    abstract Eval<'c> : ('a -> 'c) -> Cacheable<'b, 'a> -> 'r

and CacheableApply<'b, 'a> =
    abstract Apply<'r> : CacheableApplyEval<'b, 'a, 'r> -> 'r
and CacheableApplyEval<'b, 'a, 'r> =
    abstract Eval<'c> : Cacheable<'b, 'a -> 'b> -> Cacheable<'b, 'a> -> 'r

and CacheableContramap<'b, 'a> =
    abstract Apply<'r> : CacheableContramapEval<'b, 'a, 'r> -> 'r
and CacheableContramapEval<'b, 'a, 'r> =
    abstract Eval<'c> : ('c -> 'b) -> Cacheable<'b, 'a> -> 'r

and CacheableBind<'b, 'a> =
    abstract Apply<'r> : CacheableBindEval<'b, 'a, 'r> -> 'r
and CacheableBindEval<'b, 'a, 'r> = // (a -> m b) -> m a -> m b
    abstract Eval<'c> : ('c -> Cacheable<'b, 'a>) -> Cacheable<'b, 'c> -> 'r

/// Models the partial application of an argument to a cacheable with a value that is also
/// cacheable.
and CacheablePartialApplication<'b, 'a> =
    abstract Apply<'r> : CacheablePartialApplicationEval<'b, 'a, 'r> -> 'r
and CacheablePartialApplicationEval<'b, 'a, 'r> =
    abstract Eval<'c, 'd, 'e, 'f>
        : Teq<'b, 'c -> 'd>
        * Teq<'a, 'e -> 'f>
        * Cacheable<'c, 'd>
        * Cacheable<'c -> 'd, 'e -> 'f>
        -> 'r



[<EntryPoint>]
let main argv =
    let message = "F#" // Call the function
    printfn "Hello world %s" message
    0 // return an integer exit code