
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
    abstract Eval<'c> : ('c -> 'a) -> Cacheable<'b, 'c> -> 'r

and CacheableApply<'b, 'a> =
    abstract Apply<'r> : CacheableApplyEval<'b, 'a, 'r> -> 'r
and CacheableApplyEval<'b, 'a, 'r> =
    abstract Eval<'c> : Cacheable<'b, 'c -> 'a> -> Cacheable<'b, 'c> -> 'r

and CacheableContramap<'b, 'a> =
    abstract Apply<'r> : CacheableContramapEval<'b, 'a, 'r> -> 'r
and CacheableContramapEval<'b, 'a, 'r> =
    abstract Eval<'c> : ('b -> 'c) -> Cacheable<'c, 'a> -> 'r

and CacheableBind<'b, 'a> =
    abstract Apply<'r> : CacheableBindEval<'b, 'a, 'r> -> 'r
and CacheableBindEval<'b, 'a, 'r> =
    abstract Eval<'c> : ('c -> Cacheable<'b, 'a>) -> Cacheable<'b, 'c> -> 'r

/// Models the partial application of an argument to a cacheable with a value that is also
/// cacheable.
and CacheablePartialApplication<'b, 'a> =
    abstract Apply<'r> : CacheablePartialApplicationEval<'b, 'a, 'r> -> 'r
and CacheablePartialApplicationEval<'b, 'a, 'r> =
    abstract Eval<'c, 'd>
        : Cacheable<'c, 'd>
        -> Cacheable<'c -> 'd, 'b -> 'a>
        -> 'r

module Cacheable =

    let lift (f : 'a -> 'b) (cs : CachingStrategy) : Cacheable<'a, 'b> =
        Pure (f,cs)

    let func (f : 'a -> 'b) (reset : unit IEvent) (cs : CachingStrategy) : Cacheable<'a, 'b> =
        Func (f, reset, cs)

    let map (f : 'b -> 'c) (v : Cacheable<'a, 'b>) : Cacheable<'a, 'c> =
        { new CacheableMap<'a, 'c> with
            member __.Apply e = e.Eval<'b> f v
        } |> Map

    let apply (f : Cacheable<'a, 'c -> 'b>) (v : Cacheable<'a, 'c>) : Cacheable<'a, 'b> =
        { new CacheableApply<'a, 'b> with
            member __.Apply e = e.Eval f v
        } |> Apply

    let contramap (f : 'b -> 'a) (v : Cacheable<'a, 'c>) : Cacheable<'b, 'c> =
        { new CacheableContramap<'b, 'c> with
            member __.Apply e = e.Eval f v
        } |> Contramap

    let bind (f : 'c -> Cacheable<'a, 'b>) (v : Cacheable<'a, 'c>) : Cacheable<'a, 'b> =
        { new CacheableBind<'a, 'b> with
            member __.Apply e = e.Eval f v
        } |> Bind

    let applyArg (f : Cacheable<'a -> 'b, 'c -> 'd>) (v : Cacheable<'a, 'b>) : Cacheable<'c, 'd> =
        { new CacheablePartialApplication<'c, 'd> with
            member __.Apply e = e.Eval v f
        } |> PartialApplication



[<EntryPoint>]
let main argv =
    printfn "todo"
    0