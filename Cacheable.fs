namespace Cacheable

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