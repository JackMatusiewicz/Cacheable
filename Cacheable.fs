namespace Cacheable

module Cacheable =

    let lift (f : 'a -> 'b) : Cacheable<'a, 'b> =
        Pure (f, CachingStrategy.NoCaching)

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

    // TODO - Need to account for the caching strategies.
    // Notably, we don't memoise if there is a caching strategy of none (most likely in the Pure case)
    let rec convert<'a, 'b> (v : Cacheable<'a, 'b>) : (('a -> 'b) * unit IEvent) =
        match v with
        | Pure (f, _) ->
            let ev = Event<unit>().Publish
            Function.memoise ev f, ev

        | Func (f, ev, _) ->
            Function.memoise ev f, ev

        | Map mc ->
            mc.Apply { new CacheableMapEval<'a, 'b, ('a -> 'b) * unit IEvent> with
                member __.Eval<'c> (f : 'c -> 'b) v =
                    let v, innerEv = convert<'a, 'c> v
                    let f = Function.memoise innerEv f
                    (fun a -> f (v a)), innerEv
            }

        | Contramap cc ->
            cc.Apply { new CacheableContramapEval<'a, 'b, ('a -> 'b) * unit IEvent> with
                member __.Eval<'c> f v =
                    let v, innerEv = convert<'c, 'b> v
                    let f = Function.memoise innerEv f
                    (fun a -> v (f a)) , innerEv
            }

        | Apply ac ->
            ac.Apply { new CacheableApplyEval<'a, 'b, ('a -> 'b) * unit IEvent> with
                member __.Eval<'c> f v =
                    let (f', fEv) = convert<'a, 'c -> 'b> f
                    let (v', vEv) = convert<'a, 'c> v
                    let mergedEvent = Event.merge fEv vEv
                    Function.memoise mergedEvent (fun a -> f' a (v' a)), mergedEvent
            }

        | PartialApplication pc ->
            pc.Apply { new CacheablePartialApplicationEval<'a, 'b, ('a -> 'b) * unit IEvent> with
                member __.Eval<'c, 'd> f v =
                    let (f, fEv) = convert<'c, 'd> f
                    let (v, vEv) = convert<'c -> 'd, 'a -> 'b> v
                    let merged = Event.merge fEv vEv
                    Function.memoise merged (fun a -> v f a), merged
            }

        | Bind _ -> failwith "Bind is currently not supported."