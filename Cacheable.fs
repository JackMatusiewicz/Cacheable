namespace Cacheable

module Cacheable =

    let lift (f : 'a -> 'b) (cs : CachingStrategy) : Cacheable<'a, 'b> =
        Pure (f, cs)

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

    let applyArg (v : Cacheable<'a, 'b>) (f : Cacheable<'a -> 'b, 'c -> 'd>) : Cacheable<'c, 'd> =
        { new CacheablePartialApplication<'c, 'd> with
            member __.Apply e = e.Eval v f
        } |> PartialApplication

    // TODO - Need to account for the caching strategies.
    // Notably, we don't memoise if there is a caching strategy of none (most likely in the Pure case)
    let rec convert'<'a, 'b, 'k>
        (v : Cacheable<'a, 'b>)
        (kont : (('a -> 'b) * unit IEvent) -> 'k)
        : 'k
        =
        match v with
        | Pure (f, _) ->
            let ev = Event<unit>().Publish
            (Function.memoise ev f, ev)
            |> kont

        | Func (f, ev, _) ->
            (Function.memoise ev f, ev)
            |> kont

        | Map mc ->
            mc.Apply { new CacheableMapEval<'a, 'b, 'k> with
                member __.Eval<'c> (f : 'c -> 'b) v =
                    convert'<'a, 'c, 'k> v
                        (fun (v, innerEv) ->
                            let f = Function.memoise innerEv f
                            ((fun a -> f (v a)), innerEv)
                            |> kont
                        )
            }

        | Contramap cc ->
            cc.Apply { new CacheableContramapEval<'a, 'b, 'k> with
                member __.Eval<'c> f v =
                    convert'<'c, 'b, 'k> v
                        (fun (v, innerEv) ->
                            let f = Function.memoise innerEv f
                            ((fun a -> v (f a)) , innerEv)
                            |> kont
                        )
            }

        | Apply ac ->
            ac.Apply { new CacheableApplyEval<'a, 'b, 'k> with
                member __.Eval<'c> f v =
                    convert'<'a, 'c -> 'b, 'k> f
                        (fun (f', fEv) ->
                            convert'<'a, 'c, 'k> v
                                (fun (v', vEv) ->
                                    let mergedEvent = Event.merge fEv vEv
                                    (Function.memoise mergedEvent (fun a -> f' a (v' a)), mergedEvent)
                                    |> kont
                                )
                        )
            }

        | PartialApplication pc ->
            pc.Apply { new CacheablePartialApplicationEval<'a, 'b, 'k> with
                member __.Eval<'c, 'd> f v =
                    convert'<'c, 'd, 'k> f
                        (fun (f, fEv) ->
                            convert'<'c -> 'd, 'a -> 'b, 'k> v
                                (fun (v, vEv) ->
                                    let merged = Event.merge fEv vEv
                                    (Function.memoise merged (fun a -> v f a), merged)
                                    |> kont
                                )
                        )
            }

    let convert v = convert' v id |> fst