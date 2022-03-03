﻿namespace Cacheable

open System.Reflection
open Microsoft.FSharp.Reflection
open System.Collections.Generic
open FSharp.Quotations
open System
open FSharp.Quotations.Patterns

module Function =

    let private invokeStaticMethod (lambda : Expr) (types : Type list) (args : obj list) =
        let rec invoke lambda =
            match lambda with
            | Lambda(_, innerExpr) ->
                    invoke innerExpr
            | Call(None, mi, _) ->
                let mig = mi.GetGenericMethodDefinition ()
                let genM = mig.MakeGenericMethod(types |> Array.ofList)
                genM.Invoke(null, args |> List.toArray)
            | _ ->
                sprintf "Unable to call a function of type: %s" (lambda.GetType().Name)
                |> failwith
        invoke lambda

    let private containsInterface (t : System.Type) (o : obj) =
        t.ToString().Contains(o.ToString())

    /// Takes a thing of type 'a and tries to make a cachable version.
    /// If 'a is not a function type, then it just returns the value.
    /// If 'a is actually a 'b -> 'c, then it will create a cachable version.
    let rec memoise<'a> (clearSignal : IEvent<unit>) (f : 'a) : 'a =
        match FSharpType.IsFunction (typeof<'a>) with
        | false -> f
        | true ->
            let (dom, range) = FSharpType.GetFunctionElements typeof<'a>
            let implementsEquality =
                let equalityInterface = "System.IEquality"
                let filter = TypeFilter containsInterface
                dom.FindInterfaces (filter, equalityInterface)
                |> fun l -> l.Length > 0
            if FSharpType.IsFunction dom then
                invokeStaticMethod
                    <@ makeCacheFuncWithNonCachedArg @>
                    [dom; range]
                    [clearSignal; f]
                    |> unbox<'a>
            elif implementsEquality then
                invokeStaticMethod <@ makeCacheFunc @> [dom; range] [clearSignal; f] |> unbox<'a>
            else
                invokeStaticMethod <@ makeObjCacheFunc @> [dom; range] [clearSignal; f] |> unbox<'a>

    /// If the domain type is a function, then there is no meaningful caching we can do.
    and private makeCacheFuncWithNonCachedArg<'a, 'b> (clearSignal : IEvent<unit>) (f : 'a -> 'b) : 'a -> 'b =
        let v = fun a -> f a |> memoise clearSignal
        v

    and private makeCacheFunc<'a, 'b when 'a : equality> (clearSignal : IEvent<unit>) (f : 'a -> 'b) : 'a -> 'b =
        let d = Dictionary<'a, 'b> ()
        clearSignal.Add (fun _ -> d.Clear())

        fun a ->
            if d.ContainsKey a then
                d.[a]
            else
                let v = f a |> memoise clearSignal
                d.[a] <- v
                v

    and private makeObjCacheFunc<'a, 'b> (clearSignal : IEvent<unit>) (f : 'a -> 'b) : 'a -> 'b =
        let d = Dictionary<obj, 'b> ()
        clearSignal.Add (fun _ -> d.Clear())

        fun a ->
            if d.ContainsKey a then
                d.[a]
            else
                let v = f a |> memoise clearSignal
                d.[a] <- v
                v