namespace Cacheable

open System.Reflection
open Microsoft.FSharp.Reflection
open System.Collections.Generic
open FSharp.Quotations
open System
open FSharp.Quotations.Patterns

// This entire module is incredibly thread unsafe.
module Function =

    let private invokeStaticMethod
        (lambda : Expr)
        (types : Type array)
        (args : obj array)
        : obj
        =
        let rec invoke lambda =
            match lambda with
            | Lambda(_, innerExpr) ->
                invoke innerExpr
            | Call(None, mi, _) ->
                let mig = mi.GetGenericMethodDefinition ()
                let genM = mig.MakeGenericMethod types
                genM.Invoke(null, args)
            | _ ->
                $"Unable to call a function of type: %s{lambda.GetType().Name}"
                |> failwith
        invoke lambda        

    let private typeImplementsEqualityCache = Dictionary<Type, bool> ()
    let private equalityInterface = nameof IEquatable
    let private typeImplementsEquality (t : Type) =
        match typeImplementsEqualityCache.TryGetValue t with
        | true, v -> v
        | _ ->
            let v =
                let mutable equatable = false
                t.GetInterfaces ()
                |> Array.iter(fun i ->
                    if i.ToString().Contains(equalityInterface) then
                        equatable <- true)
                equatable
            typeImplementsEqualityCache.[t] <- v
            v
            
    /// Takes a thing of type 'a and tries to make a cachable version.
    /// If 'a is not a function type, then it just returns the value.
    /// If 'a is actually a 'b -> 'c, then it will create a cachable version.
    let rec memoise<'a> (clearSignal : IEvent<unit>) (f : 'a) : 'a =    
        match FSharpType.IsFunction typeof<'a> with
        | false -> f
        | true ->
            let dom, range = FSharpType.GetFunctionElements typeof<'a>
            let implementsEquality = typeImplementsEquality dom
            let genericTypes = [|dom ; range|]
            let args = [|box clearSignal ; box f|]

            if FSharpType.IsFunction dom then
                invokeStaticMethod
                    <@ makeCacheFuncWithNonCachedArg @>
                    genericTypes
                    args
                    |> unbox<'a>
            elif implementsEquality then
                invokeStaticMethod
                    <@ makeCacheFunc @>
                    genericTypes
                    args
                |> unbox<'a>
            else
                invokeStaticMethod
                    <@ makeObjCacheFunc @>
                    genericTypes
                    args
                |> unbox<'a>

    /// If the domain type is a function, then there is no meaningful caching we can do.
    and private makeCacheFuncWithNonCachedArg<'a, 'b>
        (clearSignal : IEvent<unit>)
        (f : 'a -> 'b)
        : 'a -> 'b
        =
        let v = fun a -> f a |> memoise clearSignal
        v

    and private makeCacheFunc<'a, 'b when 'a : equality>
        (clearSignal : IEvent<unit>)
        (f : 'a -> 'b)
        : 'a -> 'b
        =
        let d = Dictionary<'a, 'b> ()
        clearSignal.Add (fun _ -> d.Clear())

        fun a ->
            match d.TryGetValue a with
            | (true, v) -> v
            | _ ->
                let v = f a |> memoise clearSignal
                d.[a] <- v
                v

    /// The fallback case where we have to box the key.
    and private makeObjCacheFunc<'a, 'b>
        (clearSignal : IEvent<unit>)
        (f : 'a -> 'b)
        : 'a -> 'b
        =
        let d = Dictionary<obj, 'b> ()
        clearSignal.Add (fun _ -> d.Clear())

        fun a ->
            match d.TryGetValue a with
            | true, v -> v
            | _ ->
                let v = f a |> memoise clearSignal
                d.[a] <- v
                v