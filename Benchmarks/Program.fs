open System.Collections.Generic
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open System.Runtime.CompilerServices
open Cacheable

[<MemoryDiagnoser>]
[<SimpleJob(3,3,3)>]
type LowerOrderFunctionBenchmark () =

    let simpleCache = Dictionary<struct(int * int * int * int * int), int> ()
    let simpleCachedFunction a b c d e : int =
        let k = struct(a,b,c,d,e)
        match simpleCache.TryGetValue k with
        | (true, v) -> v
        | _ ->
            let f = a + b + c + d + e
            simpleCache.[k] <- f
            f

    let addFive a b c d e =
        a + b + c + d + e

    let addFiveCached =
        Cacheable.lift addFive CachingStrategy.All
        |> Cacheable.convert

    [<Benchmark>]
    [<MethodImpl(MethodImplOptions.NoOptimization ||| MethodImplOptions.NoInlining)>]
    member _.CustomCaching () =
        for i in 1 .. 1000 do
            simpleCachedFunction 1 2 3 4 5 |> ignore

    [<Benchmark>]
    [<MethodImpl(MethodImplOptions.NoOptimization ||| MethodImplOptions.NoInlining)>]
    member _.CacheableCaching () =
        for i in 1 .. 1000 do
            addFiveCached 1 2 3 4 5 |> ignore

    [<Benchmark>]
    [<MethodImpl(MethodImplOptions.NoOptimization ||| MethodImplOptions.NoInlining)>]
    member _.CustomCachingAllUnique () =
        for i in 1 .. 1000 do
            simpleCachedFunction i i i i i |> ignore

    [<Benchmark>]
    [<MethodImpl(MethodImplOptions.NoOptimization ||| MethodImplOptions.NoInlining)>]
    member _.CacheableCachingAllUnique () =
        for i in 1 .. 1000 do
            addFiveCached i i i i i |> ignore

[<EntryPoint>]
let main argv =
    BenchmarkRunner.Run<LowerOrderFunctionBenchmark>() |> ignore
    0
