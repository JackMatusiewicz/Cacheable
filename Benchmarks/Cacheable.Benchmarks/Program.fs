﻿open System.Collections.Generic
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open System.Runtime.CompilerServices
open Cacheable

let mutable simpleCounter = 0
let mutable cacheableCounter = 0

[<MemoryDiagnoser>]
[<SimpleJob(3,3,3)>]
type LowerOrderFunctionBenchmark () =

    let simpleCache = Dictionary<int * int * int * int * int, int> ()
    let simpleCachedFunction a b c d e : int =
        let k = (a,b,c,d,e)
        match simpleCache.TryGetValue k with
        | (true, v) -> v
        | _ ->
            simpleCounter <- simpleCounter + 1
            let f = a + b + c + d + e
            simpleCache.[k] <- f
            f

    let addFive a b c d e =
        cacheableCounter <- cacheableCounter + 1
        a + b + c + d + e

    let addFiveCached =
        Cacheable.lift addFive CachingStrategy.All
        |> Cacheable.convert

    [<Benchmark>]
    [<MethodImpl(MethodImplOptions.NoOptimization ||| MethodImplOptions.NoInlining)>]
    member x.CustomCaching () =
        simpleCachedFunction 1 2 3 4 5

    [<Benchmark>]
    [<MethodImpl(MethodImplOptions.NoOptimization ||| MethodImplOptions.NoInlining)>]
    member x.CacheableCaching () =
        addFiveCached 1 2 3 4 5

(*
Benchmark results:

|           Method |     Mean |   Error |  StdDev |  Gen 0 | Allocated |
|----------------- |---------:|--------:|--------:|-------:|----------:|
|    CustomCaching | 276.8 ns | 3.91 ns | 2.33 ns | 0.1273 |     400 B |
| CacheableCaching | 279.4 ns | 2.36 ns | 1.41 ns | 0.0381 |     120 B |
*)

[<EntryPoint>]
let main argv =
    BenchmarkRunner.Run<LowerOrderFunctionBenchmark>() |> ignore
    printfn "%d - %d" simpleCounter cacheableCounter
    0