module Day01

open Serilog
open System.Collections.Generic

let run (lines: IEnumerable<string>) =
    Log.Information $"Executing Day 1"

    let ints = lines |> Seq.map int
    let (r1, r2) =
        ints
        |> Seq.collect (fun i1 -> ints |> Seq.map (fun i2 -> (i1, i2)))
        |> Seq.find (fun (p1, p2) -> p1 + p2 = 2020)

    let (s1, s2, s3) =
        ints
        |> Seq.collect (fun i1 -> ints |> Seq.map (fun i2 -> (i1, i2)))
        |> Seq.collect (fun (p1, p2) -> ints |> Seq.map (fun i -> (p1, p2, i)))
        |> Seq.find (fun (p1, p2, p3) -> p1 + p2 + p3 = 2020)

    Log.Information $"Task 1 result is {r1} * {r2} = {r1 * r2}"
    Log.Information $"Task 2 result is {s1} * {s2} * {s3} = {s1 * s2 * s3}"

    0