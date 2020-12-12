module Day09

open Serilog
open System.Collections.Generic

let run (lines: IEnumerable<string>) =
    Log.Information $"Executing Day 9"

    let vals = lines |> Seq.map int64 |> Seq.toList

    let results =
        vals
        |> Seq.skip 25
        |> Seq.zip (seq { 25..vals.Length })
        |> Seq.map (fun (ix, v) -> match ix with
                                     | n when n < 25 -> (v, Seq.empty |> Seq.toList)
                                     | n -> (v, vals.[(n - 25) .. (n - 1)]))
        |> Seq.map (fun (v, potentials) -> (v, potentials |> Seq.allPairs potentials |> Seq.map (fun (a, b) -> a + b) |> Seq.contains v))

    let (stage_1_result, _) = results |> Seq.find (fun (v, x) -> not x)

    let subarray_sum_exists (arr: int64 list, sum: int64) =
        seq { 0..arr.Length }
        |> Seq.map (fun ix -> (ix, arr.[..ix] |> Seq.sum))
        |> Seq.tryFind (fun (ix, calc) -> calc = sum)

    let stage_2_list =
        seq { 0..vals.Length }
        |> Seq.map (fun ix -> vals.[ix..])
        |> Seq.choose (fun l -> match (subarray_sum_exists(l, stage_1_result)) with
                                | None -> None
                                | Some((ix2, _)) -> Some(l.[..ix2]))
        |> Seq.head

    Log.Information $"Stage 1: First element without matching previous sum {stage_1_result}"
    Log.Information $"Stage 2: List is {stage_2_list} with smallest {Seq.min stage_2_list} and last {Seq.max stage_2_list} giving result {Seq.min stage_2_list + (Seq.max stage_2_list)}"

    0