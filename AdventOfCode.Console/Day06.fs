module Day06

open Serilog
open System
open System.Collections.Generic

let run (lines: IEnumerable<string>) =
    Log.Information $"Executing Day 6"

    let groups =
        lines
        |> Seq.collect (fun line -> match String.IsNullOrWhiteSpace line with
                                    | true -> "\n"
                                    | false -> $" {line.Trim()}")
        |> Seq.toArray
        |> String
        |> (fun s -> s.Split('\n'))

    let stage_1_answer =
        groups
        |> Seq.map (fun line -> line.Replace(" ", String.Empty).Trim().ToCharArray())
        |> Seq.map Seq.distinct
        |> Seq.map Seq.length
        |> Seq.sum

    let count x = Seq.filter ((=) x) >> Seq.length

    let stage_2_answer =
        groups
        |> Seq.map (fun chars -> (chars.Replace(" ", String.Empty), count ' ' chars))
        |> Seq.map (fun (chars, num_people) -> chars |> Seq.distinct |> Seq.map (fun c -> count c chars) |> Seq.filter (fun c -> c = num_people) |> Seq.length)
        |> Seq.sum

    Log.Information $"Stage 1 answer: {stage_1_answer}"
    Log.Information $"Stage 2 answer: {stage_2_answer}"

    0