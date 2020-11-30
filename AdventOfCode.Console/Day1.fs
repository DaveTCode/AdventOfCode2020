module Day1

open Serilog
open System.Linq
open System.Collections.Generic

let run (lines: IEnumerable<string>) =
    Log.Information $"Executing Day 1"

    let result =
        lines.Select int
            |> Seq.map (fun m -> m / 3 - 2)
            |> Seq.sum

    Log.Information $"Results is {result}"

    0