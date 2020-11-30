module Day1

open Serilog
open System.Linq
open System.Collections.Generic

let massToFuel mass =
    mass / 3 - 2

let run (lines: IEnumerable<string>) =
    Log.Information $"Executing Day 1"

    let result =
        lines.Select int
            |> Seq.map massToFuel
            |> Seq.sum

    Log.Information $"Results is {result}"

    0