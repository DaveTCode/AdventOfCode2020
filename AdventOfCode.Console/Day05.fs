module Day05

open Serilog
open System
open System.Collections.Generic

let run (lines: IEnumerable<string>) =
    Log.Information $"Executing Day 5"

    let seats =
        lines
        |> Seq.map (fun line -> (line.Substring(0, 7).Replace('B', '1').Replace('F', '0'), line.Substring(7).Replace('R', '1').Replace('L', '0')))
        |> Seq.map (fun (row, col) -> (Convert.ToInt32(row, 2), Convert.ToInt32(col, 2)))
        |> Seq.map (fun (row, col) -> (row, col, row * 8 + col))
        |> Seq.sortByDescending (fun (_, _, seat_num) -> seat_num)
        |> Seq.toList

    let ((_, _, num), (_, _, _)) =
        seats
        |> Seq.zip (seats.[1..seats.Length-1])
        |> Seq.find (fun ((_, _, num), (_, _, num2)) -> not (num2 = num + 1))

    Log.Information $"Task 1: {seats.[0]}"
    Log.Information $"Task 2: My seat is {num + 1}"

    0