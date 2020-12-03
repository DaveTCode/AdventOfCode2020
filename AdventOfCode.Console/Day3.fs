module Day3

open Serilog
open System.Collections.Generic

exception ParseException of string;

type Cell = Tree | Blank

type Mountain = Cell[][]

type Slope = Cell seq

let run (lines: IEnumerable<string>) =
    Log.Information $"Executing Day 3"

    let rec generate_slope (mountain: Mountain, x: int, y: int, x_inc: int, y_inc: int) =
        seq {
            match Seq.length mountain - y with
                | x when x <= 0 -> ()
                | _ ->
                    yield mountain.[y].[x]
                    yield! (generate_slope (mountain, (x + x_inc) % mountain.[y].Length, y + y_inc, x_inc, y_inc))
        }

    let char_to_cell c =
        match c with
            | '.' -> Blank
            | '#' -> Tree
            | _ -> raise (ParseException $"Invalid char {c} in input")

    let process_line line =
        Seq.toList line
            |> Seq.map char_to_cell
            |> Seq.toArray

    let count_trees slope = slope |> Seq.filter (fun cell -> cell = Tree) |> Seq.length |> int64

    let mountain = lines |> Seq.map process_line |> Seq.toArray

    let task1 = generate_slope (mountain, 0, 0, 3, 1)
    let results = [
        count_trees (generate_slope (mountain, 0, 0, 1, 1))
        count_trees (generate_slope (mountain, 0, 0, 3, 1))
        count_trees (generate_slope (mountain, 0, 0, 5, 1))
        count_trees (generate_slope (mountain, 0, 0, 7, 1))
        count_trees (generate_slope (mountain, 0, 0, 1, 2))
    ]

    Log.Information $"Task 1 - {count_trees task1} trees"
    Log.Information $"Task 2 - {results |> Seq.reduce (*)} trees"

    0