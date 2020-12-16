module Day16

open Serilog
open System.Collections.Generic
open System.Text.RegularExpressions

type Field = string * IEnumerable<int> * IEnumerable<int>

type Ticket = int[]

let line_to_field s =
    let m = Regex.Match(s, @"(?<fname>[\w\s]+): (?<r1l>\d+)-(?<r1h>\d+) or (?<r2l>\d+)-(?<r2h>\d+)")
    (
        m.Groups.["fname"].Value,
        seq { (int m.Groups.["r1l"].Value) .. (int m.Groups.["r1h"].Value)},
        seq { (int m.Groups.["r2l"].Value) .. (int m.Groups.["r2h"].Value)}
    )

let line_to_ticket (line: string) =
    line.Split(',') |> Seq.map int |> Seq.toArray

let is_value_valid_for_field v (_, fr1, fr2) =
    (Seq.contains v fr1) || (Seq.contains v fr2)

let run (lines: IEnumerable<string>) =
    Log.Information $"Executing Day 16"

    let fields =
        lines |> Seq.filter (fun s -> s.Contains("or")) |> Seq.map line_to_field

    let my_ticket = lines |> Seq.skipWhile (fun l -> not (l.StartsWith("your ticket:"))) |> Seq.skip 1 |> Seq.head |> line_to_ticket

    let all_tickets = lines |> Seq.filter (fun l -> l.Contains(",")) |> Seq.skip 1 |> Seq.map line_to_ticket

    let nearby_tickets_invalid_values =
        all_tickets
        |> Seq.map (fun ticket ->
                        ticket
                        |> Array.filter (fun v -> fields |> Seq.forall (fun f -> not (is_value_valid_for_field v f)))
                        |> Array.sum)
        |> Seq.sum

    Log.Information $"Task 1: Sum of all nearby invalid ticket fields {nearby_tickets_invalid_values}"

    let valid_tickets =
        all_tickets
        |> Seq.filter (fun ticket -> ticket |> Array.forall (fun v -> fields |> Seq.exists (fun f -> is_value_valid_for_field v f)))

    let field_order =
        fields
        |> Seq.map (fun f ->
                        (
                            f,
                            seq { 0 .. my_ticket.Length - 1 }
                            |> Seq.filter (fun ix -> valid_tickets |> Seq.forall (fun ticket -> is_value_valid_for_field ticket.[ix] f))
                            |> Seq.toList
                        )
                    )
        |> Seq.toList

    let rec reduce_to_final_values (found: (Field * int) list) (potentials: (Field * int list) list) =
        if Seq.length potentials = 0 then
            found
        else
            let new_found =
                found @
                (potentials
                 |> Seq.filter (fun (_, l) -> (Seq.length l) = 1)
                 |> Seq.map (fun (f, l) -> (f, Seq.head l))
                 |> Seq.toList)

            let fixed_ix = new_found |> Seq.map (fun (_, a) -> a)

            let new_potentials =
                potentials
                |> Seq.filter (fun (_, l) -> (Seq.length l) > 1)
                |> Seq.map (fun (f, l) -> (f, Seq.except fixed_ix l |> Seq.toList))
                |> Seq.toList

            reduce_to_final_values new_found new_potentials

    for ((fname, _, _), ix) in field_order do
        printfn "%s %A" fname (ix |> Seq.toArray)

    let actual_fields =
        reduce_to_final_values List.empty field_order

    for ((fname, _, _), ix) in actual_fields do
        printfn "%s %d" fname ix

    let task2 =
        actual_fields
        |> Seq.filter (fun ((fname, _, _), _) -> fname.StartsWith("departure"))
        |> Seq.map (fun (_, ix) -> int64 my_ticket.[ix])
        |> Seq.reduce (*)

    Log.Information $"Task 2 multiply = {task2}"

    0