module Day13

open Serilog
open System.Collections.Generic

// Rosetta code
let modular_inverse n g =
  let rec egcd n i g e l a =
    match e with
    | 0L -> g
    | _ -> let o = n / e
           egcd e l a (n - o * e) (i - o * l) (g - o * a) 
  (n + (egcd n 1L 0L g 0L 1L)) % n

let run (lines: IEnumerable<string>) =
    Log.Information $"Executing Day 13"

    let earliest_time = int64 (Seq.head lines)
    let all_buses = ((Seq.skip 1 lines) |> Seq.head).Split(',')
    let possible_buses = all_buses |> Seq.filter (fun x -> not (x = "x")) |> Seq.map int64

    let (task1_bus, rem, num_waits) =
        possible_buses
        |> Seq.map (fun bus_id -> (bus_id, earliest_time % bus_id, (earliest_time / bus_id) + 1L))
        |> Seq.sortBy (fun (bus_id, remainder, _) -> bus_id - remainder)
        |> Seq.head

    Log.Information $"Task 1, closest bus {task1_bus * (task1_bus - rem)} with waits {num_waits} and remainder {rem} and bus id {task1_bus}"

    let part_2_raw =
        all_buses
        |> Seq.zip (seq { 0 .. Seq.length all_buses})
        |> Seq.choose (fun (ix, c) -> match c with
                                        | "x" -> None
                                        | _ -> Some((int64 ix, int64 c)))

    // Just CRT implemented in F#. Not clever :(
    let product = part_2_raw |> Seq.map (fun (_, id) -> id) |> Seq.reduce (*)
    let answer =
        part_2_raw
        |> Seq.map (fun (ix, id) ->
                        let p = product / id
                        let m = modular_inverse id p
                        ((id - ix) * p * m))
        |> Seq.sum

    Log.Information $"{answer % product}"

    0