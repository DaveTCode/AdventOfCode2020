module Day15

open Serilog
open System.Collections.Generic

type NumberDetails = {
    last: int;
    last_but_one: int;
}

let run (lines: IEnumerable<string>) =
    Log.Information $"Executing Day 15"

    let starting_numbers =
        Dictionary<int, NumberDetails>(lines |> Seq.take (Seq.length lines - 1)|> Seq.mapi (fun ix v -> (int v, { last = ix + 1; last_but_one = 0 })) |> dict)

    let rec number_sequence last_spoken (d: Dictionary<int, NumberDetails>) index =
        seq {
            let answer = if d.ContainsKey(last_spoken) then
                             index - d.[last_spoken].last
                         else
                             0

            if d.ContainsKey(last_spoken) then
                d.[last_spoken] <- { d.[last_spoken] with last_but_one = d.[last_spoken].last; last = index }
            else
                d.Add(last_spoken, { last_but_one = 0; last = index })

            yield answer
            yield! (number_sequence answer d (index + 1))
        }



    let task1_seq = number_sequence (lines |> Seq.last |> int) starting_numbers (Seq.length lines)
    let task1_answer = Seq.item (30000000 - Seq.length lines - 1) task1_seq

    Log.Information $"Task 1 2020 -> {task1_answer}"

    0