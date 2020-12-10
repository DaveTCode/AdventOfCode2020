module Day10

open Serilog
open System.Collections.Generic

let run (lines: IEnumerable<string>) =
    Log.Information $"Executing Day 10"

    let adapters = lines |> Seq.map int
    let sorted_adapters = adapters |> Seq.sort |> Seq.toArray
    
    let task1_pairs =
        sorted_adapters
        |> Seq.zip (seq { 0 .. Seq.length sorted_adapters})
        |> Seq.map (fun (ix, j) -> match ix with
                                    | 0 -> j
                                    | n -> j - sorted_adapters.[n-1])
        |> Seq.append (Seq.singleton 3)

    let one_jolt_pairs = task1_pairs |> Seq.filter (fun x -> x = 1) |> Seq.length
    let three_jolt_pairs = task1_pairs |> Seq.filter (fun x -> x = 3) |> Seq.length


    let rec slow_valid_paths start_val (possibles: int[]) =
        match Seq.length possibles with
            | 0 -> int64 1
            | _ -> possibles
                    |> Seq.zip (seq {0 .. Seq.length possibles})
                    |> Seq.takeWhile (fun (_, n) -> n <= (start_val + 3))
                    |> Seq.map (fun (ix, n) -> (slow_valid_paths n (possibles.[ix + 1..])))
                    |> Seq.sum
                    |> int64

    let rec fast_valid_paths possibilities prev_path_lens: int64 =
        let current = Seq.head possibilities
        let routes_to_me =
            prev_path_lens
            |> Seq.filter (fun (p, _) -> current - p <= 3)
            |> Seq.map (fun (_, r) -> r)
            |> Seq.sum

        match Seq.length possibilities with
            | 1 -> routes_to_me
            | _ -> (fast_valid_paths (Seq.tail possibilities) (Seq.append (Seq.tail prev_path_lens) (Seq.singleton (current, routes_to_me))))

    let total_paths = fast_valid_paths sorted_adapters (seq { (0, int64 0); (0, int64 0); (0, int64 1) } |> Seq.toArray)


    Log.Information $"Task 1 result is {one_jolt_pairs} * {three_jolt_pairs} = {one_jolt_pairs * three_jolt_pairs}"
    Log.Information $"Task 2 result is {total_paths}"

    0