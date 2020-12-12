module Day11

open Common
open Serilog
open System.Collections.Generic

type Cell = Floor | Chair | Occupied

let run (lines: IEnumerable<string>) =
    Log.Information $"Executing Day 11"

    let lobby =
        lines
        |> Seq.map (fun line ->
                        line
                        |> Seq.map (fun c -> match c with
                                              | '.' -> Floor
                                              | 'L' -> Chair
                                              | _ -> raise (ParseException $"Invalid character {c}"))
                        |> Seq.toArray)
        |> Seq.toArray

    let immediate_adjacencies (lobby: Cell[][]) x y =
        seq { (x-1,y-1); (x, y-1); (x+1,y-1); (x-1,y); (x+1,y); (x-1,y+1); (x, y+1); (x+1,y+1)}
        |> Seq.filter (fun (x,y) -> x >= 0 && x < lobby.Length && y >= 0 && y < lobby.[0].Length)
        |> Seq.map (fun (x,y) -> lobby.[x].[y])
        |> Seq.filter (fun c -> c = Occupied)
        |> Seq.length

    let rec slope (lobby: Cell[][]) x y x_inc y_inc =
        seq {
            if x < 0 || x >= lobby.Length || y < 0 || y >= lobby.[0].Length then
                ()
            else
                yield lobby.[x].[y]
                yield! (slope lobby (x + x_inc) (y + y_inc) x_inc y_inc)
        }

    let raytraced_adjacencies (lobby: Cell[][]) x y =
        seq { (-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1)}
        |> Seq.map (fun (x_inc, y_inc) -> (slope lobby (x + x_inc) (y + y_inc) x_inc y_inc))
        |> Seq.map (fun s ->
                        let firstChair = (s |> Seq.tryFind (fun c -> c = Occupied || c = Chair))
                        match firstChair with
                            | Some(Occupied) -> true
                            | _ -> false)
        |> Seq.filter (fun b -> b)
        |> Seq.length

    let step_lobby lobby adjacencies max_adjs =
        lobby
            |> Array.indexed
            |> Array.map (fun (x, line) ->
                            line
                            |> Array.indexed
                            |> Array.map (fun (y, c) ->
                                            match (c, (adjacencies lobby x y)) with
                                                | (Chair, 0) -> Occupied
                                                | (Chair, _) -> Chair
                                                | (Occupied, x) when x >= max_adjs -> Chair
                                                | (Occupied, _) -> Occupied
                                                | (Floor, _) -> Floor))

    let compare_lobbies lobby1 lobby2 =
        Array.zip lobby1 lobby2
            |> Array.map (fun (l1, l2) -> Array.zip l1 l2 |> Array.map (fun (a, b) -> a = b) |> Array.reduce (&&))
            |> Array.reduce (&&)

    let rec iterate_until_stable lobby iterations adjacencies max_adjs =
        Log.Information $"----- {iterations} -----"
        let next_lobby = step_lobby lobby adjacencies max_adjs 
        if compare_lobbies lobby next_lobby then
            (lobby, iterations)
        else 
            iterate_until_stable next_lobby (iterations + 1) adjacencies max_adjs

    let count_occupied lobby =
        lobby
            |> Array.map (fun row -> row |> Array.filter (fun c -> c = Occupied) |> Array.length)
            |> Array.sum

    let (task1_lobby, _task1_iterations) = iterate_until_stable lobby 0 immediate_adjacencies 4
    let (task2_lobby, _task2_iterations) = iterate_until_stable lobby 0 raytraced_adjacencies 5

    Log.Information $"Task 1 {count_occupied task1_lobby}"
    Log.Information $"Task 2 {count_occupied task2_lobby}"

    0