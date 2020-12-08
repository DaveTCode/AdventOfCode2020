module Day7

open Serilog
open System
open System.Collections.Generic
open System.Text.RegularExpressions

type Color = String

type Bag = {
    color: Color;
    bags: IDictionary<Color, int>;
    mutable nodes: IEnumerable<Tuple<Bag, int>>;
}

let run (lines: IEnumerable<string>) =
    Log.Information $"Executing Day 7"

    let parse_bag_line line =
        let parts = Regex.Match(line, @"^([\w\s]+)bags contain (.*)$")
        let bag_color = parts.Groups.[1].Value.Trim()
        let contains =
            match parts.Groups.[2].Value.Trim() with
            | "no other bags." -> [] |> dict
            | other -> other.Split(',', StringSplitOptions.RemoveEmptyEntries)
                        |> Seq.map (fun c ->
                                    let components = Regex.Match(c, @"(\d+) ([\w\s]+) bags?").Groups
                                    (components.[2].Value.Trim()), int (components.[1].Value.Trim()))
                        |> dict
        { color = bag_color; bags = contains; nodes = Seq.empty }

    let rec contains_color color bag =
        match bag.color with
            | c when c = color -> true
            | _ -> bag.nodes |> Seq.map (fun (b, _) -> contains_color color b) |> Seq.fold (||) false

    let rec count_contents bag =
        bag.nodes
        |> Seq.map (fun (b, count) -> count + count * (count_contents b))
        |> Seq.sum

    let bags = lines
               |> Seq.map parse_bag_line
               |> Seq.toList

    for bag in bags do
        bag.nodes <- (bag.bags |> Seq.map (fun b1 -> (bags |> Seq.find (fun b2 -> b2.color = b1.Key), b1.Value)))

    let shiny_gold_containers =
        bags
        |> Seq.filter (fun bag -> contains_color "shiny gold" bag)
        |> Seq.length

    let shiny_gold_bag = bags |> Seq.find (fun bag -> bag.color = "shiny gold")
    let shiny_gold_contents = count_contents shiny_gold_bag
        

    Log.Information $"Shiny gold containers: {shiny_gold_containers - 1}"
    Log.Information $"Shiny gold contents: {shiny_gold_contents}"

    0