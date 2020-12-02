module Day2

open Serilog
open System.Collections.Generic
open System.Linq
open System.Text.RegularExpressions

exception ParseException of string;

type PasswordRule = {
    min: int;
    max: int;
    letter: char;
}

type LineData = {
    rule: PasswordRule;
    password: string;
}

let convert_line (line: string) =
    let (|ParseRegex|_|) regex str =
        let m = Regex(regex).Match(str)
        if m.Success
        then Some (List.tail [ for x in m.Groups -> x.Value ])
        else None

    match line with
    | ParseRegex @"(\d+)-(\d+) (\w): (\w+)" [min; max; chr; pw] -> { rule = { min = int min; max = int max; letter = char chr }; password = pw }
    | _ -> raise (ParseException($"Invalid line {line}"))

let check_line_stage_1 (line: LineData) =
    let char_count = line.password.Count (fun c -> c = line.rule.letter)
    char_count >= line.rule.min && char_count <= line.rule.max

let check_line_stage_2 (line: LineData) =
    let position1 = line.password.[line.rule.min - 1]
    let position2 = line.password.[line.rule.max - 1]
    (position1 = line.rule.letter || position2 = line.rule.letter) && not (position1 = line.rule.letter && position2 = line.rule.letter)

let run (lines: IEnumerable<string>) =
    Log.Information $"Executing Day 2"

    let parsedLines = lines |> Seq.map convert_line
    let stage1Results = parsedLines |> Seq.filter check_line_stage_1
    let stage2Results = parsedLines |> Seq.filter check_line_stage_2
    let stage1 = stage1Results.Count()
    let stage2 = stage2Results.Count()

    Log.Information $"{stage1} passwords valid stage 1"
    Log.Information $"{stage2} passwords valid stage 2"

    0