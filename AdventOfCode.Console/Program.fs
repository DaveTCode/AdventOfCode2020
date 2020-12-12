open CommandLine
open Serilog
open System.IO

type Options = {
    [<Value(0, MetaName="day", HelpText = "AOC Day (1-25)")>] day : int;
}

[<EntryPoint>]
let main args =
    Log.Logger <- LoggerConfiguration()
        .WriteTo.Console()
        .CreateLogger()
    
    Log.Information "Starting application"
    
    let result = Parser.Default.ParseArguments<Options> args
    let (func, inputFile) = match result with
                            | :? CommandLine.Parsed<Options> as options ->
                                match options.Value.day with
                                | 01 -> (Day01.run, "../../../../Inputs/day01.txt")
                                | 02 -> (Day02.run, "../../../../Inputs/day02.txt")
                                | 03 -> (Day03.run, "../../../../Inputs/day03.txt")
                                | 04 -> (Day04.run, "../../../../Inputs/day04.txt")
                                | 05 -> (Day05.run, "../../../../Inputs/day05.txt")
                                | 06 -> (Day06.run, "../../../../Inputs/day06.txt")
                                | 07 -> (Day07.run, "../../../../Inputs/day07.txt")
                                | 08 -> (Day08.run, "../../../../Inputs/day08.txt")
                                | 09 -> (Day09.run, "../../../../Inputs/day09.txt")
                                | 10 -> (Day10.run, "../../../../Inputs/day10.txt")
                                | 11 -> (Day11.run, "../../../../Inputs/day11.txt")
                                | 12 -> (Day12.run, "../../../../Inputs/day12.txt")
                                | 13 -> (Day13.run, "../../../../Inputs/day13.txt")
                                | 14 -> (Day14.run, "../../../../Inputs/day14.txt")
                                | 15 -> (Day15.run, "../../../../Inputs/day15.txt")
                                | 16 -> (Day16.run, "../../../../Inputs/day16.txt")
                                | 17 -> (Day17.run, "../../../../Inputs/day17.txt")
                                | 18 -> (Day18.run, "../../../../Inputs/day18.txt")
                                | 19 -> (Day19.run, "../../../../Inputs/day19.txt")
                                | 20 -> (Day20.run, "../../../../Inputs/day20.txt")
                                | 21 -> (Day21.run, "../../../../Inputs/day21.txt")
                                | 22 -> (Day22.run, "../../../../Inputs/day22.txt")
                                | 23 -> (Day23.run, "../../../../Inputs/day23.txt")
                                | 24 -> (Day24.run, "../../../../Inputs/day24.txt")
                                | 25 -> (Day25.run, "../../../../Inputs/day25.txt")
                                | _ -> 
                                    Log.Error $"Day {options.Value.day} not yet written"
                                    exit 1
                            | :? CommandLine.NotParsed<Options> -> exit 1
                            | _ -> 
                                Log.Error "Invalid command line parameters"
                                exit 1

    let lines =
        File.ReadLines inputFile
        |> Seq.toList

    func lines
