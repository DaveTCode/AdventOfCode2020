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
                                | 1 -> (Day1.run, "../../../../Inputs/day1.txt")
                                | 2 -> (Day2.run, "../../../../Inputs/day2.txt")
                                | 3 -> (Day3.run, "../../../../Inputs/day3.txt")
                                | 4 -> (Day4.run, "../../../../Inputs/day4.txt")
                                | 5 -> (Day5.run, "../../../../Inputs/day5.txt")
                                | 6 -> (Day6.run, "../../../../Inputs/day6.txt")
                                | 7 -> (Day7.run, "../../../../Inputs/day7.txt")
                                | 8 -> (Day8.run, "../../../../Inputs/day8.txt")
                                | 9 -> (Day9.run, "../../../../Inputs/day9.txt")
                                | 10 -> (Day10.run, "../../../../Inputs/day10.txt")
                                | 11 -> (Day11.run, "../../../../Inputs/day11.txt")
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
