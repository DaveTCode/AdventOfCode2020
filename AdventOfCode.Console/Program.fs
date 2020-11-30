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
                                | 1 -> (Day1.run, "c:/code/adventofcode/2020/AdventOfCode/Inputs/day1.txt")
                                | 2 -> (Day2.run, "c:/code/adventofcode/2020/AdventOfCode/Inputs/day2.txt")
                                | _ -> 
                                    Log.Error $"Day {options.Value.day} not yet written"
                                    exit 1
                            | :? CommandLine.NotParsed<Options> -> exit 1
                            | _ -> 
                                Log.Error "Invalid command line parameters"
                                exit 1

    let lines = File.ReadLines inputFile

    func lines