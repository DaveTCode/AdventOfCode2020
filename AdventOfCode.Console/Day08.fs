module Day08

open Common
open Serilog
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

type Instruction =
    | Acc of inc: int
    | Nop of noval: int
    | Jmp of rel: int

type Handheld = {
    mutable accumulator: int;
    mutable program_counter: int;
    program: Instruction[];
}

let run_program_to_completion program =
    let mutable success = None
    let visited_pc = HashSet()
    while success = None do
        match program.program.[program.program_counter] with
            | Nop(_) -> program.program_counter <- program.program_counter + 1
            | Acc(i) ->
                program.program_counter <- program.program_counter + 1
                program.accumulator <- program.accumulator + i
            | Jmp(r) -> program.program_counter <- program.program_counter + r

        if program.program_counter = program.program.Length then
            success <- Some(true)
        elif program.program_counter < 0 || program.program_counter > program.program.Length then
            success <- Some(false)
        elif visited_pc.Contains program.program_counter then
            success <- Some(false)
        else
            visited_pc.Add program.program_counter |> ignore

    success

let program_to_6502 program =
    let ines_header = seq { 0x4E; 0x45; 0x53; 0x1A; 0x1; 0x0; 0x0; 0x0; 0x0; 0x0; 0x0; 0x0; 0x0; 0x0; 0x0; 0x0 } |> Seq.map byte
    let prg_rom = program.program
                  |> Seq.collect (fun instruction -> match instruction with
                                                     | Nop(_) -> Seq.singleton (byte 0xEA)
                                                     | Acc(x) when x > -256 && x < 256 -> seq { byte 0x69; byte x }
                                                     | Acc(x) -> seq { byte 0x69; byte (x - 256) } // TODO - Handle 16 bit adc/sbc
                                                     | Jmp(r) -> seq { byte 0x38; byte 0xB0; byte r })
                  |> Seq.toList

    Array.init 0x4000 (fun i -> match i with
                                | ix when ix = 0x3FFD -> byte 0x80
                                | ix when ix < (Seq.length prg_rom) -> byte prg_rom.[i]
                                | _ -> byte 0)
        |> Seq.append ines_header

let run (lines: IEnumerable<string>) =
    Log.Information $"Executing Day 8"

    let parse_instruction line =
        let m = Regex.Match(line, @"^(acc|nop|jmp) ([+-]\d+)$")
        if not m.Success then raise (ParseException $"Invalid line {line}")
        let v = int (m.Groups.[2].Value.TrimStart('+'))

        match m.Groups.[1].Value with
            | "acc" -> Acc(inc = v)
            | "nop" -> Nop(noval = v)
            | "jmp" -> Jmp(rel = v)
            | _ -> raise (ParseException $"Invalid instruction {line}")

    let base_program = {
        accumulator = 0;
        program_counter = 0;
        program = lines |> Seq.map parse_instruction |> Seq.toArray
    }

    let all_possible_programs =
        base_program.program
        |> Seq.zip (seq { 0 .. base_program.program.Length })
        |> Seq.choose (fun (ix, instruction) -> match instruction with
                                                | Nop(n) ->
                                                    let new_instr = Jmp(n)
                                                    Some({ accumulator = 0; program_counter = 0; program = seq { yield! base_program.program.[0..ix-1]; yield new_instr; yield! base_program.program.[ix+1..]; } |> Seq.toArray})
                                                | Jmp(r) ->
                                                    let new_instr = Nop(r)
                                                    Some({ accumulator = 0; program_counter = 0; program = seq { yield! base_program.program.[0..ix-1]; yield new_instr; yield! base_program.program.[ix+1..]; } |> Seq.toArray})
                                                | _ -> None)
        |> Seq.toList

    let nes_rom_base_program = program_to_6502 base_program

    File.WriteAllBytes("rom.nes", nes_rom_base_program |> Seq.toArray)

    let _ = run_program_to_completion base_program
    Log.Information $"Stage 1: {base_program.accumulator}"

    for program in all_possible_programs do
        match run_program_to_completion program with
            | Some(true) -> Log.Information $"Stage 2: {program.accumulator}"
            | _ -> ()
    
    0