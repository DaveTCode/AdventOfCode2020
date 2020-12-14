module Day14

open Serilog
open System.Collections.Generic
open System
open System.Text.RegularExpressions
open Common

type Address = int64

type Mask = {
    zero_mask: Address;
    one_mask: Address;
    mask: char[];
}

type ProgramState = {
    mask: Mask;
    memory: Dictionary<Address, int64>;
}

type Instruction =
    | MaskInstruction of mask: Mask
    | MemoryInstruction of address: Address * value: int64

let run (lines: IEnumerable<string>) =
    Log.Information $"Executing Day 14"

    let string_to_mask (line: string) =
        let clean_line = line.Trim()
        {
            zero_mask = Convert.ToInt64(clean_line.Replace('1', 'X').Replace('0', '1').Replace('X', '0'), 2);
            one_mask = Convert.ToInt64(clean_line.Replace('X', '0'), 2);
            mask = clean_line.ToCharArray();
        }

    let string_to_instruction (line: string) =
        let parts = line.Split('=')
        match parts.[0].[1] with
            | 'a' -> MaskInstruction(mask = (string_to_mask parts.[1]))
            | 'e' ->
                let groups = Regex.Match(parts.[0], @"mem\[(\d+)\]");
                MemoryInstruction(address = int64 groups.Groups.[1].Value, value = int64 parts.[1])
            | _ -> raise (ParseException $"Invalid line {line}")


    let instructions = Seq.map string_to_instruction lines

    let emulator_v1_mem_instruction address value state =
        let masked_value = (value ||| state.mask.one_mask) &&& (~~~ state.mask.zero_mask)
        if state.memory.ContainsKey(address) then
            state.memory.[address] <- masked_value
        else
            state.memory.Add(address, masked_value)

        state

    let rec mask_combinations (acc: char list) tail: seq<char list> =
        seq {
            match tail with
            | [] -> acc
            | ls ->
                let cur = List.head ls
                let trailer = List.tail ls
                match cur with
                | '1' | '0' -> yield! (mask_combinations (acc @ [ cur ]) trailer)
                | 'X' ->
                    yield! (mask_combinations (acc @ [ '0' ]) trailer)
                    yield! (mask_combinations (acc @ [ '1' ]) trailer)
                | _ -> raise (ParseException $"Invalid char{cur}")
        }

    let apply_mask (address: char[]) (mask: char[]) =
        String (Seq.zip address mask
                |> Seq.map (fun (a, m) -> match m with
                                            | '1' | 'X' -> m
                                            | '0' -> a
                                            | _ -> raise (ParseException $"Invalid char {m}"))
                |> Seq.toArray)
            

    let emulator_v2_mem_instruction address value state =
        let address_str = Convert.ToString(int64 address, 2).PadLeft(36, '0').ToCharArray()
        let masked_addr = apply_mask address_str state.mask.mask 
        let combinations =
            mask_combinations [] (Seq.toList masked_addr)
            |> Seq.map (Seq.toArray) |> Seq.map String |> Seq.map (fun s -> Convert.ToInt64(s, 2))

        for a in combinations do
            state.memory.[a] <- value

        state


    let rec emulator state instructions memory_instruction =
        if Seq.length instructions = 0 then
            state
        else
            let new_state = match (Seq.head instructions) with
                            | MaskInstruction(m) -> { state with mask = m }
                            | MemoryInstruction(address, value) -> memory_instruction address value state
            emulator new_state (Seq.tail instructions) memory_instruction

    let task1_state = { mask = { zero_mask = 0L; one_mask = 0L; mask = "0".ToCharArray() }; memory = new Dictionary<Address, int64>(); }
    let task2_state = { mask = { zero_mask = 0L; one_mask = 0L; mask = "0".ToCharArray() }; memory = new Dictionary<Address, int64>(); }

    let task1_final_state = emulator task1_state instructions emulator_v1_mem_instruction
    let task2_final_state = emulator task2_state instructions emulator_v2_mem_instruction

    let task1_val =
        task1_final_state.memory
        |> Seq.map (fun kv -> kv.Value)
        |> Seq.sum

    let task2_val =
        task2_final_state.memory
        |> Seq.map (fun kv -> kv.Value)
        |> Seq.sum

    Log.Information $"Task 1 memory sum {task1_val}"
    Log.Information $"Task 2 memory sum {task2_val}"

    0