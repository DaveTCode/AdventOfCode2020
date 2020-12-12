module Day04

open Serilog
open System
open System.Collections.Generic
open System.Text.RegularExpressions

let (|Height|_|) input =
    let m = Regex.Match(input, @"([0-9]+)(in|cm)")
    if (m.Success) then Some((int m.Groups.[1].Value, m.Groups.[2].Value)) else None

type Passport = {
    byr: string;
    iyr: string;
    eyr: string;
    hgt: string;
    hcl: string;
    ecl: string;
    pid: string;
    cid: string option;
}

let passport_from_dict (dictionary: IDictionary<string, string>) =
    if (dictionary.ContainsKey("byr") && dictionary.ContainsKey("iyr") && dictionary.ContainsKey("eyr") &&
        dictionary.ContainsKey("hgt") && dictionary.ContainsKey("hcl") && dictionary.ContainsKey("ecl") &&
        dictionary.ContainsKey("pid")) then
        Some({
            byr = dictionary.["byr"];
            iyr = dictionary.["iyr"];
            eyr = dictionary.["eyr"];
            hgt = dictionary.["hgt"];
            hcl = dictionary.["hcl"];
            ecl = dictionary.["ecl"];
            pid = dictionary.["pid"];
            cid = if dictionary.ContainsKey("cid") then Some(dictionary.["cid"]) else None;
        })
    else None

let run (lines: IEnumerable<string>) =
    Log.Information $"Executing Day 1"

    let normalised_lines =
        lines
        |> Seq.collect (fun line -> match String.IsNullOrWhiteSpace line with
                                    | true -> "\n"
                                    | false -> $" {line.Trim()}")
        |> Seq.toArray
        |> String
        |> (fun s -> s.Split('\n'))
    
    let passports =
        normalised_lines
        |> Seq.map (fun line -> line.Split(' ', StringSplitOptions.RemoveEmptyEntries)
                                |> Seq.map (fun c -> (c.Split(':').[0], c.Split(':').[1]))
                                |> dict)
        |> Seq.choose passport_from_dict

    let is_valid_passport (passport: Passport) =
        let valid_byr = Regex.IsMatch(passport.byr, @"^[0-9]{4}$") &&
                        match int passport.byr with
                            | x when x >= 1920 && x <= 2002 -> true
                            | _ -> false

        let valid_iyr = Regex.IsMatch(passport.iyr, @"^[0-9]{4}$") &&
                        match int passport.iyr with
                            | x when x >= 2010 && x <= 2020 -> true
                            | _ -> false

        let valid_eyr = Regex.IsMatch(passport.eyr, @"^[0-9]{4}$") &&
                        match int passport.eyr with
                            | x when x >= 2020 && x <= 2030 -> true
                            | _ -> false

        let valid_hgt = match passport.hgt with
                            | Height (value, unit) when (unit = "cm" && value >= 150 && value <= 193) || (unit = "in" && value >= 59 && value <= 76) -> true
                            | _ -> false

        let valid_hcl = Regex.IsMatch(passport.hcl, @"^#[0-9a-f]{6}$")

        let valid_ecl = match passport.ecl with
                            | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" -> true
                            | _ -> false

        let valid_pid = Regex.IsMatch(passport.pid, "^[0-9]{9}$")

        valid_byr && valid_iyr && valid_eyr && valid_hgt && valid_hcl && valid_ecl && valid_pid

    let valid_passports = passports |> Seq.filter is_valid_passport

    Log.Information $"Stage 1: {Seq.length passports} passports valid"
    Log.Information $"Stage 2: {Seq.length valid_passports} passports valid"

    0