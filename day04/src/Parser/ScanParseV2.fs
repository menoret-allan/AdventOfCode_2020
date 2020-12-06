namespace ParserV2

open FParsec

open CoreV2.Passport

module ScanParse =
    let (|Prefix|_|) (p:string) (s:string) =
        if s.StartsWith(p) then
            Some(s.Substring(p.Length))
        else
            None

    let manyDigit = many digit

    let parseManyDigit (s:string) = run manyDigit s |> function
        | Success(result, _, _)   -> Some result
        | _ -> None

    let createEcl str =
        match str with
        | "amb" -> Some (Ecl Amb)
        | "blu" -> Some (Ecl Blu)
        | "brn" -> Some (Ecl Brn)
        | "gry" -> Some (Ecl Gry)
        | "grn" -> Some (Ecl Grn)
        | "hzl" -> Some (Ecl Amb)
        | "oth" -> Some (Ecl Oth)
        | _ -> None

    let createPid str =
        match parseManyDigit str with
        | Some result when List.length result = 9 -> Some (Pid str)
        | _ -> None

    let createEyr s = run pint32 s |> function
        | Success(year, _, _) when year >= 2020 && year <= 2030  -> Some (Eyr year)
        | _ -> None

    let createIyr s = run pint32 s |> function
        | Success(year, _, _) when year >= 2010 && year <= 2020  -> Some (Iyr year)
        | _ -> None

    let createByr s = run pint32 s |> function
        | Success(year, _, _) when year >= 1920 && year <= 2002  -> Some (Byr year)
        | _ -> None

    let toField str =
        match str with
        | Prefix "ecl:" rest -> createEcl rest
        | Prefix "pid:" rest -> createPid rest 
        | Prefix "eyr:" rest -> createEyr rest 
        | Prefix "hcl:" rest -> createHcl rest 
        | Prefix "byr:" rest -> createByr rest 
        | Prefix "iyr:" rest -> createIyr rest 
        | Prefix "cid:" rest -> createCid rest 
        | Prefix "hgt:" rest -> createHgt rest 
        | _ -> failwith "Unknow identification"

    let splitPassword (scan: string) =
        let oneLine = scan.Split '\n' |> List.ofArray |> List.reduce (fun acc x -> acc + " " + x)
        oneLine.Split "  " |> List.ofArray

    let translatePassword (pass: string) =
        pass.Split ' ' |> List.ofArray |> List.map toField 
        //|> List.where (<> None)

    let translateScan (scan: string) =
        scan |> splitPassword |> List.map translatePassword

