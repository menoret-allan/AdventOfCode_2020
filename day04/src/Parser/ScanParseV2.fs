namespace ParserV2

open FParsec

open CoreV2.Passport

module ScanParse =
    let (|Prefix|_|) (p:string) (s:string) =
        if s.StartsWith(p) then
            Some(s.Substring(p.Length))
        else
            None

    let (|Sizing|_|) (p:string) (s:string) =
        if s.EndsWith(p) then
            let rest = s.Substring(0, s.Length - p.Length)
            match run pint32 rest with
            | Success(height, _, _) -> Some (height)
            | _ -> None
        else
            None

    let manyDigit = many digit
    let manyHex = many hex

    let parseManyDigit (s:string) = run manyDigit s |> function
        | Success(result, _, _)   -> Some result
        | _ -> None

    let parseManyHex (s:string) = run manyHex s |> function
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

    let createHclFromDigit s =
        match parseManyHex s with
        | Some result when List.length result = 6 -> Some (Hcl s)
        | _ -> None

    let createHcl s =
        match s with
        | Prefix "#" rest -> createHclFromDigit rest
        | _ -> None

    let createCid s = Some (Cid s)

    let createHgt s =
        match s with
        | Sizing "cm" h when h >= 150 &&  h <= 193 -> Some (Hgt (Cm h))
        | Sizing "in" h when h >= 59 &&  h <= 76 -> Some (Hgt (In h))
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
        pass.Split ' ' |> List.ofArray |> List.map toField |> List.filter ((<>) None) |> List.map (fun x -> match x with | Some v -> v | None -> failwith "Not possible should have been filtered")

    let translateScan (scan: string) =
        scan |> splitPassword |> List.map translatePassword

