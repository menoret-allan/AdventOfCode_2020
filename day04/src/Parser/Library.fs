namespace Parser

open Core.Passport

module ScanParse =
    let (|Prefix|_|) (p:string) (s:string) =
        if s.StartsWith(p) then
            Some(s.Substring(p.Length))
        else
            None


    let toField str =
        match str with
        | Prefix "ecl:" rest -> {Category=Ecl; Value=rest}
        | Prefix "pid:" rest -> {Category=Pid; Value=rest} 
        | Prefix "eyr:" rest -> {Category=Eyr; Value=rest} 
        | Prefix "hcl:" rest -> {Category=Hcl; Value=rest} 
        | Prefix "byr:" rest -> {Category=Byr; Value=rest} 
        | Prefix "iyr:" rest -> {Category=Iyr; Value=rest} 
        | Prefix "cid:" rest -> {Category=Cid; Value=rest} 
        | Prefix "hgt:" rest -> {Category=Hgt; Value=rest} 
        | _ -> failwith "Unknow identification"

    let splitPassword (scan: string) =
        let oneLine = scan.Split '\n' |> List.ofArray |> List.reduce (fun acc x -> acc + " " + x)
        oneLine.Split "  " |> List.ofArray

    let translatePassword (pass: string) =
        pass.Split ' ' |> List.ofArray |> List.map toField


    let translateScan (scan: string) =
        scan |> splitPassword |> List.map translatePassword

