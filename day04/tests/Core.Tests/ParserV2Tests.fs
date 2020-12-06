module ParserV2Tests

open Xunit
open FsUnit
open Inputs

open ParserV2.ScanParse
open CoreV2.Passport

[<Fact>]
let ``Parser group small input into 4 groups`` () =
    smallScan |> splitPassword |> should equal [
        "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm";
        "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929";
        "hcl:#ae17e1 iyr:2013 eyr:2024 ecl:brn pid:760753108 byr:1931 hgt:179cm";
        "hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in"]



[<Fact>]
let ``Check invalid`` () =
    smallInvalidScan |> translateScan |> checkPassports |> List.length |> should equal 0

[<Fact>]
let ``Check valid`` () =
    smallValidScan |> translateScan |> checkPassports |> List.length |> should equal 4

[<Fact>]
let ``Check all`` () =
    bigScan |> translateScan |> checkPassports |> List.length |> should equal 194



