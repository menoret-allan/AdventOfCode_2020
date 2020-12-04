module ParserTests

open Xunit
open FsUnit

open Parser.ScanParse
open Core.Passport
open Inputs

[<Fact>]
let ``Parser group small input into 4 groups`` () =
    smallScan |> splitPassword |> should equal [
        "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm";
        "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929";
        "hcl:#ae17e1 iyr:2013 eyr:2024 ecl:brn pid:760753108 byr:1931 hgt:179cm";
        "hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in"]


[<Fact>]
let ``Parser to category ecl:gry`` () =
    "ecl:gry" |> toField |> should equal {Category=Ecl; Value= "gry"}

[<Fact>]
let ``Parser to category pid:860033327`` () =
    "pid:860033327" |> toField |> should equal {Category=Pid; Value= "860033327"}

[<Fact>]
let ``Parser to category eyr:2020`` () =
    "eyr:2020" |> toField |> should equal {Category=Eyr ; Value="2020"}

[<Fact>]
let ``Parser to category hcl:#fffffd`` () =
    "hcl:#fffffd" |> toField |> should equal {Category=Hcl; Value= "#fffffd"}

[<Fact>]
let ``Parser to category byr:1937`` () =
    "byr:1937" |> toField |> should equal {Category=Byr; Value= "1937"}

[<Fact>]
let ``Parser to category iyr:2017`` () =
    "iyr:2017" |> toField |> should equal {Category=Iyr; Value= "2017"}

[<Fact>]
let ``Parser to category cid:147`` () =
    "cid:147" |> toField |> should equal {Category=Cid; Value= "147"}

[<Fact>]
let ``Parser to category hgt:183cm`` () =
    "hgt:183cm" |> toField |> should equal {Category=Hgt; Value= "183cm"}


[<Fact>]
let ``Parser all gives list`` () =
    "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm" |>
    translatePassword |> should equal [
        {Category=Ecl; Value= "gry"};
        {Category=Pid; Value= "860033327"};
        {Category=Eyr; Value= "2020"};
        {Category=Hcl; Value= "#fffffd"};
        {Category=Byr; Value= "1937"};
        {Category=Iyr; Value= "2017"};
        {Category=Cid; Value= "147"};
        {Category=Hgt; Value= "183cm"}]

