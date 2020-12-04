module Tests

open Xunit
open FsUnit

open Parser.ScanParse
open Core.Passport
open Inputs

[<Fact>]
let ``My test`` () =
    Inputs.smallScan |> translateScan |> countValide |> should equal 2

[<Fact>]
let ``Big set`` () =
    Inputs.bigScan |> translateScan |> countValide |> should equal 235

[<Fact>]
let ``Isvalide with valid`` () =
    "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm" |>
        translatePassword |>
        isValide |> should equal true

[<Fact>]
let ``Isvalide with invalid`` () =
    "ecl:gry iyr:2017 cid:147 hgt:183cm" |>
        translatePassword |>
        isValide |> should equal false

[<Fact>]
let ``noMatch false`` () =
    Eyr |> (noMatch [{Category=Eyr;Value="test"}]) |> should equal false
