module Tests

open Xunit
open FsUnit

open Core.AdapterArray
open Inputs

[<Fact>]
let ``small set part1`` () =
    smallSet |> compute |> should equal 220

[<Fact>]
let ``big set part1`` () =
    bigSet |> compute |> should equal 1920


[<Fact>]
let ``super small set part2`` () =
    superSmallSet |> compute2 |> should equal 7I
    
[<Fact>]
let ``super small3 set part2`` () =
    superSmallSet3 |> compute2 |> should equal 3I

[<Fact>]
let ``super small4 set part2`` () =
    superSmallSet4 |> compute2 |> should equal 2I

[<Fact>]
let ``super small5 set part2`` () =
    superSmallSet5 |> compute2 |> should equal 2I

[<Fact>]
let ``extra small set part2`` () =
    extraSmallSet |> compute2 |> should equal 8I

[<Fact>]
let ``small set part2`` () =
    smallSet |> compute2 |> should equal 19208I

[<Fact>]
let ``big set part2`` () =
    bigSet |> compute2 |> should equal 1511207993344I
