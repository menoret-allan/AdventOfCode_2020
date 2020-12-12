module Tests

open FsUnit
open Xunit

open Inputs
open Parser.SeatingSystem
open Core.SeatingSystem

[<Fact>]
let ``smallset part 1`` () =
    smallSet |> scan |> compute |> should equal 37

[<Fact>]
let ``smallset part 1 test occupied`` () =
    let mapping = smallSet2 |> scan
    countOccupiedAround (0,0) mapping.Mapping mapping.Limits |> should equal 1

[<Fact>]
let ``smallset part 1 test computeNewState`` () =
    let mapping = smallSet2 |> scan
    computeNewState mapping.Mapping mapping.Limits ((0,0), Occupied) |> should equal ((0,0), Occupied)


[<Fact>]
let ``bigSet part 1`` () =
    bigSet |> scan |> compute |> should equal 2386

