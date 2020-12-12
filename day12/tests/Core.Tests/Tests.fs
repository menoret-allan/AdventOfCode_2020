module Tests

open Xunit
open FsUnit

open Core.Boat
open Parser.Boat
open Inputs

[<Fact>]
let ``small set part 01`` () =
    samllSet |> scan |> computeManhattanDistance |> should equal 25

[<Fact>]
let ``small set 2 part 01`` () =
    samllSet2 |> scan |> computeManhattanDistance |> should equal 0

[<Fact>]
let ``big set part 01`` () =
    bigSet |> scan |> computeManhattanDistance |> should equal 25
