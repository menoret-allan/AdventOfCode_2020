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
    bigSet |> scan |> computeManhattanDistance |> should equal 415



[<Theory>]
[<InlineData(90, 1, -1)>]
[<InlineData(180, -1, -1)>]
[<InlineData(270, -1, 1)>]
let ``turnClockWise`` (angle, x, y) =
    turnClockWise {X=1;Y=1} angle |> should equal {X=x;Y=y}

[<Theory>]
[<InlineData(90, -1, 1)>]
[<InlineData(180, -1, -1)>]
[<InlineData(270, 1, -1)>]
let ``turnCounterClockWise`` (angle, x, y) =
    turnCounterClockWise {X=1;Y=1} angle |> should equal {X=x;Y=y}


[<Fact>]
let ``small set part 02`` () =
    samllSet |> scan |> computeManhattanDistance2 |> should equal 286

[<Fact>]
let ``big set part 02`` () =
    bigSet |> scan |> computeManhattanDistance2 |> should equal 29401
