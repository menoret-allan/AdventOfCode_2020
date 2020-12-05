module Tests

open System
open Xunit
open FsUnit

open Inputs
open Core.Plane

[<Theory>]
[<InlineData("BFFFBBFRRR", 70, 7)>]
[<InlineData("FFFBBBFRRR", 14, 7)>]
[<InlineData("BBFFBBFRLL", 102, 4)>]
let ``Compute row col`` (input, expectedRow, expectedCol) =
    input |> getRowCol |> should equal (expectedRow, expectedCol)

[<Theory>]
[<InlineData("BFFFBBFRRR", 567)>]
[<InlineData("FFFBBBFRRR", 119)>]
[<InlineData("BBFFBBFRLL", 820)>]
let ``GetId`` (input, expectedId) =
    input |> getId |> should equal expectedId

[<Fact>]
let ``big input`` () =
    bigSet.Split '\n' |> Array.map getId |> Array.max |> should equal 892