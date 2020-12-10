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
