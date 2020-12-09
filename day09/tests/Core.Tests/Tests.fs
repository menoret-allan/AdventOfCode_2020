module Tests

open Xunit
open FsUnit

open Inputs
open Core.Xmas

[<Fact>]
let ``small set test`` () =
    smallSet |> scan |> should equal 999

[<Fact>]
let ``big set test`` () =
    bigSet |> scan |> should equal 1212510616L
