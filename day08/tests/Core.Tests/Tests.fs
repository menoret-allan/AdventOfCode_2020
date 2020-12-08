module Tests

open Xunit
open FsUnit

open Core.Inst
open Parser.Inst
open Inputs

[<Fact>]
let ``My small test`` () =
    smallSet |> scan |> compute |> should equal 5

[<Fact>]
let ``My bigg test`` () =
    bigSet |> scan |> compute |> should equal 1501


