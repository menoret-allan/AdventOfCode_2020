module Tests

open System
open Xunit
open FsUnit

open Inputs
open Parser.Form

[<Fact>]
let ``My test`` () =
    smallSet |> joinForm |>  List.sum |> should equal 11

[<Fact>]
let ``My test big`` () =
    bigSet |> joinForm |>  List.sum |> should equal 6430
