module Tests

open FsUnit
open Xunit

open Core.ShuttleSearch
open Inputs


[<Fact>]
let ``small set part 1`` () =
    smallSet |> search |> should equal 295

[<Fact>]
let ``big set part 1`` () =
    bigSet |> search |> should equal 1835


