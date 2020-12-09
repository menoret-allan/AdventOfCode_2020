module Tests

open Xunit
open FsUnit

open Inputs
open Core.Xmas

[<Fact>]
let ``small set test`` () =
    smallSet |> scan |> should equal 204L

[<Fact>]
let ``big set test`` () =
    bigSet |> scan |> should equal 1212510616L

[<Fact>]
let ``small set test part 2`` () =
    smallSet |> scan2 |> should equal 102L

[<Fact>]
let ``big set test part 2`` () =
    bigSet |> scan2 |> should equal 171265123L
