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

[<Theory>]
[<InlineData("abc", "abc", "abc")>]
[<InlineData("abc", "", "")>]
[<InlineData("", "abc", "")>]
[<InlineData("aebcd", "xabyc", "abc")>]
let ``merge`` (acc, str, expected) =
    merge acc str |> should equal expected



[<Fact>]
let ``My test part 2`` () =
    smallSet |> joinForm2 |> should equal 6

[<Fact>]
let ``My test big part 2`` () =
    bigSet |> joinForm2 |> should equal 3125
