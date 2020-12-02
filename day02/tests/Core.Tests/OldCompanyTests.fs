module Tests

open System
open Xunit
open Parser.OldCompany
open FsUnit
open Core.OldCompany

[<Fact>]
let ``Parser   `` () =
    extract "1-3 a: abcde" |> should equal {Policy= {Letter = 'a'; Range={Min=1;Max=3}}; Password="abcde"}

[<Fact>]
let ``Tobbogan count small`` () =
    let input = ["1-3 a: abcde";"1-3 b: cdefg";"2-9 c: ccccccccc"]
    input |> List.map extract |> numberOfGoodPassword |> should equal 2


[<Fact>]
let ``Tobbogan count big`` () =
    Input.puzzle |> Array.toList |> List.map extract |> numberOfGoodPassword |> should equal 467
