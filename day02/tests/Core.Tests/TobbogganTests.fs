module TobbogganTests

open Xunit
open Parser.Tobboggan
open FsUnit
open Core.Tobboggan

[<Fact>]
let ``Parser   `` () =
    extract "1-3 a: abcde" |> should equal {Policy= {Letter = 'a'; Positions=1::3::[]}; Password="abcde"}

[<Fact>]
let ``Tobbogan 1`` () =
    "1-3 a: abcde" |> extract |> validate |> should equal true

[<Fact>]
let ``Tobbogan 2`` () =
    "1-3 b: cdefg" |> extract |> validate |> should equal false

[<Fact>]
let ``Tobbogan 3`` () =
    "2-9 c: ccccccccc" |> extract |> validate |> should equal false

[<Fact>]
let ``Tobbogan count small`` () =
    let input = ["1-3 a: abcde";"1-3 b: cdefg";"2-9 c: ccccccccc"]
    input |> List.map extract |> numberOfGoodPassword |> should equal 1


[<Fact>]
let ``Tobbogan count big`` () =
    Input.puzzle |> Array.toList |> List.map extract |> numberOfGoodPassword |> should equal 441
