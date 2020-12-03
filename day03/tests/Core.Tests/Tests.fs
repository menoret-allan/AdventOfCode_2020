module Tests

open Xunit
open FsUnit

open Parser.MapParse
open Core.World

[<Fact>]
let ``7 trees on the path`` () =
    Inputs.smallMap |> parseMap |> howManyTrees (3, 1) |> should equal 7

[<Fact>]
let ``big map`` () =
    Inputs.bigMap |> parseMap |> howManyTrees (3, 1) |> should equal 169
