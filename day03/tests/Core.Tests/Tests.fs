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


[<Fact>]
let ``small map / part 2 map`` () =
    let map = Inputs.smallMap |> parseMap
    [(1,1);(3,1);(5,1);(7,1);(1,2)] |> List.fold (fun x mov -> (howManyTrees mov map) * x) 1 |> should equal 336


[<Fact>]
let ``part 2 map`` () =
    let map = Inputs.bigMap |> parseMap
    [(1,1);(3,1);(5,1);(7,1);(1,2)] |> List.fold (fun x mov -> (bigint (howManyTrees mov map)) * x) 1I |> should equal 7560370818I
