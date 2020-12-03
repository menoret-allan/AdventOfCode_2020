module ParserTests

open System
open Xunit
open FsUnit

open Parser.MapParse
open Core.World

[<Fact>]
let ``Parse line`` () =
    "..##......." |> parseLine |> should equal [Free;Free;Tree;Tree;Free;Free;Free;Free;Free;Free;Free]


[<Fact>]
let ``Parse small map should contain .#.#.#....#`` () =
    Inputs.smallMap |> parseMap |> should contain [Free;Tree;Free;Tree;Free;Tree;Free;Free;Free;Free;Tree]

let ``Parse small map`` () =
    Inputs.smallMap |> parseMap |> should haveCount 11






