
module TestsPart02

open System
open Xunit
open FsUnit

open Core.BagsPart2
open Parser.BagsParser02
open Inputs



[<Fact>]
let ``Parse test`` () =
    "light red bags contain 1 bright white bag, 42 muted yellow bags." |>
        splitBagAndCeption |>
        should equal {Main="light red"; Ception=[("bright white",1);("muted yellow",42)]}

[<Fact>]
let ``Parse test empty`` () =
    "faded blue bags contain no other bags." |>
        splitBagAndCeption |>
        should equal {Main="faded blue"; Ception=[]}


[<Fact>]
let ``Parse test2`` () =
    "light red bags contain 1 bright white bag, 2 muted yellow bags." |>
        scan |>
        should equal [{Main="light red"; Ception=[("bright white",1);("muted yellow",2)]}]


[<Fact>]
let ``My small test part 2`` () =
    smallSet |> scan |> calc "shiny gold" |> should equal 32

[<Fact>]
let ``My small test2  part 2`` () =
    smallSet3 |> scan |> calc "shiny gold" |> should equal 126

[<Fact>]
let ``My big test  part 2`` () =
    bigSet |> scan |> calc "shiny gold" |> should equal 27526

