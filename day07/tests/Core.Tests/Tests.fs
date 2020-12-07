module Tests

open System
open Xunit
open FsUnit

open Core.Bags
open Parser.BagsParser
open Inputs

[<Fact>]
let ``Parse test`` () =
    "light red bags contain 1 bright white bag, 2 muted yellow bags." |>
        splitBagAndCeption |>
        should equal {Main="light red"; Ception=["bright white";"muted yellow"]}

[<Fact>]
let ``Parse test empty`` () =
    "faded blue bags contain no other bags." |>
        splitBagAndCeption |>
        should equal {Main="faded blue"; Ception=[]}


[<Fact>]
let ``Parse test2`` () =
    "light red bags contain 1 bright white bag, 2 muted yellow bags." |>
        scan |>
        should equal [{Main="light red"; Ception=["bright white";"muted yellow"]}]


[<Fact>]
let ``get match`` () =
    let target = "shiny gold"
    let main = {Main="faded blue"; Ception=["shiny gold"]}
    getMatchBags [target] [main] |> should equal ["faded blue"]


[<Fact>]
let ``calc super smal`` () =
    let target = "shiny gold"
    let main = {Main="faded blue"; Ception=["shiny gold"]}
    calc target [main] |> should equal 1


[<Fact>]
let ``My small test`` () =
    smallSet |> scan |> calc "shiny gold" |> should equal 4

[<Fact>]
let ``My small test2`` () =
    smallSet2 |> scan |> calc "shiny gold" |> should equal 6

[<Fact>]
let ``My big test`` () =
    bigSet |> scan |> calc "shiny gold" |> should equal 378


