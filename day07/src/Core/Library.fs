﻿namespace Core

module Bags =
    type Bag = string

    type Bagception = {Main:Bag;Ception:Bag list}

    let cleanBags bags targets =
        bags |>
            List.filter (fun {Main=main} -> List.contains main targets |> not)

    let listIntersect list1 list2 =
        Set.intersect (Set.ofList list1) (Set.ofList list2) |> Set.toList
    
    let getMatchBags targets bags =
        bags |> List.filter (fun {Ception=ception} -> ception |> listIntersect targets |> List.isEmpty |> not) |>
            List.map (fun {Main=main} -> main) |>
            List.distinct

    let rec calc2 targets bags =
        let cleaned = cleanBags bags targets
        match List.isEmpty targets with
        | true  -> 0
        | false -> (List.length targets) + calc2 (getMatchBags targets cleaned) cleaned 

    let calc (target:string) (bags:Bagception list) =
        let firstTargets = bags |> getMatchBags [target]
        calc2 firstTargets bags


module BagsPart2 =
    type Bag = string
    type Bagception = {Main:Bag;Ception:(Bag*int) list}

    let rec calc (target:Bag) (bags:Bagception list) =
        let {Ception=ception} = bags |> List.find (fun {Main=main} -> main = target)
        ception |> List.sumBy (fun (b,c) -> c + c * calc b bags)

