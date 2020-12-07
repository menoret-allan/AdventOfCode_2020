namespace Parser

open Core.BagsPart2

module BagsParser02 =
    let extractBag (str:string) =
        let sp = str.Split ' '
        (sp |> Array.skip 1 |> Array.take 2 |> String.concat " ", sp|> Array.head |> int)

    let splitCeptionBags (bags:string) =
        bags.Split ", " |> Array.map extractBag |> List.ofArray

    let handleCeptionBags bags =
        match bags with
        | "no other bags." -> []
        | _ -> splitCeptionBags bags

    let splitBagAndCeption (bags:string) =
        let res = bags.Split " bags contain "
        {Main = Array.head res; Ception = Array.last res |> handleCeptionBags}

    let scan (bags:string) =
        bags.Split '\n' |> Array.map splitBagAndCeption |> List.ofArray
