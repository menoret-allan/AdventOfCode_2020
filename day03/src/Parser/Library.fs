namespace Parser

open Core.World


module MapParse =
    let parseLine str : Line =
        str |> Seq.map (fun c -> match c with '#' -> Tree | _ -> Free) |> List.ofSeq

    let parseMap (str: string) : WorldMap =
        str.Split '\n' |> Seq.map parseLine |> List.ofSeq
