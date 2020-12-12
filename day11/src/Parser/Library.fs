namespace Parser

open Core.SeatingSystem

module SeatingSystem =
    let createCase v =
        match v with
        | 'L' -> Empty
        | '.' -> Floor
        | '#' -> Occupied
        | _ -> failwith "unexpecte case value"

    let scan (map:string) =
        let listPos = map.Split '\n' |> Seq.ofArray |> Seq.mapi (fun y l -> l |> Seq.mapi (fun x v -> ((x, y), createCase v))) |> Seq.concat
        let xLimit = listPos |> Seq.maxBy (fst >> fst) |> fst |> fst
        let yLimit = listPos |> Seq.maxBy (fst >> snd) |> fst |> snd
        {Mapping=Mapping listPos; Limits=(xLimit, yLimit)}