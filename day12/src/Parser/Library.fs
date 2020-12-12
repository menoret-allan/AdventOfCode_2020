namespace Parser

open Core.Boat

module Boat =

    let (|Prefix|_|) (p:char) (s:string) =
        if (s.StartsWith(p)) then Some (s.Substring(1) |> int) else None


    let parseLine str =
        match str with
        | Prefix 'N' x -> N x |> Move
        | Prefix 'S' x -> S x |> Move
        | Prefix 'E' x -> E x |> Move 
        | Prefix 'W' x -> W x |> Move 
        | Prefix 'F' x -> F x |> Move 
        | Prefix 'R' x -> R x |> Turn
        | Prefix 'L' x -> L x |> Turn
        | _ -> failwith "unknowm command"

    let scan (str:string) =
        str.Split '\n' |> List.ofArray |> List.map parseLine
