namespace Core

module Xmas =

    let compute pos (input: int64 array) =
        let the25 = input |> Array.skip pos |> Array.take 25
        let filetered = the25 |> Array.allPairs the25 |> Array.filter (fun (x,y) -> x<>y) |> Array.map (fun (x,y) -> x+y)
        let targetValue = input.[pos + 25]
        if Array.contains targetValue filetered then None else Some targetValue

    let rec tillFailure pos (input: int64 array) =
        match compute pos input with
        | Some x -> (pos+25,x)
        | None -> tillFailure (pos+1) input

    let scan (str:string) =
        let input = str.Split '\n' |> Array.map int64
        let (_, x) = tillFailure 0 input
        x

    let rec generateList target input list =
        match (target - (list |> List.sum), input, list) with
        | (0L, [], _) -> seq {yield list}
        | (0L, n::rest, _) -> seq {yield list; yield! generateList target rest (list @ [n])}
        | (_, [], _) -> Seq.empty
        | (x, n::rest, _) when x > 0L -> seq {yield! generateList target rest (list @ [n])}
        | (x, _, n::rest) when x < 0L -> seq {yield! generateList target input rest}
        | _ -> failwith "no match inside hte list generator"



    let scan2 (str:string) =
        let input = str.Split '\n' |> Array.map int64
        let (pos, x) = tillFailure 0 input
        let inputReduced = input |> Array.take pos |> Array.toList
        let (_, list) = (generateList x inputReduced []) |> Seq.map (fun x -> (List.length x, x)) |> Seq.minBy (fun (s,_) -> s)
        List.max list + (List.min list)

