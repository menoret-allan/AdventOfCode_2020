namespace Core

module Xmas =

    let compute pos (input: int64 array) =
        let the25 = input |> Array.skip pos |> Array.take 25
        let filetered = the25 |> Array.allPairs the25 |> Array.filter (fun (x,y) -> x<>y) |> Array.map (fun (x,y) -> x+y)
        let targetValue = input.[pos + 25]
        if Array.contains targetValue filetered then None else Some targetValue

    let rec tillFailure pos (input: int64 array) =
        match compute pos input with
        | Some x -> x
        | None -> tillFailure (pos+1) input

    let scan (str:string) =
        let input = str.Split '\n' |> Array.map int64
        tillFailure 0 input
