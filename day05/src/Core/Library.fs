namespace Core

module Plane =

    let computeRow row =
        let rec compute row count =
            match row with
            | [] -> 0
            | 'B'::rest -> (pown 2 count) + compute rest (count - 1)
            | 'F'::rest -> compute rest (count - 1)
            | _ -> failwith "false letter in row"
        compute row 6

    let computeCol col =
        let rec compute col count =
            match col with
            | [] -> 0
            | 'R'::rest -> (pown 2 count) + compute rest (count - 1)
            | 'L'::rest -> compute rest (count - 1)
            | _ -> failwith "false letter in col"
        compute col 2


    let getRowCol (board:string) =
        let row = board |> Seq.take 7 |> List.ofSeq |> computeRow
        let col = board |> Seq.skip 7 |> List.ofSeq |> computeCol
        (row, col)

    let calcId (row, col) =
        row * 8 + col

    let getId (board: string) =
        getRowCol board |> calcId

    let generateAllIds =
        let rows = seq {1..126}
        let cols = seq {0..7}
        cols |> Seq.allPairs rows |> Seq.map calcId |> List.ofSeq
