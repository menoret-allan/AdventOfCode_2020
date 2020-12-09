namespace Core

module Inst =

    type Instructions = | Acc of int | Jmp of int | Nop of int

    let computePos pos acc instructions =
        match instructions |> Array.item (pos-1) with
        | Acc n -> (pos+1,acc+n)
        | Jmp n -> (pos+n,acc)
        | Nop n -> (pos+1,acc)

    let rec iterTillLoop pos acc prevPos instructions =
        match (prevPos |> List.contains pos, computePos pos acc instructions) with
        | (true, _) -> acc
        | (_, (x,newAcc)) -> iterTillLoop x newAcc (pos::prevPos) instructions

    let compute instructions =
        instructions |> iterTillLoop 1 0 []

    let rec iterTillLoop3 pos acc prevPos instructions =
        match (prevPos |> List.contains pos, (pos > Array.length instructions)) with
        | (_, true) -> Some acc
        | (true, _) -> None
        | (_, _) -> match computePos pos acc instructions with| (x,newAcc) -> iterTillLoop3 x newAcc (pos::prevPos) instructions

    let iterTillLoop2 opt instructions =
        if opt <> None then opt
        else iterTillLoop3 1 0 [] instructions

    let createNewInst instructions pos newVal =
        let t =  Array.copy instructions
        t.[pos] <- newVal
        t

    let generatePossibilities instructions list (pos, inst) =
        match inst with
        | Nop n when n <> 0 -> (createNewInst instructions pos (Jmp n))::list
        | Jmp n -> (createNewInst instructions pos (Nop n))::list
        | _ -> list

    let compute2 instructions =
        let result = instructions |> Array.mapi (fun i x -> (i,x)) |> Array.fold (generatePossibilities instructions) [instructions] |> List.fold iterTillLoop2 None
        match result with
            | Some acc -> acc
            | None -> failwith "nothing found"
