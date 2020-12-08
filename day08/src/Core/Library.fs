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
