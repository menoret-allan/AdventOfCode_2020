namespace Core

module OldCompany =
    type Range = {Min: int; Max:int}
    type Policy = {Letter: char; Range: Range}
    type PaPo = {Policy: Policy; Password: string}
    let validate {Policy=policy; Password=password} =
        password |> Seq.countBy id |> Seq.exists (fun (key, count) -> key = policy.Letter && count >= policy.Range.Min && count <= policy.Range.Max)

    let numberOfGoodPassword (passwords: PaPo List) =
        passwords |> List.filter validate |> List.length 

module Tobboggan =
    type Positions = int list
    type Policy = {Letter: char; Positions: Positions}
    type PaPo = {Policy: Policy; Password: string}

    let validate {Policy=policy; Password=password} =
        policy.Positions
            |> List.map (fun pos -> password |> Seq.item (pos-1))
            |> List.filter (fun c -> c = policy.Letter)
            |> List.length = 1

    let numberOfGoodPassword (passwords: PaPo List) =
        passwords |> List.filter validate |> List.length 
