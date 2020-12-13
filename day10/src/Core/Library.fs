namespace Core

module AdapterArray =
    let diffBetweenJolt str =
        let res = ("0\n"+str).Split '\n' |> List.ofArray|> List.map int |> List.sort
        (res @ [List.last res + 3]) |> List.pairwise |> List.map (fun (x,y) -> y-x)

    let compute (str:string) =
        let (x,y,z) = (diffBetweenJolt str) |> List.fold (fun (x,y,z) n -> match n with |1->(x+1,y,z)|2->(x,y+1,z)|3->(x,y,z+1) |_ -> (x,y,z)) (0,0,0)
        x*z

    let rec generateAll x =
        match x with
        | [] -> (bigint(1), [])
        | 2::x::rest ->
            let (v,_) = generateAll ((x+1)::rest)
            let (v2,r) = generateAll ((x)::rest)
            (v+v2,r)
        | 3::x::rest ->
            let (v2,r) = generateAll ((x)::rest)
            (1I+v2,r)
        | x::rest -> (bigint(1), rest)

    let rec generateAll2 x =
        match (generateAll x) with
        | (v, []) -> v
        | (v, rest) -> v * (generateAll2 rest)

    let compute2 (str:string) =
        let x = (diffBetweenJolt str) |> List.pairwise |> List.map (fun (x,y) -> y+x)
        generateAll2 x
