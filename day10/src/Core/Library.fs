namespace Core

module AdapterArray =
    
    
    let compute (str:string) =
        let (x,y,z) = ("0\n"+str).Split '\n' |> List.ofArray|> List.map int |> List.sort |> List.pairwise |> List.map (fun (x,y) -> y-x) |> List.fold (fun (x,y,z) n -> match n with |1->(x+1,y,z)|2->(x,y+1,z)|3->(x,y,z+1) |_ -> (x,y,z)) (0,0,0)
        x*(z+1)
