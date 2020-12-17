namespace Core

module RambunctiousRecitation =
    
    
    let compute (start: int list) limit =
        let s = List.length start
        let rec compute start next count =
            if limit = count then start |> List.head
            else
                match List.tryFindIndex ((=)next) start with
                | Some pos -> compute (next::start) (pos+1) (count+1)
                | None -> compute (next::start) 0 (count+1)
        compute (start |> List.rev |> List.skip 1) (List.last start) (s-1) 


