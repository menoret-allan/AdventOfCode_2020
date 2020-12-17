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

    let compute2 (start: int list) limit =
        let s = List.length start
        let rec compute2 (m: Map<int, int>) next count l =
            if limit = count then l
            else
                match m.TryFind(next) with
                | Some pos -> compute2 (m.Add (next, count)) (count-pos) (count+1) next
                | None -> compute2 (m.Add (next, count)) 0 (count+1) next
        compute2 (Map(start |> List.take (s-1) |> List.mapi (fun i x -> (x, i)))) (List.last start) (s-1) 0
    
    let compute3 (start: int list) limit =
        let s = List.length start
        let a = Array.init (limit + List.max start) (fun _ -> -1)
        start |> List.take (s-1) |> List.iteri (fun i x -> a.[x] <- i)
        let rec compute3 next count l =
            if limit = count then l
            else
                let x = a.[next]
                a.[next] <- count
                match x with
                | -1 -> compute3 0 (count+1) next
                | x -> compute3 (count-x) (count+1) next
        compute3 (List.last start) (s-1) 0


