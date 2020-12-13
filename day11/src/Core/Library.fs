namespace Core

module SeatingSystem =

    type State = | Floor | Occupied | Empty
    type Mapping = Map<(int*int), State>
    type SeatMap = {Mapping:Mapping; Limits:int*int}

    let translate (_, v) = match v with | Occupied -> "#" | Floor -> "." | Empty -> "L"

    let printMap (maps: SeatMap) =
        printf "\n%i-%i {0,0}=%s\n" (maps.Limits |> fst) (maps.Limits |> snd) (translate ((0,0),maps.Mapping.[(0,0)]))
        let t = maps.Mapping |> Map.toSeq |> Seq.sortBy (fun ((x,y),_) -> (y*100+x)) |> Seq.map translate
        t |> Seq.chunkBySize ((maps.Limits |> snd) + 1) |> Seq.iter (Seq.reduce (+) >> printf "%s\n") 

    let excludedInvalidSeats (currentX,currentY) (limitX, limitY) value =
        match value with
        | (x,_) when x < 0 || x > limitX -> false
        | (_,y) when y < 0 || y > limitY -> false
        | (x,y) when x=currentX && y=currentY -> false
        | _ -> true

    let countOccupiedAround (x, y) (mapping:Mapping) limits =
        let res = (seq {y-1..y+1})
                |> Seq.allPairs (seq {x-1..x+1})
                |> Seq.filter (excludedInvalidSeats (x,y) limits)
                |> Seq.map (fun pos -> mapping.[pos])
                |> Seq.filter ((=)Occupied)
        Seq.length res

    let computeNewState mapping limit (pos,v) =
        if v = Floor then (pos, v)
        else
            let numberOccupied = countOccupiedAround pos mapping limit
            match (numberOccupied, mapping.[pos]) with
                | (0, _) -> ((pos), Occupied)
                | (n, Occupied) when n >=4 -> ((pos), Empty)
                | _ -> (pos, v)

    let newCycle seatMap =
        let res = seatMap.Mapping |> Map.toSeq |> Seq.map (computeNewState seatMap.Mapping seatMap.Limits)
        { seatMap with Mapping = Mapping res}

    let countOccupiedSeats mapping =
        let res = mapping |> Map.toSeq |> Seq.filter (snd >> ((=) Occupied))
        Seq.length res

    let mapAreSame (map1: SeatMap) (map2: SeatMap)  =
        map1 = map2

    let compute (sitsMap: SeatMap) =
        let rec compute oldMap newMap =
            if mapAreSame oldMap newMap then countOccupiedSeats newMap.Mapping
            else compute newMap  (newCycle newMap)
        compute sitsMap (newCycle sitsMap)

    let isOccupiedOnSight (limitX, limitY) value (mapping:Mapping) (func: int*int->int*int) =
        let rec isOccupiedOnSight value =
            match value with
            | (x,_) when x < 0 || x > limitX -> 0
            | (_,y) when y < 0 || y > limitY -> 0
            | pos when mapping.[pos] = Empty -> 0
            | pos when mapping.[pos] = Occupied -> 1
            | _ -> (isOccupiedOnSight (func value))
        isOccupiedOnSight (func value)

    let straight (x,y) = (x+1,y)
    let up (x,y) = (x,y-1)
    let back (x,y) = (x-1,y)
    let down (x,y) = (x,y+1)
    let straightUp (x,y) = (x+1,y-1)
    let straightDown (x,y) = (x+1,y+1)
    let backUp (x,y) = (x-1,y-1)
    let backDown (x,y) = (x-1,y+1)

    let translationFuncs = [straight;up;back;down;straightUp;straightDown;backUp;backDown]

    let countOccupiedAround2 pos (mapping:Mapping) limits =
        translationFuncs |> List.sumBy (fun x -> isOccupiedOnSight limits pos mapping x)

    let computeNewState2 mapping limit (pos,v) =
        if v = Floor then (pos, v)
        else
            match (countOccupiedAround2 pos mapping limit, mapping.[pos]) with
                | (0, _) -> ((pos), Occupied)
                | (n, Occupied) when n >=5 -> ((pos), Empty)
                | _ -> (pos, v)

    let newCycle2 seatMap =
        let res = seatMap.Mapping |> Map.toSeq |> Seq.map (computeNewState2 seatMap.Mapping seatMap.Limits)
        { seatMap with Mapping = Mapping res}

    let compute2 (sitsMap: SeatMap) =
        let rec compute oldMap newMap =
            if mapAreSame oldMap newMap then
                countOccupiedSeats newMap.Mapping
            else compute newMap  (newCycle2 newMap)
        compute sitsMap (newCycle2 sitsMap)
