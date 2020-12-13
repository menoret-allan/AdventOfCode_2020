namespace Core

module ShuttleSearch =
    type Schedule = {Timestamp:int; BusID:int list}

    let parse (str:string) =
        let s = str.Split(System.Environment.NewLine)
        let busID = s.[1].Split ',' |> List.ofArray |> List.filter ((<>) "x") |> List.map int
        {Timestamp= int s.[0]; BusID=busID}

    let search (str:string) =
        let {Timestamp=timestamp; BusID=busID} = parse str
        let (id, time) = busID |> List.map (fun x -> (x,(timestamp/x)*x+x)) |> List.minBy snd
        id*(time-timestamp)

    
    let parse2 (str:string) =
        str.Split ',' |> List.ofArray |> List.mapi (fun i v -> i,v) |> List.filter (snd >> (<>) "x") |> List.map (fun (i,v) -> (bigint i), bigint (int v))

    let allIdsWorked ids (target:bigint) (targetI:bigint) =
        ids |> List.exists (fun (i,v) -> (target - targetI + i) % v <> 0I) |> not

    let rec findTimestamp (i, targetId) ids c =
        let target = targetId * c
        printf "c: %A / target %A\n" c target
        match allIdsWorked ids target i with
        | true -> c
        | false -> findTimestamp (i, targetId) ids (c+1I)

    let search2 (str:string) =
        let busIDs = parse2 str |> List.sortByDescending (fun (_,v)-> v)
        let (i,v) = List.head busIDs
        let c = findTimestamp (i,v) (List.skip 1 busIDs) 1I
        let res = v*c-i
        res
