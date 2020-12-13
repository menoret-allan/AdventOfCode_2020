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
