namespace Parser

module Form =
    let joinForm (forms: string) =
        let oneLine = forms.Split '\n' |> List.ofArray |> List.reduce (fun acc x -> acc + " " + x)
        oneLine.Split "  " |>
            List.ofArray |>
            List.map (fun x -> x |> Seq.filter (fun c -> c <> ' ') |> Seq.distinct |> Seq.length)


    let merge (acc:string) (v:string) =
        acc |> Seq.append v |>
            Seq.groupBy id |>
            Seq.filter (fun (_, v) -> Seq.length v = 2) |> Seq.map (fun (key, _) -> key) |>
            Seq.toArray |>
            System.String
        
    let joinForm2 (forms: string) =
        let oneLine = forms.Split '\n' |> List.ofArray |> List.reduce (fun acc x -> acc + " " + x)
        let result = oneLine.Split "  " |> List.ofArray |> List.map (fun x -> x.Split ' ' |> Array.reduce merge)
        result |> List.sumBy (fun x -> x.Length)

