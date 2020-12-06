namespace Parser

module Form =
    let joinForm (forms: string) =
        let oneLine = forms.Split '\n' |> List.ofArray |> List.reduce (fun acc x -> acc + " " + x)
        oneLine.Split "  " |>
            List.ofArray |>
            List.map (fun x -> x |> Seq.filter (fun c -> c <> ' ') |> Seq.distinct |> Seq.length)
