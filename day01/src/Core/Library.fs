namespace Core

module Report =
    let fixExpense (expenses: int list) =
        let (x,y) = expenses |> List.allPairs expenses |> List.find (fun (x, y) -> x+y = 2020)
        x*y

    let fixExpense2 (expenses: int list) =
        let (x,(y,z)) = expenses |> List.allPairs expenses |> List.allPairs expenses |> List.find (fun (x, (y, z)) -> x+y+z = 2020)
        x*y*z
