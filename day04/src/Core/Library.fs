namespace Core

module Passport =
    type Category = 
        | Ecl
        | Pid
        | Eyr
        | Hcl
        | Byr
        | Iyr
        | Cid
        | Hgt

    type Field = {Category:Category; Value:string}

    type Password = Category list

    let mandatory = [Ecl;Pid;Eyr;Hcl;Byr;Iyr;Hgt]

    let noMatch list (cat: Category)  =
        list |> List.exists (fun x -> x.Category = cat) |> not

    let isValide pass =
        mandatory |> List.filter (noMatch pass) |> List.isEmpty

    let countValide passports =
        passports |> List.filter isValide |> List.length
