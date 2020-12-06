namespace CoreV2

module Passport =
    type Byr = int
    type Iyr = int
    type Eyr = int
    type Hgt = | Cm of int | In of int
    type Hcl = string
    type Ecl = | Amb | Blu | Brn | Gry | Grn | Hzl | Oth
    type Pid = string
    type Cid = string

    type Category = 
        | Ecl of Ecl
        | Pid of Pid
        | Eyr of Eyr
        | Hcl of Hcl
        | Byr of Byr
        | Iyr of Iyr
        | Hgt of Hgt
        | Cid of Cid

    type Password = Category list

    let containEcl list = list |> List.exists  (fun x -> match x with | Ecl _ -> true | _ -> false)
    let containPid list = list |> List.exists  (fun x -> match x with | Pid _ -> true | _ -> false)
    let containEyr list = list |> List.exists  (fun x -> match x with | Eyr _ -> true | _ -> false)
    let containHcl list = list |> List.exists  (fun x -> match x with | Hcl _ -> true | _ -> false)
    let containByr list = list |> List.exists  (fun x -> match x with | Byr _ -> true | _ -> false)
    let containIyr list = list |> List.exists  (fun x -> match x with | Iyr _ -> true | _ -> false)
    let containHgt list = list |> List.exists  (fun x -> match x with | Hgt _ -> true | _ -> false)

    let mandatories = [containEcl; containPid; containEyr; containHcl; containByr; containIyr; containHgt]

    let containMandatory passport =
        mandatories |> List.forall (fun x -> x passport)

    let checkPassports passports =
        passports |> List.filter containMandatory