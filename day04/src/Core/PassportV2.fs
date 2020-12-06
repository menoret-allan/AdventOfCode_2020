namespace CoreV2

module Passport =
    type HgtMeasure = | Cm of int | In of int

    type Byr = int
    type Iyr = int
    type Eyr = int
    type Hgt = HgtMeasure
    type Hcl = string
    type Ecl = | Amb | Blu | Brn | Gry | Grn | Hzl | Oth
    type Pid = string
    type Cid = string

    type Category = 
        | Ecl of Ecl
        | Pid of Pid
        | Eyr of  Eyr
        | Hcl of Hcl
        | Byr of Byr
        | Iyr of Iyr
        | Cid of Cid
        | Hgt of Hgt

    type Password = Category list
