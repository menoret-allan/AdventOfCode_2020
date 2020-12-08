namespace Parser

open Core.Inst

module Inst =

    let (|Prefix|_|) (p:string) (s:string) =
        if s.StartsWith(p) then Some(s.Substring(p.Length)) else None


    let translate str =
        match str with
        | Prefix "acc " rest -> Acc (int rest)
        | Prefix "jmp " rest -> Jmp (int rest)
        | Prefix "nop " rest -> Nop (int  rest)
        | _ -> failwith "unknow instruction"


    let scan (str:string) =
        str.Split '\n' |> Array.map translate
