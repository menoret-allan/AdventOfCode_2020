namespace Parser

open FParsec

module Commun =
    let p = pint32 .>> skipChar '-' .>>. pint32 .>> skipChar ' ' .>>. anyChar .>> skipString ": " .>>. (restOfLine false)

    let parse (s:string) = run p s |> function
        | Success(result, _, _)   -> result
        | _ -> failwith "Fail to parse"


module OldCompany =
    open Core.OldCompany
    open Commun

    let extract (s:string) = parse s |> function
        | (((min, max), c), rest) -> {Policy= {Letter = c; Range={Min=min;Max=max}}; Password=rest}


module Tobboggan =
    open Core.Tobboggan
    open Commun

    let extract (s:string) = parse s |> function
        | (((pos1, pos2), c), rest) -> {Policy= {Letter = c; Positions=pos1::pos2::[] }; Password=rest}

