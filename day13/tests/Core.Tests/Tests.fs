module Tests

open FsUnit
open Xunit

open Core.ShuttleSearch
open Inputs


[<Fact>]
let ``small set part 1`` () =
    smallSet |> search |> should equal 295

[<Fact>]
let ``big set part 1`` () =
    bigSet |> search |> should equal 1835




[<Theory>]
[<InlineData("7,13,x,x,59,x,31,19", 1068781)>]
[<InlineData("17,x,13,19", 3417)>]
[<InlineData("67,7,59,61", 754018)>]
[<InlineData("67,x,7,59,61", 779210)>]
[<InlineData("67,7,x,59,61", 1261476)>]
[<InlineData("1789,37,47,1889", 1202161486)>]
let ``smallset part 2`` (input, (expectation:int)) =
    input |> search2 |> should equal (bigint expectation)

// [<Fact>]
// let ``big set part 2`` () =
//     bigSet2 |> search2 |> should equal 1835


