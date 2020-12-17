module Tests

open Xunit
open FsUnit

open Core.RambunctiousRecitation

[<Fact>]
let ``part 1 test 1`` () =
    compute [1;3;2] 2020 |> should equal 1
[<Fact>]
let ``part 1 test 2`` () =
    compute [2;1;3] 2020 |> should equal 10
[<Fact>]
let ``part 1 test 3`` () =
    compute [1;2;3] 2020 |> should equal 27
[<Fact>]
let ``part 1 test 4`` () =
    compute [2;3;1] 2020 |> should equal 78
[<Fact>]
let ``part 1 test 5`` () =
    compute [3;2;1] 2020 |> should equal 438
[<Fact>]
let ``part 1 test 6`` () =
    compute [3;1;2] 2020 |> should equal 1836

[<Fact>]
let ``part 1 test 7`` () =
    compute [0;3;6] 4 |> should equal 0

[<Fact>]
let ``part 1 test 8`` () =
    compute [0;3;6] 5 |> should equal 3

[<Fact>]
let ``part 1 bigset`` () =
    compute [14;3;1;0;9;5] 2020 |> should equal 614



