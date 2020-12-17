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




[<Fact>]
let ``part 1 c2 test 1`` () =
    compute2 [1;3;2] 2020 |> should equal 1
[<Fact>]
let ``part 1 c2 test 2`` () =
    compute2 [2;1;3] 2020 |> should equal 10
[<Fact>]
let ``part 1 c2 test 3`` () =
    compute2 [1;2;3] 2020 |> should equal 27
[<Fact>]
let ``part 1 c2 test 4`` () =
    compute2 [2;3;1] 2020 |> should equal 78
[<Fact>]
let ``part 1 c2 test 5`` () =
    compute2 [3;2;1] 2020 |> should equal 438
[<Fact>]
let ``part 1 c2 test 6`` () =
    compute2 [3;1;2] 2020 |> should equal 1836



[<Fact>]
let ``part 1 c2 test 7`` () =
    compute2 [0;3;6] 4 |> should equal 0

[<Fact>]
let ``part 1 c2 test 8`` () =
    compute2 [0;3;6] 5 |> should equal 3




[<Fact>]
let ``part 2 test 1`` () =
    compute3 [1;3;2] 2020 |> should equal 1
[<Fact>]
let ``part 2 test 2`` () =
    compute3 [2;1;3] 2020 |> should equal 10
[<Fact>]
let ``part 2 test 3`` () =
    compute3 [1;2;3] 2020 |> should equal 27
[<Fact>]
let ``part 2 test 4`` () =
    compute3 [2;3;1] 2020 |> should equal 78
[<Fact>]
let ``part 2 test 5`` () =
    compute3 [3;2;1] 2020 |> should equal 438
[<Fact>]
let ``part 2 test 6`` () =
    compute3 [3;1;2] 2020 |> should equal 1836



[<Fact>]
let ``part 2 test 7`` () =
    compute3 [0;3;6] 4 |> should equal 0

[<Fact>]
let ``part 2 test 8`` () =
    compute3 [0;3;6] 5 |> should equal 3



[<Fact>]
let ``part 2 test 1 2`` () =
    compute3 [1;3;2] 30000000 |> should equal 2578
[<Fact>]
let ``part 2 test 2 2`` () =
    compute3 [2;1;3] 30000000 |> should equal 3544142
[<Fact>]
let ``part 2 test 3 2`` () =
    compute3 [1;2;3] 30000000 |> should equal 261214
[<Fact>]
let ``part 2 test 4 2 `` () =
    compute3 [2;3;1] 30000000 |> should equal 6895259
[<Fact>]
let ``part 2 test 5 2`` () =
    compute3 [3;2;1] 30000000 |> should equal 18
[<Fact>]
let ``part 2 test 6 2`` () =
    compute3 [3;1;2] 30000000 |> should equal 362


[<Fact>]
let ``part 2 bigset`` () =
    compute3 [14;3;1;0;9;5] 30000000 |> should equal 1065
