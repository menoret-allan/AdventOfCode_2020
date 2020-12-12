namespace Core

module Boat =

    type Direction = North | South | East | West

    type Move = | N of int | S of int | E of int | W of int | F of int
    type Turn = | L of int | R of int

    type Command = | Move of Move | Turn of Turn

    type State = {X:int;Y:int;Angle:int}

    let computeForward state v =
        match state.Angle % 360 with
        | 0 -> {state with X=state.X+v}
        | 90 | -270 -> {state with Y=state.Y+v}
        | 180 | -180 -> {state with X=state.X-v}
        | 270 | -90 -> {state with Y=state.Y-v}
        | _ -> failwith "Not a valid angle"

    let nextStep state command =
        match command with
        | Move (N y) -> {state with Y=state.Y+y}
        | Move (S y) -> {state with Y=state.Y-y}
        | Move (E x) -> {state with X=state.X+x}
        | Move (W x) -> {state with X=state.X-x}
        | Turn (L a) -> {state with Angle=state.Angle+a}
        | Turn (R a) -> {state with Angle=state.Angle-a}
        | Move (F x) -> computeForward state x

    let computeManhattanDistance list =
        let res = list |> List.fold nextStep {X=0;Y=0;Angle=0}
        (abs res.X) + (abs res.Y)
