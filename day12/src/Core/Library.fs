namespace Core

module Boat =

    type Direction = North | South | East | West

    type Move = | N of int | S of int | E of int | W of int | F of int
    type Turn = | L of int | R of int

    type Command = | Move of Move | Turn of Turn

    type State = {X:int;Y:int;Angle:int}

    type Pos = {X:int;Y:int}

    let computeForward state v =
        match state.Angle % 360 with
        | 0 -> {state with X=state.X+v}
        | 90 | -270 -> {state with Y=state.Y+v}
        | 180 | -180 -> {state with X=state.X-v}
        | 270 | -90 -> {state with Y=state.Y-v}
        | _ -> failwith "Not a valid angle"

    let nextStep (state:State) command =
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

    let handle90 (wayPoint:Pos) = {X= -wayPoint.Y;Y= wayPoint.X}
    let handle180 (wayPoint:Pos) = {X= -wayPoint.X;Y= -wayPoint.Y}
    let handle270 (wayPoint:Pos) = {X= wayPoint.Y;Y= -wayPoint.X}

    let turnClockWise (wayPoint:Pos) a =
        match a with
        | 90 -> handle270 wayPoint
        | 180 -> handle180 wayPoint
        | 270 -> handle90 wayPoint
        | _ -> failwith "unvalid angle"

    let turnCounterClockWise (wayPoint:Pos) a =
        match a with
        | 90 -> handle90 wayPoint
        | 180 -> handle180 wayPoint
        | 270 -> handle270 wayPoint
        | _ -> failwith "unvalid angle"

    let nextStep2 ((pos:Pos), (wayPoint:Pos)) command =
        match command with
        | Move (N y) -> (pos, {wayPoint with Y=wayPoint.Y+y})
        | Move (S y) -> (pos, {wayPoint with Y=wayPoint.Y-y})
        | Move (E x) -> (pos, {wayPoint with X=wayPoint.X+x})
        | Move (W x) -> (pos, {wayPoint with X=wayPoint.X-x})
        | Turn (L a) -> (pos, turnCounterClockWise wayPoint a)
        | Turn (R a) -> (pos, turnClockWise wayPoint a)
        | Move (F x) -> ({X=pos.X+wayPoint.X*x;Y=pos.Y+wayPoint.Y*x}, wayPoint)

    let computeManhattanDistance2 (list: Command list) =
        let (res, _) = list |> List.fold nextStep2 ({X=0;Y=0}, {X=10;Y=1})
        (abs res.X) + (abs res.Y)
