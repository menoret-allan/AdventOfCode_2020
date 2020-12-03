namespace Core

module World =
    type Case = Tree | Free
    type Line = Case List
    type WorldMap = Line List
    type Moves = (int * int)
    type Pos = (int * int)

    let calculateNextPos (movX, moxY) (posX, posY) =
        (movX+posX, moxY+posY)

    let mapAccess (map: WorldMap) (x, y) =
        if y >= List.length map then None
        else
            let line = map |> List.item y
            line |> List.item (x % (List.length line)) |> Some
    
    let howManyTrees moves map =
        let calcPos = calculateNextPos moves
        let rec compute(pos: Pos) =
            match mapAccess map pos with 
            | None -> 0
            | Some Free -> 0 + compute (calcPos pos)
            | Some Tree -> 1 + compute (calcPos pos)
        compute (0,0)