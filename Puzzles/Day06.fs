module AoC_2024.Day06

open System
module Helper = 
    type Direction =
    | North
    | East
    | South
    | West

    let getIndexOfGuard (map: string array) : (int * int) = 
        let y = Array.FindIndex (map, fun x-> x.Contains "^")
        let x = map[y].IndexOf '^'

        (x, y)

    let causesALoop (map: string array) (item_x: int, item_y: int) (guardIndex: int * int) : bool =
        let x_limit = map[0].Length - 1
        let y_limit = map.Length - 1

        let getSteps (index: int * int) (direction: Direction) : ((int * int) array * bool) =
            let (x, y) = index
            let (start, skip, stop) = match direction with
                                                        | North -> (y, -1, 0)
                                                        | South -> (y, 1, y_limit)
                                                        | East -> (x, 1, x_limit)
                                                        | West -> (x, -1, 0)

            let arr1 = 
                [|
                    for i in start..skip..stop do
                       yield match direction with
                             | North
                             | South -> (x, i)
                             | East
                             | West -> (i, y)
                |]

            let arr2 = Array.takeWhile (fun (x, y) -> 
                                                    (map[y][x] <> '#') && ((x, y) <> (item_x, item_y))) arr1

            let isOnMap = arr1.Length <> arr2.Length
        
            (arr2, isOnMap)

        let unique_passed_index = Collections.Generic.HashSet<(int * int)> ()

        let mutable curIndex = guardIndex

        let mutable curDirection = North 
        let mutable isOnMap = true
        let mutable inALoop = false
        let mutable setHasNotChanged = 0

        let mutable loopCount = 0
        while isOnMap && (not inALoop) do
            let (listOfSteps, isStillOnMap) = getSteps curIndex curDirection 

            let prev_set_count = unique_passed_index.Count

            for pair in listOfSteps do
                unique_passed_index.Add (pair) |> ignore

            isOnMap <- isStillOnMap

            curIndex <- (Array.last listOfSteps)
            curDirection <- match curDirection with
                            | North -> East
                            | East -> South
                            | South -> West
                            | West -> North
            
            loopCount <- loopCount + 1

            setHasNotChanged <- if (unique_passed_index.Count = prev_set_count) then setHasNotChanged + 1 else setHasNotChanged

            if (loopCount % 4 = 0) then
                if setHasNotChanged = 4 then inALoop <- true
                else setHasNotChanged <- 0

        inALoop


let solve (input: string array) : (string * string) =

    let x_limit = input[0].Length - 1
    let y_limit = input.Length - 1

    let getSteps (index: int * int) (direction: Helper.Direction) : ((int * int) array * bool) =
        let (x, y) = index
        let (start, skip, stop) = match direction with
                                                    | Helper.North -> (y, -1, 0)
                                                    | Helper.South -> (y, 1, y_limit)
                                                    | Helper.East -> (x, 1, x_limit)
                                                    | Helper.West -> (x, -1, 0)

        let arr1 = 
            [|
                for i in start..skip..stop do
                    yield match direction with
                            | Helper.North
                            | Helper.South -> (x, i)
                            | Helper.East
                            | Helper.West -> (i, y)
            |]

        let arr2 = Array.takeWhile (fun (x, y) -> input[y][x] <> '#') arr1

        let isOnMap = arr1.Length <> arr2.Length
        
        (arr2, isOnMap)

    let guardIndex = Helper.getIndexOfGuard input

    let unique_passed_index = Collections.Generic.HashSet<(int * int)> ()

    let mutable curIndex = guardIndex

    let mutable curDirection : Helper.Direction = Helper.Direction.North 
    let mutable isOnMap = true

    while isOnMap do
        let (listOfSteps, isStillOnMap) = getSteps curIndex curDirection 

        for pair in listOfSteps do
            unique_passed_index.Add (pair) |> ignore

        isOnMap <- isStillOnMap

        curIndex <- (Array.last listOfSteps)
        curDirection <- match curDirection with
                        | Helper.North -> Helper.East
                        | Helper.East -> Helper.South
                        | Helper.South -> Helper.West
                        | Helper.West -> Helper.North

    let mutable part2 = 0

    for y in 0..y_limit do
        for x in 0..x_limit do
            if (input[y][x] = '.') && (unique_passed_index.Contains (x, y)) then
                if Helper.causesALoop input (x, y) (guardIndex) then
                    part2 <- part2 + 1


    ($"{unique_passed_index.Count}", $"{part2}")