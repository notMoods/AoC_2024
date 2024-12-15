module AoC_2024.Day15

open System

module Helper = 
    [<Struct>]
    type Box = {
        Pos_Y : int
        Pos_Left_X : int
        Pos_Right_X : int
    }
    type Direction = 
        | Left
        | Right
        | Up
        | Down

    let handleMovement (pos_x, pos_y) direction (map: char array array) = 
        let (candidate_x, candidate_y) = 
            match direction with
            | Up -> (pos_x, pos_y - 1)
            | Down -> (pos_x, pos_y + 1)
            | Left -> (pos_x - 1, pos_y)
            | Right -> (pos_x + 1, pos_y)

        let mutable moved = false

        if map[candidate_y][candidate_x] = '.' then
            moved <- true
            map[candidate_y][candidate_x] <- '@'
            map[pos_y][pos_x] <- '.'
        
        elif map[candidate_y][candidate_x] = 'O' then
            let (dx, dy) = 
                match direction with
                | Up -> (0, -1)
                | Down -> (0, 1)
                | Left -> (-1, 0)
                | Right -> (1, 0)

            let mutable (check_x, check_y) = 
                (candidate_x + dx, candidate_y + dy)

            while map[check_y][check_x] = 'O' do
                check_x <- check_x + dx
                check_y <- check_y + dy

            if map[check_y][check_x] = '.' then
                moved <- true
                map[candidate_y][candidate_x] <- '@'
                map[pos_y][pos_x] <- '.'
                map[check_y][check_x] <- 'O'

        if moved then (candidate_x, candidate_y) else (pos_x, pos_y)

    let moveRobotAroundMap (x, y) (sequence: char seq) (map: char array array) : char array array =
        let mutable (robot_x, robot_y) = (x, y)

        for move in sequence do
            if move = '^' then
                let (res_x, res_y) =
                    handleMovement (robot_x, robot_y) Up map

                robot_x <- res_x
                robot_y <- res_y
                
            elif move = 'v' then
                let (res_x, res_y) = 
                    handleMovement (robot_x, robot_y) Down map

                robot_x <- res_x
                robot_y <- res_y

            elif move = '<' then
                let (res_x, res_y) =  
                    handleMovement (robot_x, robot_y) Left map
                
                robot_x <- res_x
                robot_y <- res_y

            elif move = '>' then
                let (res_x, res_y) =  
                    handleMovement (robot_x, robot_y) Right map

                robot_x <- res_x
                robot_y <- res_y

            else printfn "You should not be seeing this"
        map

    let handleWeirdMovement box dy (map: char array array)  : bool =

        let rec hWM (boxes: Box seq) (cur_y: int) : bool =
            let isFree box = 
                map[box.Pos_Y + dy][box.Pos_Left_X] = '.' && map[box.Pos_Y + dy][box.Pos_Right_X] = '.'
            
            let hasWall box = 
                map[box.Pos_Y + dy][box.Pos_Left_X] = '#' || map[box.Pos_Y + dy][box.Pos_Right_X] = '#'

            let mutable all_free = true
            let mutable cant_be_moved = false

            for box in boxes do
                if not (isFree box) then
                    all_free <- false
                    if hasWall box then
                        cant_be_moved <- true
            if all_free then
                for box in boxes do
                    map[box.Pos_Y + dy][box.Pos_Left_X] <- '['
                    map[box.Pos_Y + dy][box.Pos_Right_X] <- ']'

                    map[box.Pos_Y][box.Pos_Left_X] <- '.'
                    map[box.Pos_Y][box.Pos_Right_X] <- '.'
                true
            elif cant_be_moved then
                false
            else
                let new_box_list = Collections.Generic.HashSet<Box> ()

                let indexes_to_check = seq {
                    for box in boxes do
                        yield box.Pos_Left_X
                        yield box.Pos_Right_X
                }
                for x in indexes_to_check do
                    if map[cur_y + dy][x] = '[' then
                        let box = { Pos_Y = cur_y + dy; Pos_Left_X = x; Pos_Right_X = x + 1 }
                        new_box_list.Add box |> ignore
                    elif map[cur_y + dy][x] = ']' then
                        let box = { Pos_Y = cur_y + dy; Pos_Left_X = x - 1; Pos_Right_X = x }
                        new_box_list.Add box |> ignore

                let canSwap = hWM new_box_list (cur_y + dy)

                if canSwap then
                    for box in boxes do
                        map[box.Pos_Y + dy][box.Pos_Left_X] <- '['
                        map[box.Pos_Y + dy][box.Pos_Right_X] <- ']'

                        map[box.Pos_Y][box.Pos_Left_X] <- '.'
                        map[box.Pos_Y][box.Pos_Right_X] <- '.'
                    true
                else false

        hWM [box] box.Pos_Y

    let handleMovementNew (pos_x, pos_y) direction (map: char array array) = 
        let isABox tile = 
            (tile = '[') || (tile = ']')

        let (candidate_x, candidate_y) = 
            match direction with
            | Up -> (pos_x, pos_y - 1)
            | Down -> (pos_x, pos_y + 1)
            | Left -> (pos_x - 1, pos_y)
            | Right -> (pos_x + 1, pos_y)

        let mutable moved = false

        if map[candidate_y][candidate_x] = '.' then
            moved <- true
            map[candidate_y][candidate_x] <- '@'
            map[pos_y][pos_x] <- '.'
        
        elif isABox (map[candidate_y][candidate_x])  then
            let (dx, dy) = 
                match direction with
                | Up -> (0, -1)
                | Down -> (0, 1)
                | Left -> (-1, 0)
                | Right -> (1, 0)

            if direction = Left || direction = Right then

                let mutable check_x = 
                    candidate_x + dx

                while isABox (map[candidate_y][check_x])  do
                    check_x <- check_x + dx

                if map[candidate_y][check_x] = '.' then
                    let mutable l_b = 
                        if direction = Right then true else false

                    for x in (candidate_x + dx)..dx..check_x do
                        map[candidate_y][x] <-
                            if l_b then '[' else ']'
                        
                        l_b <- l_b <> true

                    moved <- true
                    map[candidate_y][candidate_x] <- '@'
                    map[pos_y][pos_x] <- '.'
            else
                let (l_side_x, r_side_x) = 
                    if map[candidate_y][candidate_x] = '[' then
                        (candidate_x, candidate_x + 1)
                    else (candidate_x - 1, candidate_x)

                let box = { Pos_Y = candidate_y; Pos_Left_X = l_side_x; Pos_Right_X = r_side_x }

                let should_move = handleWeirdMovement box dy map 

                if should_move then
                    moved <- true

                    map[candidate_y][candidate_x] <- '@'
                    map[pos_y][pos_x] <- '.'

        if moved then (candidate_x, candidate_y) else (pos_x, pos_y)

    let moveRobotAroundMapTwo (x, y) (sequence: char seq) (map: char array array) : char array array =
        let mutable (robot_x, robot_y) = (x, y)

        for move in sequence do
            if move = '^' then
                let (res_x, res_y) =
                    handleMovementNew (robot_x, robot_y) Up map

                robot_x <- res_x
                robot_y <- res_y
    
            elif move = 'v' then
                let (res_x, res_y) = 
                    handleMovementNew (robot_x, robot_y) Down map

                robot_x <- res_x
                robot_y <- res_y

            elif move = '<' then
                let (res_x, res_y) =  
                    handleMovementNew (robot_x, robot_y) Left map
                
                robot_x <- res_x
                robot_y <- res_y

            elif move = '>' then
                let (res_x, res_y) =  
                    handleMovementNew (robot_x, robot_y) Right map

                robot_x <- res_x
                robot_y <- res_y

            else printfn "You should not be seeing this"

        map

    let GPSScore tile (map: char array array) : Int64 = 
        map
        |> Array.indexed
        |> Array.collect (
            fun (y, arr) ->
                [|
                    for i in 0 .. (arr.Length - 1) do
                        if arr[i] = tile then
                            yield (i, y)
                |] )
        |> Array.sumBy ( fun (x, y) ->
                (100L * (int64 y)) + (int64 x) )

let solve (input: string array) : (string * string) = 
    let getRobot map = 
        map
        |> Array.fold (
            fun (x, y, y_count) str -> 
                let index = Array.IndexOf (str, '@')
                if index <> -1 then
                    (index, y_count, y_count + 1)
                else (x, y, y_count + 1)
        ) (0, 0, 0)
        |> (fun (x, y, _) -> (x, y))
        
    let index_of_space = 
        input
        |> Array.findIndexBack (fun str -> str = String.Empty)

    let map = 
        input[..(index_of_space - 1)]
        |> Array.map (fun str -> str.ToCharArray ())

    let map_2 =
        map
        |> Array.map (
            fun arr ->
                arr |> Array.collect ( 
                    function
                        | '#' -> [|'#'; '#'|]
                        | 'O' -> [|'['; ']'|]
                        | '.' -> [|'.'; '.'|]
                        | '@' -> [|'@'; '.'|]
                        | _ -> Array.empty ) )

    let robot = map |> getRobot
   
    let robot_2 = map_2 |> getRobot

    let robot_sequence = 
        input[(index_of_space + 1)..]
        |> Seq.collect (fun str -> str)

    let part_one = 
        map
        |> Helper.moveRobotAroundMap robot robot_sequence
        |> Helper.GPSScore 'O'

    let part_two = 
        map_2
        |> Helper.moveRobotAroundMapTwo robot_2 robot_sequence
        |> Helper.GPSScore '['

    ($"{part_one}", $"{part_two}")