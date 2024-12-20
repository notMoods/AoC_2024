module AoC_2024.Day20

open System
open System.Collections.Generic

module Helper = 
    [<Struct>]
    type State = {
        Pos_X : int
        Pos_Y : int
        Cheated : bool
    }

    let getPoint (point: char) (map: string array) =
        let (x, y) = 
            map
            |> Array.fold (
                fun (x, y, y_count) str -> 
                    let index = str.IndexOf point
                    if index <> -1 then
                        (index, y_count, y_count + 1)
                    else (x, y, y_count + 1)
            ) (0, 0, 0)
            |> (fun (x, y, _) -> (x, y))

        {
            Pos_X = x
            Pos_Y = y
            Cheated = false
        }

    type Direction = 
        | Up
        | Down
        | Left
        | Right
    let getAllValidMovementsTwo (state) (map: char array array) = 
        let outOfBounds (x, y) = 
            x <= 0 || y <= 0 || x >= map[0].Length - 1 || y >= map.Length - 1

        let getDirectionsBut (x, y) dir =
            [|
                if dir <> Up then yield (x, y + 1)
                if dir <> Down then yield (x, y - 1)

                if dir <> Left then yield (x + 1, y)
                if dir <> Right then yield (x - 1, y)
            |]

        let possibleCheatMovements (x, y) new_dir =
                [|
                    for (x', y') in getDirectionsBut (x, y) new_dir do
                        if map[y'][x'] <> '#' then
                            yield { Pos_X = x'; Pos_Y = y'; Cheated = true }
                |]

        let (x, y) = (state.Pos_X, state.Pos_Y)

        let directions = 
            [| (x + 1, y, Right); (x - 1, y, Left); (x, y + 1, Down); (x, y - 1, Up) |]

        [|
            for (dX, dY, dir) in directions do
                if (not (outOfBounds (dX, dY))) then
                    if map[dY][dX] <> '#' then
                        yield { Pos_X = dX; Pos_Y = dY; Cheated = state.Cheated }
                    if state.Cheated = false && map[dY][dX] = '#' then
                        yield! possibleCheatMovements (dX, dY) dir
        |]

    let getCardinal (x, y) (map: char array array) =
        let outOfBounds (x, y) = 
            x <= 0 || y <= 0 || x >= map[0].Length - 1 || y >= map.Length - 1

        let directions = 
            [| (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) |]

        [|
            for (dX, dY) in directions do
                if (not (outOfBounds (dX, dY))) && map[dY][dX] <> '#' then
                    yield (dX, dY)
        |]

    let getPositionsAfterCheating (x, y) (map: char array array) =
        let outOfBounds (x, y) = 
            x <= 0 || y <= 0 || x >= map[0].Length - 1 || y >= map.Length - 1

        let getDirectionsBut (x, y) dir =
            [|
                if dir <> Up then yield (x, y + 1)
                if dir <> Down then yield (x, y - 1)

                if dir <> Left then yield (x + 1, y)
                if dir <> Right then yield (x - 1, y)
            |]

        let possibleCheatMovements (x, y) new_dir =
                [|
                    for (x', y') in getDirectionsBut (x, y) new_dir do
                        if (not (outOfBounds (x', y'))) && map[y'][x'] <> '#' then
                            yield (x', y')
                |]

        let hashes = 
            [|
                for (x', y', dir) in [|(x - 1, y, Left); (x + 1, y, Right); (x, y - 1, Up); (x, y + 1, Down)|] do
                    if (not (outOfBounds (x, y))) && map[y'][x'] = '#' then yield (x', y', dir)
            |]

        [|
            for (x', y', dir) in hashes do
                yield! possibleCheatMovements (x', y') dir
        |]

    let getMinCosts (goal_x, goal_y) (start_x, start_y) (map: char array array) =
        let dict = Dictionary<(int * int), int>()

        let f_queue = Queue<(int * int) * int>()
        f_queue.Enqueue ((goal_x, goal_y), 0)

        let f_visited_nodes = HashSet<int * int>()

        while f_queue.Count > 0 do
            let ((cur_x, cur_y), cur_cost) = f_queue.Dequeue ()

            dict.Add ((cur_x, cur_y), cur_cost)

            f_visited_nodes.Add (cur_x, cur_y) |> ignore

            if not ((cur_x = start_x) && (cur_y = start_y)) then
                for (new_x, new_y) in getCardinal (cur_x, cur_y) map do
                    if not (f_visited_nodes.Contains(new_x, new_y)) then
                        f_queue.Enqueue ((new_x, new_y), cur_cost + 1)

        dict

    let solveOne start goal (map: char array array) 
        (min_costs: Dictionary<(int * int), int>) = 
        let normal_goal = min_costs[(start.Pos_X, start.Pos_Y)]

        let mutable res = 0
        let queue = Queue<(int * int) * int>()
        queue.Enqueue ((start.Pos_X, start.Pos_Y), 0)

        let visited_nodes = HashSet<int * int>()
        
        while queue.Count > 0 do 
            let ((cur_x, cur_y), cur_cost) = queue.Dequeue ()

            visited_nodes.Add (cur_x, cur_y) |> ignore

            let allPossiblePosAfterCheat = getPositionsAfterCheating (cur_x, cur_y) map

            for (pos_x, pos_y ) in allPossiblePosAfterCheat do
                let (exists, value) = min_costs.TryGetValue ((pos_x, pos_y))

                if exists then
                    let the_cost = cur_cost + 2 + value
                    if the_cost + 100 <= normal_goal then res <- res + 1

            for (new_x, new_y) in getCardinal (cur_x, cur_y) map do
                if not (visited_nodes.Contains(new_x, new_y)) then
                    queue.Enqueue  ((new_x, new_y), cur_cost + 1)  

        res


    let solveMODIFIED start (min_costs_from_goal: Dictionary<(int *int), int>) =
        let getDistance (x1: int, y1: int) (x2, y2) = 
            (Math.Abs (x1 - x2)) + (Math.Abs (y1 - y2))

        let normal_goal = min_costs_from_goal[(start.Pos_X, start.Pos_Y)]

        let mutable res = 0

        let list_nodes = 
            min_costs_from_goal
            |> Seq.sortByDescending (
                fun kvp ->
                    kvp.Value
            )
            |> Seq.toArray

        let mutable loopcount = 0
        for i in 0..(list_nodes.Length - 1) do
            for j in (i + 1)..(list_nodes.Length - 1) do
                let first_node = list_nodes[i]
                let second_node = list_nodes[j]

                let dist_between = getDistance first_node.Key second_node.Key

                if dist_between <= 20 then
                    let path_cost = normal_goal - first_node.Value
                    let goal_cost = dist_between + second_node.Value

                    if (path_cost + goal_cost + 100) <= normal_goal then
                        res <- res + 1
        res

let solve (input: string array) : (string * string) = 
    let start_node = Helper.getPoint 'S' input 
    let goal_node = Helper.getPoint 'E' input

    let map =
        input |> 
        Array.map (fun str -> str.ToCharArray())

    let min_costs = 
        Helper.getMinCosts (goal_node.Pos_X, goal_node.Pos_Y) (start_node.Pos_X, start_node.Pos_Y) map

    let part_one =  Helper.solveOne start_node goal_node map min_costs

    let part_two =  Helper.solveMODIFIED start_node min_costs

    ($"{part_one}", $"{part_two}")