module AoC_2024.Day18

open System
open System.Collections.Generic

module Helper = 
    let getAllValidMovements (x, y) (map: char array array) = 
        let outOfBounds (x, y) = 
            x < 0 || y < 0 || x >= map[0].Length || y >= map.Length

        let directions = 
            [| (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) |]

        [|
            for (dX, dY) in directions do
                if (not (outOfBounds (dX, dY))) && map[dY][dX] <> '#' then
                    yield (dX, dY)
        |]
    let traverseMap (map: char array array) (start_x, start_y) 
        (goal_x,goal_y)  = 

        let min_costs = Dictionary<int * int, int> ()

        min_costs.Add ((start_x, start_y), 0)

        let queue = PriorityQueue<(int * int), int> ()
        queue.Enqueue ((start_x, start_y), min_costs[(start_x, start_y)])

        let mutable res = Int32.MaxValue

        let visited_nodes = HashSet<int * int> ()

        let mutable stop = false
        while queue.Count > 0 do

            let cur_x, cur_y = queue.Dequeue ()
            visited_nodes.Add (cur_x, cur_y) |> ignore

            for (new_x, new_y) in (getAllValidMovements (cur_x,cur_y) map) do
                if not (visited_nodes.Contains (new_x, new_y)) then

                    let new_dist = min_costs[(cur_x, cur_y)] + 1

                    let (exists, prev_dist) = min_costs.TryGetValue ((new_x, new_y))

                    if (new_x = goal_x) && (new_y = goal_y) then
                        res <- Math.Min (res, new_dist)

                    else if exists && new_dist < prev_dist then
                        min_costs[(new_x, new_y)] <- new_dist

                        queue.Remove ((new_x, new_y)) |> ignore

                        queue.Enqueue ((new_x, new_y), new_dist)
                    elif not exists then
                        min_costs.Add ((new_x, new_y), new_dist)
                        queue.Enqueue ((new_x, new_y), new_dist)

        res

    let floodFill (map: char array array) (node_x, node_y) =

        let rec dfs (x, y) (cur_set: HashSet<int * int>) =
            cur_set.Add (x, y) |> ignore

            let allMoves = getAllValidMovements (x, y) map

            for (x', y') in allMoves do
                if not (cur_set.Contains(x', y')) then
                    dfs (x', y') cur_set

        let region = HashSet<int * int> ()
        dfs (node_x, node_y) (region) 
        region

    let floodFillCheck (map: char array array) (bytes: (int * int) array) =
        
        let mutable (res_x, res_y) = (0, 0)

        let mutable continue_loop = true

        let mutable index = 0

        while continue_loop do
            let (x, y) = bytes[index]

            map[y][x] <- '#'

            let region = floodFill map (0, 0)

            if not (region.Contains (70, 70)) then
                continue_loop <- false
                res_x <- x
                res_y <- y

            index <- index + 1

        (res_x, res_y)

let solve (input: string array) : (string * string) = 
    let map = 
        [|
            for i in 0..70 do
                [|
                    for j in 0..70 do '.'
                |]
        |]

    input[..1023]
    |> Array.iter (
        fun str ->
            //8,66
            let foo = str.Split (',', StringSplitOptions.RemoveEmptyEntries)
            let x = Int32.Parse foo[0]
            let y = Int32.Parse foo[1]
            map[y][x] <- '#'

    )

    let part_one = Helper.traverseMap map (0, 0) (70, 70)

    let more_bytes = 
        input[1024..]
        |> Array.map (
            fun str ->
                let arr = str.Split (',', StringSplitOptions.RemoveEmptyEntries)

                (Int32.Parse arr[0], Int32.Parse arr[1])
        )

    let (p2_x, p2_y) = Helper.floodFillCheck map more_bytes
    
    
    ($"{part_one}", $"{p2_x},{p2_y}")