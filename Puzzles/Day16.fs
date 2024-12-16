module AoC_2024.Day16

open System
open System.Collections.Generic

module Helper = 
    type Direction = 
        | North
        | South
        | East
        | West

    type Point = {
        Facing : Direction
        Pos_X : int
        Pos_Y : int
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
            Facing = East
            Pos_X = x
            Pos_Y = y
        }
    let getValidPoints point (map: string array) = 
        let forward_point = 
            match point.Facing with
            | North -> { point with Pos_Y = point.Pos_Y - 1 }
            | South -> { point with Pos_Y = point.Pos_Y + 1 }
            | East -> { point with Pos_X = point.Pos_X + 1 }
            | West -> { point with Pos_X = point.Pos_X - 1 }

        let (clockwise, counterclockwise) = 
            match point.Facing with
            | North -> ({ point with Facing = East; Pos_X = point.Pos_X + 1 }, 
                        { point with Facing = West; Pos_X = point.Pos_X - 1 })
            | South -> ({ point with Facing = West; Pos_X = point.Pos_X - 1 }, 
                        { point with Facing = East; Pos_X = point.Pos_X + 1})
            | East -> ({ point with Facing = South; Pos_Y = point.Pos_Y + 1}, 
                        { point with Facing = North; Pos_Y = point.Pos_Y - 1})
            | West -> ({ point with Facing = North; Pos_Y = point.Pos_Y - 1 }, 
                        { point with Facing = South; Pos_Y = point.Pos_Y + 1 })

        [|
            if map[forward_point.Pos_Y][forward_point.Pos_X] <> '#' then
                (forward_point, 1L)

            if map[clockwise.Pos_Y][clockwise.Pos_X] <> '#' then
                (clockwise, 1001L)

            if map[counterclockwise.Pos_Y][counterclockwise.Pos_X] <> '#' then
                (counterclockwise, 1001L)
        |]
    let dijkstra start_pos (goal_x, goal_y) (map: string array) 
        (state_costs: Dictionary<Point, Int64>) = 
        let isGoal node =
            node.Pos_X = goal_x && node.Pos_Y = goal_y

        let p_queue = PriorityQueue<Point, Int64> ()

        let visited_nodes = HashSet<Point> ()

        p_queue.Enqueue (start_pos, state_costs[start_pos])

        let mutable res = Int64.MaxValue

        while (p_queue.Count > 0) do
            let cur_node = p_queue.Dequeue ()
            visited_nodes.Add (cur_node) |> ignore
    
            for (node, cost) in (getValidPoints cur_node map) do

                if not (visited_nodes.Contains node) then
                    let considered_new_dist = state_costs[cur_node] + cost

                    let (exists, prev_dist) = state_costs.TryGetValue node

                    if isGoal node then
                        res <- Math.Min (res, considered_new_dist)

                    elif exists && considered_new_dist < prev_dist then

                        state_costs[node] <- considered_new_dist

                        p_queue.Remove (node) |> ignore

                        p_queue.Enqueue (node, considered_new_dist)

                    elif not exists then 
                        state_costs.Add (node, considered_new_dist)
                        p_queue.Enqueue (node, considered_new_dist)

        res

    let dijkstraTwo start_pos (goal_x, goal_y) (map: string array) 
        (state_costs: Dictionary<Point, Int64>) =
        let isGoal node =
            node.Pos_X = goal_x && node.Pos_Y = goal_y

        let p_queue = PriorityQueue<(Point * Point list), Int64> ()

        p_queue.Enqueue ((start_pos, [ start_pos ]), 0)

        let paths_for_each_node = Dictionary<Point, (Point list) list>()

        paths_for_each_node.Add (start_pos, [ [ start_pos ] ])

        let mutable the_goal = Int64.MaxValue

        let unique_nodes = HashSet<int * int>()

        while p_queue.Count > 0 do
            let (cur_node, cur_path) = p_queue.Dequeue ()
        
            for (node, cost) in (getValidPoints cur_node map) do
                    let new_cost = state_costs[cur_node] + cost
                    let list_of_path = cur_path @ [node]

                    let (exists, prev_dist) = state_costs.TryGetValue node

                    if isGoal node then
                        if new_cost = the_goal then
                            for point in list_of_path do
                                unique_nodes.Add (point.Pos_X, point.Pos_Y) |> ignore

                        elif new_cost < the_goal then
                            unique_nodes.Clear ()

                            the_goal <- new_cost
                            for point in list_of_path do
                                unique_nodes.Add (point.Pos_X, point.Pos_Y) |> ignore

                    elif not exists then
                        state_costs.Add (node, new_cost)

                        paths_for_each_node.Add (node, [ list_of_path ])

                        p_queue.Enqueue ((node, list_of_path), new_cost)

                    elif exists && new_cost < prev_dist then
                        state_costs[node] <- new_cost

                        paths_for_each_node.Remove (node) |> ignore

                        paths_for_each_node.Add (node, [ list_of_path ])

                        p_queue.Enqueue ((node, list_of_path), new_cost)
                        
                
                    elif exists && new_cost = prev_dist then
                        paths_for_each_node[node] <- paths_for_each_node[node] @ [ list_of_path ]
                        p_queue.Enqueue ((node, list_of_path), new_cost)
                        
        unique_nodes.Count
let solve (input: string array) : (string * string) =

    let reindeer_pos = Helper.getPoint 'S' input

    let goal = Helper.getPoint 'E' input

    let state_cost = Dictionary<Helper.Point, Int64> ()
    state_cost.Add (reindeer_pos, 0)

    let part_1 = Helper.dijkstra reindeer_pos (goal.Pos_X, goal.Pos_Y) input state_cost

    state_cost.Clear()
    state_cost.Add (reindeer_pos, 0)
    let part_2 = Helper.dijkstraTwo reindeer_pos (goal.Pos_X, goal.Pos_Y) input state_cost

    ($"{part_1}", $"{part_2}")