module AoC_2024.Day14

open System
open System.Collections.Generic

module Helper = 
    type Robot = {
        P_X : int
        P_Y : int

        V_X : int
        V_Y : int
    }

    let makeRobot (str: string) : Robot = 
        let nums = 
            str.Split ([|','; ' '; 'p'; '='; 'v'|], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun x -> Int32.Parse x)

        { P_X = nums[0]; P_Y = nums[1]; V_X = nums[2]; V_Y = nums[3] }

    let afterXSeconds secs (quadrants: int64 array) robot : unit =
        let (new_pos_unbounded_x, new_pos_unbounded_y) = 
            (robot.P_X + (robot.V_X * secs), robot.P_Y + (robot.V_Y * secs))

        let x = 
            let x' = new_pos_unbounded_x % 101
            if x' < 0 then 101 + x'
            else x'

        let y = 
            let y' = new_pos_unbounded_y % 103
            if y' < 0 then 103 + y'
            else y'

        if x < 50 && y < 51 then
            quadrants[0] <- quadrants[0] + 1L
        elif x < 50 && y > 51 then
            quadrants[2] <- quadrants[2] + 1L
        elif x > 50 && y < 51 then
            quadrants[1] <- quadrants[1] + 1L
        elif x > 50 && y > 51 then
            quadrants[3] <- quadrants[3] + 1L

    let mutateRobot robot : Robot =
        let (new_pos_unbounded_x, new_pos_unbounded_y) = 
            (robot.P_X + robot.V_X , robot.P_Y + robot.V_Y )

        let x = 
            let x' = new_pos_unbounded_x % 101
            if x' < 0 then 101 + x'
            else x'

        let y = 
            let y' = new_pos_unbounded_y % 103
            if y' < 0 then 103 + y'
            else y'

        {
            robot with
                P_X = x
                P_Y = y
        }

    let getCardinalDirections (x, y) x_limit y_limit = 
        let inBounds (x, y) x_limit y_limit =
            x >= 0 && x <= x_limit && y >= 0 && y <= y_limit

        let (coord_1, coord_2, coord_3, coord_4) = (
            (x - 1, y),
            (x + 1, y),
            (x, y - 1),
            (x, y + 1)
        )

        [|
            if inBounds coord_1 x_limit y_limit then yield coord_1
            if inBounds coord_2 x_limit y_limit then yield coord_2
            if inBounds coord_3 x_limit y_limit then yield coord_3
            if inBounds coord_4 x_limit y_limit then yield coord_4
        |]

    let floodFill (x: int, y: int) (robot_set: HashSet<(int * int)>) (been_explored: HashSet<(int * int)>) = 

        let rec dfs (x, y) (cur_set: HashSet<(int * int)>) 
            (already_seen: HashSet<(int * int)>) = 

            cur_set.Add (x, y) |> ignore
            already_seen.Add (x, y) |> ignore

            let cardinal_directions = 
                    getCardinalDirections (x, y) (100) (102)

            for (x', y') in cardinal_directions do
                if robot_set.Contains (x', y') && (not (already_seen.Contains (x', y'))) then
                    dfs (x', y') cur_set already_seen

        let res = HashSet<(int * int)> ()

        dfs (x, y) (res) (been_explored)

        res

    let lookForTree (robots : Robot array) =
        let coords_set = HashSet<(int * int)> ()

        let mutable should_break = true
        let mutable seconds = 0

        while should_break do
            coords_set.Clear ()
            for i in 0.. (robots.Length - 1) do
                robots[i] <- mutateRobot (robots[i])
                coords_set.Add ((robots[i]).P_X, (robots[i]).P_Y) |> ignore

            let been_explored = HashSet<(int * int)> ()

            let regions = 
                coords_set
                |> Seq.map (fun (x, y) ->
                                if not (been_explored.Contains (x, y)) then
                                    Some (floodFill (x, y) coords_set been_explored)
                                else None )
                |> Seq.filter (function
                                | Some _ -> true
                                | None -> false)
                |> Seq.map (fun opt -> Option.get opt)

            should_break <-
                not (regions
                    |> Seq.exists (fun region -> region.Count > 25))

            seconds <- seconds + 1

        for y in 0..102 do
            for x in 0..100 do
                let c = if coords_set.Contains (x, y) then '#' else '.'
                printf "%A" c
            printf "\n"

        seconds
           

let solve (input: string array) : (string * string) = 
    let quadrants = [|0L; 0L; 0L; 0L|]

    let robots =
        input 
        |> Array.map (Helper.makeRobot)
        
    robots |> Array.iter (Helper.afterXSeconds 100 quadrants)

    let part_1 = quadrants[0] * quadrants[1] * quadrants[2] * quadrants[3]

    let part_2 = Helper.lookForTree robots

    ($"{part_1}", $"{part_2}")