module AoC_2024.Day12

open System
open System.Collections.Generic

module Helper =
    type Direction =
        | Horizontal
        | Vertical

    type FacingFromBlock =
        | Left
        | Right

    type FacingFromBlockVert = 
        | Above
        | Below

    type BlockFacing = 
        | Vert of FacingFromBlockVert
        | Horiz of FacingFromBlock

    type Edge = {
        Direction : Direction
        Before : int
        After : int
        Facing : BlockFacing
    }
    let inBounds (x, y) x_limit y_limit =
            x >= 0 && x <= x_limit && y >= 0 && y <= y_limit
    let getCardinalDirections (x, y) x_limit y_limit = 
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


    let floodFill (x, y) letter (map: string array) 
        explored_coords  =

            let rec dfs (x, y) (cur_set: Collections.Generic.HashSet<(int * int)>) 
                (already_seen: Collections.Generic.HashSet<(int * int)>) (letter) : unit =
                
                cur_set.Add (x, y) |> ignore
                already_seen.Add (x, y) |> ignore

                let cardinal_directions = 
                    getCardinalDirections (x, y) (map[0].Length - 1) (map.Length - 1)

                for (x', y') in cardinal_directions do
                    if (map[y'][x'] = letter) && (not (already_seen.Contains (x', y'))) then
                        dfs (x', y') (cur_set) (already_seen) (letter)

            let res = Collections.Generic.HashSet<(int * int)> ()
            dfs (x, y) (res) (explored_coords) (letter)

            res
    
    let getEdgeList region letter (map: string array) =
        let x_limit = map[0].Length - 1
        let y_limit = map.Length - 1
        let distinct_edge = List<Edge * int> ()

        for (x, y) in region do
            if not (inBounds (x - 1, y) x_limit y_limit) || (map[y][x - 1] <> letter) then
                distinct_edge.Add ({ Direction = Vertical; Before = x - 1; After = x; Facing = Horiz Left}, y)

            if not (inBounds (x + 1, y) x_limit y_limit) || (map[y][x + 1] <> letter) then
                distinct_edge.Add ({ Direction = Vertical; Before = x; After = x + 1; Facing = Horiz Right}, y)

            if not (inBounds (x, y - 1) x_limit y_limit) || (map[y - 1][x] <> letter) then
                distinct_edge.Add ({ Direction = Horizontal; Before = y - 1; After = y; Facing = Vert Above}, x)

            if not (inBounds (x, y + 1) x_limit y_limit) || (map[y + 1][x] <> letter) then
                distinct_edge.Add ({ Direction = Horizontal; Before = y; After = y + 1; Facing = Vert Below}, x)

        distinct_edge

    let getSideCount (edgeList: List<(Edge * int)>) =
        edgeList
        |> Seq.groupBy (fun (edge, _) -> edge)
        |> Seq.map ( fun (_, edge_seq) ->
                edge_seq |> Seq.map ( fun (_, num) -> num))
        |> Seq.map (fun num_seq ->
                        let sorted_list =
                            num_seq
                            |> Seq.sort
                            |> Seq.toArray

                        let mutable sides = 1

                        for index in 0..(sorted_list.Length - 2) do
                            if sorted_list[index + 1] <> (sorted_list[index] + 1) then
                                sides <- sides + 1
                        sides )
        |> Seq.sum


let solve (input: string array) : (string * string) = 
    let been_explored = Collections.Generic.HashSet<(int * int)> ()

    let list_of_regions =
        input
        |> Array.indexed
        |> Array.collect (fun (y, str) ->
                            [|
                                for x in 0..(str.Length - 1) do
                                    (str[x], (x, y))
                            |])
        |> Array.map (fun (letter, (x, y)) ->
                if not (been_explored.Contains (x, y)) then
                    Some (letter, Helper.floodFill (x, y) (letter) (input) (been_explored))
                else None )
        |> Array.filter (function
                            | None -> false
                            | _ -> true)
        |> Array.map (fun opt -> Option.get opt)

    let edge_lists = 
        list_of_regions
        |> Array.map (fun (letter, region) ->
                        let edge_list = Helper.getEdgeList region letter input
                        (region.Count, edge_list))

    let part_1 =
        edge_lists
        |> Array.sumBy (fun (region_size, edge_list) ->
                            (region_size) * (edge_list.Count))

    let part_2 = 
        edge_lists
        |> Array.sumBy (fun (region_size, edge_list) ->
                            (region_size) * (Helper.getSideCount edge_list))
        
        
    ($"{part_1}", $"{part_2}")