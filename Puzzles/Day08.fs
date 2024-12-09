module AoC_2024.Day08

open System

module Helper = 
    let getAntinode (coords : (int * int) list) (x_limit: int) (y_limit: int) : (int * int) seq =
        let isValidAntinode (x, y) : bool = 
            not (x < 0 || x > x_limit || y < 0 || y > y_limit) 
        
        let getAntinodes (first_x : int, first_y : int) (second_x : int, second_y : int) = 
            let x_diff = Math.Abs (first_x - second_x)
            let y_diff = Math.Abs (first_y - second_y)

            let ((smaller_x_cord_x, smaller_x_cord_y), (larger_x_cord_x, larger_x_cord_y)) = 
                if first_x > second_x then 
                    ((second_x, second_y), (first_x, first_y)) 
                else ((first_x, first_y), (second_x, second_y))

            let first_antinode = (smaller_x_cord_x - x_diff, 
                                                        if smaller_x_cord_y > larger_x_cord_y then
                                                            smaller_x_cord_y + y_diff
                                                        else smaller_x_cord_y - y_diff)
            let second_antinode = (larger_x_cord_x + x_diff,
                                                    if larger_x_cord_y > smaller_x_cord_y then
                                                        larger_x_cord_y + y_diff
                                                    else larger_x_cord_y - y_diff)

            (first_antinode, second_antinode)
        let list_length = coords.Length
        let res_seq = seq {

            for i in 0..(list_length - 1) do
                for j in (i + 1)..(list_length - 1) do
                    let (antinode1, antinode2) = getAntinodes coords[i] coords[j]
                    if isValidAntinode antinode1 then
                        yield antinode1

                    if isValidAntinode antinode2 then
                        yield antinode2
        }

        res_seq

    let getAntinodeFixed (coords : (int * int) list) (x_limit: int) (y_limit: int) : (int * int) seq =

        let getAntinodeLine (first_x : int, first_y : int) (second_x : int, second_y : int) : (int * int) seq= 
            let x_diff = Math.Abs (first_x - second_x)
            let y_diff = Math.Abs (first_y - second_y)

            let ((smaller_x_cord_x, smaller_x_cord_y), (larger_x_cord_x, larger_x_cord_y)) = 
                if first_x > second_x then 
                    ((second_x, second_y), (first_x, first_y)) 
                else ((first_x, first_y), (second_x, second_y))

            let (smaller_x_cord_y_change, larger_x_cord_y_change, smaller_x_cord_y_stop, larger_x_cord_y_stop) = 
                if smaller_x_cord_y > larger_x_cord_y then
                    (y_diff, -y_diff, y_limit, 0)
                else (-y_diff, y_diff, 0, y_limit)

            let res_seq = seq {

                let mutable smaller_x = smaller_x_cord_x

                for y in smaller_x_cord_y..smaller_x_cord_y_change..smaller_x_cord_y_stop do
                    if smaller_x >= 0 then yield (smaller_x, y)
                    smaller_x <- smaller_x - x_diff

                let mutable larger_x = larger_x_cord_x

                for y in larger_x_cord_y..larger_x_cord_y_change..larger_x_cord_y_stop do
                    if larger_x <= x_limit then yield (larger_x, y)
                    larger_x <- larger_x + x_diff
            }
            res_seq


        let list_length = coords.Length
        let res_seq = seq {
            
            for i in 0..(list_length - 1) do
                for j in (i + 1)..(list_length - 1) do
                    let antinode_line_seq : (int * int) seq = getAntinodeLine coords[i] coords[j]

                    yield! antinode_line_seq
        }
        res_seq

let solve (input: string array) : (string * string) =

    let antinode_set = 
        Collections.Generic.HashSet<(int * int)> ()

    let antinode_set_two = 
        Collections.Generic.HashSet<(int * int)> ()

    let coordinate_dict = 
        Collections.Generic.Dictionary<char, (int * int) list >()

    let x_limit = input[0].Length - 1
    let y_limit = input.Length - 1

    for y in 0..y_limit do
        for x in 0..x_limit do
            if (input[y][x] <> '.') then
                if (coordinate_dict.ContainsKey (input[y][x])) then
                    coordinate_dict.[input[y][x]] <- (x, y) :: coordinate_dict.[input[y][x]]
                else 
                    coordinate_dict.Add (input[y][x], [(x, y)])

    for kvp in coordinate_dict do
        
        let antinode_seq = Helper.getAntinode kvp.Value x_limit y_limit

        let antinode_seq_two = Helper.getAntinodeFixed kvp.Value x_limit y_limit


        for antinode in antinode_seq do
            antinode_set.Add antinode |> ignore

        for antinode in antinode_seq_two do
            antinode_set_two.Add antinode |> ignore

    ($"{antinode_set.Count}", $"{antinode_set_two.Count}")