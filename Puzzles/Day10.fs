module AoC_2024.Day10

open System

module Helper =
    let trailheadScore (x, y) (input : string array) : int =
        let x_max = input[0].Length - 1
        let y_max = input.Length - 1
        let rec trailheadScore' (digit: int) (cur_x: int, cur_y: int) 
            (set: Collections.Generic.HashSet<(int * int)>) : unit =
                let inBounds (x, y) : bool =
                    x >= 0 && x <= x_max && y >= 0 && y <= y_max

                if digit = 9 then
                    set.Add (cur_x, cur_y) |> ignore
                else 
                    let above_coord = (cur_x, cur_y - 1)
                    let under_coord = (cur_x, cur_y + 1)
                    let left_coord = (cur_x - 1, cur_y)
                    let right_coord = (cur_x + 1, cur_y)

                    if (inBounds above_coord) && (int (input[snd above_coord][fst above_coord]) - 49 = digit) then
                        trailheadScore' (int (input[snd above_coord][fst above_coord]) - 48) above_coord set

                    if (inBounds under_coord) && (int (input[snd under_coord][fst under_coord]) - 49 = digit) then
                        trailheadScore' (int (input[snd under_coord][fst under_coord]) - 48) under_coord set

                    if (inBounds left_coord) && (int (input[snd left_coord][fst left_coord]) - 49 = digit) then
                        trailheadScore' (int (input[snd left_coord][fst left_coord]) - 48) left_coord set

                    if (inBounds right_coord) && (int (input[snd right_coord][fst right_coord]) - 49 = digit) then
                        trailheadScore' (int (input[snd right_coord][fst right_coord]) - 48) right_coord set
                

        let valid_nines = Collections.Generic.HashSet<(int * int)> ()

        trailheadScore' 0 (x, y) valid_nines

        valid_nines.Count

    
    let trailheadRating (x, y) (input : string array) : int =
        let x_max = input[0].Length - 1
        let y_max = input.Length - 1
        let rec trailheadRating' (digit: int) (cur_x: int, cur_y: int) : int =
                let inBounds (x, y) : bool =
                    x >= 0 && x <= x_max && y >= 0 && y <= y_max

                if digit = 9 then
                    1
                else 
                    let above_coord = (cur_x, cur_y - 1)
                    let under_coord = (cur_x, cur_y + 1)
                    let left_coord = (cur_x - 1, cur_y)
                    let right_coord = (cur_x + 1, cur_y)

                    let s1 = if (inBounds above_coord) && (int (input[snd above_coord][fst above_coord]) - 49 = digit) then
                                        trailheadRating' (int (input[snd above_coord][fst above_coord]) - 48) above_coord 
                                    else 0

                    let s2 = if (inBounds under_coord) && (int (input[snd under_coord][fst under_coord]) - 49 = digit) then
                                        trailheadRating' (int (input[snd under_coord][fst under_coord]) - 48) under_coord
                                    else 0

                    let s3 = if (inBounds left_coord) && (int (input[snd left_coord][fst left_coord]) - 49 = digit) then
                                        trailheadRating' (int (input[snd left_coord][fst left_coord]) - 48) left_coord 
                                    else 0

                    let s4 = if (inBounds right_coord) && (int (input[snd right_coord][fst right_coord]) - 49 = digit) then
                                        trailheadRating' (int (input[snd right_coord][fst right_coord]) - 48) right_coord
                                    else 0
                    
                    s1 + s2 + s3 + s4
                

        trailheadRating' 0 (x, y) 


let solve (input: string array) : (string * string) = 
    let (part_1, part_2) = 
        input
        |> Seq.indexed
        |> Seq.collect (fun (y, str) ->
                            seq {
                                for x in 0..(str.Length - 1) do
                                    if str[x] = '0' then
                                        yield (x, y)
                            })
        |> Seq.map (fun coord -> (Helper.trailheadScore coord input, Helper.trailheadRating coord input))
        |> Seq.fold (fun (acc1, acc2) (p1, p2) -> (acc1 + p1, acc2 + p2)) (0, 0)
    
    ($"{part_1}", $"{part_2}")