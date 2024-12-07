module AoC_2024.Day07

open System

module Helper = 
    let canHitTarget (goal: int64) (numbers: int64 array) : bool =
        let rec canHitTarget' (goal:int64) (numbers: int64 array) (acc: int64) : bool =
            match numbers with
            | [||] -> goal = acc
            | _ ->
                let head = Array.head numbers
                let rest = Array.tail numbers
                (canHitTarget' goal rest (acc + head)) || (canHitTarget' goal rest (acc * head))

        canHitTarget' goal (Array.tail numbers) (Array.head numbers)

    let canHitTargetFixed (goal: int64) (numbers: int64 array) : bool =
        let rec canHitTargetFixed' (goal:int64) (numbers: int64 array) (acc: int64) : bool =
            match numbers with
            | [||] -> goal = acc
            | _ ->
                let head = Array.head numbers
                let rest = Array.tail numbers

                let mutable temp_num: int64 = head
                let mutable pow: int64 = 1L
                while temp_num > 0 do
                        temp_num <- temp_num / 10L
                        pow <- pow * 10L

                (canHitTargetFixed' goal rest (acc + head)) || (canHitTargetFixed' goal rest (acc * head))
                || (canHitTargetFixed' goal rest ((acc * pow) + head))

        canHitTargetFixed' goal (Array.tail numbers) (Array.head numbers)

let solve (input: string array) : (string * string) = 

    let mutable part1 = 0L
    let mutable half_of_part2 = 0L

    input
    |> Array.map (fun str -> 
                    let ind_col = str.IndexOf ':'
                    let test_val = Int64.Parse str[..(ind_col - 1)]
                    let numbers = (str[(ind_col + 1)..]).Split (' ', StringSplitOptions.RemoveEmptyEntries)
                    let numbers' = [|for num in numbers -> Int64.Parse num|]
                    (test_val, numbers'))
    |> Array.iter (fun (test_val, numbers) ->
                        if (Helper.canHitTarget test_val numbers) then
                            part1 <- part1 + test_val
                        elif (Helper.canHitTargetFixed test_val numbers) then
                            half_of_part2 <- half_of_part2 + test_val)
                            
    ($"{part1}", $"{part1 + half_of_part2}")