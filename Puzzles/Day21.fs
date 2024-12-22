module AoC_2024.Day21

open System
open System.Collections.Generic

module Helper = 
    [<Struct>]
    type Pair = {
        Start: char
        End: char
    }

    let keypad_dict = 
        let kvp_seq = seq {
            KeyValuePair ('^', (1, 0))
            KeyValuePair ('A', (2, 0))

            KeyValuePair ('<', (0, 1))
            KeyValuePair ('V', (1, 1))
            KeyValuePair ('>', (2, 1))
        }
        Dictionary<char, (int * int)> (kvp_seq)

    let numpad_dict = 
        let kvp_seq = seq {
            KeyValuePair('7', (0, 0))
            KeyValuePair('8', (1, 0))
            KeyValuePair('9', (2, 0))

            KeyValuePair('4', (0, 1))
            KeyValuePair('5', (1, 1))
            KeyValuePair('6', (2, 1))

            KeyValuePair('1', (0, 2))
            KeyValuePair('2', (1, 2))
            KeyValuePair('3', (2, 2))

            KeyValuePair('0', (1, 3))
            KeyValuePair('A', (2, 3))
        }
        Dictionary<char, (int * int)> (kvp_seq)

    let getKeyPad1 () = 
        let getInitialCost (start_x: int, start_y: int) (end_x, end_y) =
            let dist_x = Math.Abs (start_x - end_x)
            let dist_y = Math.Abs (start_y - end_y)

            dist_x + dist_y + 1

        let keypad_1 = Dictionary<Pair, int64>()

        for kvp in keypad_dict do
            for kvp2 in keypad_dict do
                    let pair = { Start = kvp.Key; End = kvp2.Key }

                    let cost = getInitialCost kvp.Value kvp2.Value

                    keypad_1.Add (pair, cost)

        keypad_1

    let generateMoveSeq (cur_x, cur_y) (new_x, new_y) (avoid_x, avoid_y) =
        let do_x (the_dif) () = 
            let mutable the_dif' = the_dif
            [
                while the_dif' <> 0 do
                    if the_dif' > 0 then
                        the_dif' <- the_dif' - 1
                        yield '<'
                    elif the_dif' < 0 then
                        the_dif' <- the_dif' + 1
                        yield '>'
            ] 

        let do_y (the_dif) () =
            let mutable the_dif' = the_dif
            [
                while the_dif' <> 0 do
                    if the_dif' > 0 then
                        the_dif' <- the_dif' - 1
                        yield '^'
                    elif the_dif' < 0 then
                        the_dif' <- the_dif' + 1
                        yield 'V'
            ]
        let (dif_x, dif_y) = ((cur_x - new_x), (cur_y - new_y))

        let (list_1, list_2) = 
            if cur_y = avoid_y && (avoid_x + dif_x = cur_x) then
                //yield one list!
                (
                    Some [
                        yield! do_y dif_y ()
                        yield! do_x dif_x ()
                        yield 'A'
                    ], None
                )
            elif cur_x = avoid_x && (avoid_y + dif_y = cur_y) then
                //yield one list!
                (
                    Some [
                        yield! do_x dif_x ()
                        yield! do_y dif_y ()
                        yield 'A'
                    ], None
                )
            else
                //yield two lists :p
                (
                    Some [
                        yield! do_x dif_x ()
                        yield! do_y dif_y ()
                        yield 'A'
                    ], Some [
                        yield! do_y dif_y ()
                        yield! do_x dif_x ()
                        yield 'A'
                    ]
                )

        match (list_1, list_2) with
        | (Some list_1, None) -> [ list_1 ]
        | (None, Some list_2) -> [ list_2 ]
        | (Some list_1, Some list_2) -> [ list_1; list_2 ]
        | _ -> []

    let getMinCost (keypad_seq: char list) count =
        let rec getKeyPad (cur_key: Dictionary<Pair, int64>) count =
            if count = 0 then
                cur_key
            else
                let new_key = Dictionary<Pair, int64>()

                let the_dict = 
                    if count = 1 then numpad_dict else keypad_dict

                let avoid = 
                    if count = 1 then (0, 3) else (0, 0)

                for kvp in the_dict do
                    for kvp2 in the_dict do
                        if kvp.Key = kvp2.Key then new_key.Add ({ Start = kvp.Key; End = kvp.Key}, 1L)
                        else
                            let pair = { Start = kvp.Key; End = kvp2.Key }

                            let path_lists = 
                                generateMoveSeq kvp.Value kvp2.Value avoid
                                |> List.map (
                                    fun list ->
                                        Array.ofList ('A' :: list)
                                )

                            let mutable cost = Int64.MaxValue

                            for path_list in path_lists do
                                let mutable cost' = 0L

                                for i in 0..(path_list.Length - 2) do
                                    if path_list[i] = path_list[i + 1] then
                                        cost' <- cost' + 1L
                                    else cost' <- cost' + cur_key.[{ Start = path_list[i]; End = path_list[i + 1] }]

                                if cost' < cost then
                                    cost <- cost'

                            new_key.Add (pair, cost)

                getKeyPad new_key (count - 1)

    
        let final_kp = getKeyPad (getKeyPad1 ()) (count - 1)

        let mutable res = 0L

        let final_seq = 'A' :: keypad_seq

        for i in 0..(final_seq.Length - 2) do
            res <- res + final_kp.[{ Start = final_seq[i]; End = final_seq[i + 1] }]

        res



let solve (input: string array) : (string * string) = 
    let part_1 =
        input
        |> Array.map (
            fun press ->
                let value = Int64.Parse press[..(press.Length - 2)]
                let list = Seq.toList (press)

                (list, value)
        )
        |> Array.fold (
            fun acc (list, value) ->
                acc + ((Helper.getMinCost list 3) * value)
        ) 0L

    let part_2 =
        input
        |> Array.map (
            fun press ->
                let value = Int64.Parse press[..(press.Length - 2)]
                let list = Seq.toList (press)

                (list, value)
        )
        |> Array.fold (
            fun acc (list, value) ->
                acc + ((Helper.getMinCost list 26) * value)
        ) 0L

    ($"{part_1}", $"{part_2}")