module AoC_2024.Day22

open System
open System.Collections.Generic
open System.Linq    

module Helper = 
    let takeToSecretNumber (num: int64) = 
        let temp1 = ((num <<< 6) ^^^ num) &&& 16777215L
        let temp2 = ((temp1 >>> 5) ^^^ temp1) &&& 16777215L

        ((temp2 <<< 11) ^^^ temp2) &&& 16777215L

    let rec repeatXTimes (func: 'a -> 'a) count (value: 'a) =
        if count = 0 then
            value
        else 
            repeatXTimes func (count - 1) (func value)

    let addChangeAndPrice (dict: Dictionary<(sbyte * sbyte * sbyte * sbyte), sbyte>) 
        (changes: sbyte array) (price: sbyte )=
        let changes = (changes[0], changes[1], changes[2], changes[3])

        if not (dict.ContainsKey changes) then
            dict.Add (changes, price)
        
    let getDictForSequence (num: int64) = 
        let alterChanges (arr: sbyte array) (change: sbyte) =
            for i in 0..2 do
                arr[i] <- arr[i + 1]
            arr[3] <- change

        let cur_dict = Dictionary<(sbyte * sbyte * sbyte * sbyte), sbyte>()


        let changes = [| 0y; 0y; 0y; 0y |]

        let mutable cur_num = num
        for i in 0..3 do
            let cur_price = sbyte (cur_num % 10L)

            let new_num = takeToSecretNumber cur_num
            let new_price = sbyte (new_num % 10L)
            changes[i] <- new_price - cur_price

            cur_num <- new_num

            if i = 3 then
                addChangeAndPrice cur_dict changes new_price

        for i in 1..1996 do
            let cur_price = sbyte (cur_num % 10L)

            let new_num = takeToSecretNumber cur_num
            let new_price = sbyte (new_num % 10L)
            
            alterChanges changes (new_price - cur_price)

            cur_num <- new_num
            addChangeAndPrice cur_dict changes new_price

        cur_dict
        
let solve (input: string array) : (string * string) = 
    let values = 
        input
        |> Array.map Int64.Parse

    let part_1 = 
        values
        |> Array.fold (
            fun acc num ->
                let secret_num_2000 = Helper.repeatXTimes Helper.takeToSecretNumber 2000 num
                acc + secret_num_2000
        ) 0L

    let res_dict_of_seq = Dictionary<(sbyte * sbyte * sbyte * sbyte), int64>()

    values
    |> Array.map (
        fun num -> Helper.getDictForSequence num )
    |> Array.iter (
        fun dict ->
            for kvp in dict do
                let sequence = kvp.Key

                if res_dict_of_seq.ContainsKey sequence then
                    res_dict_of_seq[sequence] <- res_dict_of_seq[sequence] + (int64 kvp.Value)
                else res_dict_of_seq.Add (sequence, int64 kvp.Value) )

    let part_2 = 
        Enumerable.Max res_dict_of_seq.Values
    
    ($"{part_1}", $"{part_2}")