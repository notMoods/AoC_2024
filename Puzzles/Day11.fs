module AoC_2024.Day11

open System

module Helper =
    let rec repeatXTimes (func: 'a -> 'a) (value: 'a)  (repeatCount: int) : 'a =
        if repeatCount = 0 then
            value
        else
            repeatXTimes func (func value) (repeatCount - 1) 

    let getDigitCount (num: int64) : int =
        let mutable length = 0
        let mutable num' = num

        while num' > 0 do
            num' <- num' / 10L
            length <- length + 1

        length

    let getSection (num: int64) (start: int) (stop: int) : int64 =
        let mutable res = 0L
        let mutable multiplier = 1L

        let mutable num' = num

        for i in 0..stop do
            if i >= start then
                res <- res + ((num' % 10L) * multiplier)
                multiplier <- multiplier * 10L
            num' <- num' / 10L

        res

    let addValueToDict (dict: Collections.Generic.Dictionary<'a, uint64>) (key: 'a) (value) : unit =
        if dict.ContainsKey key then
            dict[key] <- dict[key] + value
        else dict.Add (key, value)
    
let solve (input: string array) : (string * string) =
    let dict = Collections.Generic.Dictionary<int64, uint64> ()
    
    input[0].Split (' ', StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun x -> Int64.Parse x)
    |> Array.iter (fun x ->
                    if dict.ContainsKey x then
                        dict[x] <- dict[x] + 1UL
                    else dict.Add (x, 1UL))

    let solver =
        dict
        |> Helper.repeatXTimes (fun dict ->
                                        let new_dict = Collections.Generic.Dictionary<int64, uint64> ()

                                        for kvp in dict do
                                            let x = kvp.Key
                                            let x_count = kvp.Value

                                            let x_digit_count = Helper.getDigitCount x

                                            if x = 0 then
                                                Helper.addValueToDict new_dict 1 x_count

                                            elif (x_digit_count % 2 = 0) then
                                                let first_half = Helper.getSection x (x_digit_count / 2) (x_digit_count - 1)
                                                let second_half = Helper.getSection x 0 (x_digit_count / 2 - 1)
                                                
                                                Helper.addValueToDict new_dict first_half x_count
                                                Helper.addValueToDict new_dict second_half x_count

                                            else Helper.addValueToDict new_dict (x * 2024L) x_count
                                            
                                        new_dict
                                        ) 

    let (part_1, part_2) = 
        (Seq.sumBy (fun (kvp: Collections.Generic.KeyValuePair<int64, uint64>) -> kvp.Value) (solver 25), 
        Seq.sumBy (fun (kvp: Collections.Generic.KeyValuePair<int64, uint64>) -> kvp.Value) (solver 75))
        
    ($"{part_1}", $"{part_2}")