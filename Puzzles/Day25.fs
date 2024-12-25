module AoC_2024.Day25

open System
open System.Collections.Generic

module Helper = 
    let getKeysAndLocks (keys_and_locks: string array array) =
        let keys = List<string array>()
        let locks = List<string array>()

        for region in keys_and_locks do
            if region[0].Contains '.' then
                keys.Add region
            else locks.Add region

        (keys.ToArray(), locks.ToArray())

    let findKeyCombo (key: Dictionary<int, int>) (lock_numbers: Dictionary<int, int> array) (key_lock_limit)= 

        let combo_count = 
            lock_numbers
            |> Array.fold (
                fun acc lock ->
                    let mutable can_fit = true

                    for kvp in lock do
                        let lock_value = kvp.Value
                        let key_value = key[kvp.Key]

                        if lock_value + key_value > key_lock_limit then
                            can_fit <- false

                    if can_fit then
                        acc + 1
                    else acc
            ) 0

        combo_count

let solve (input: string array) : (string * string) =
    let height_of_region = 
            Array.findIndex (fun str -> str = String.Empty) input
 
    let keys_and_locks = 
        input
        |> Array.filter (fun str -> str <> String.Empty)
        |> Array.chunkBySize height_of_region

    let (keys, locks) = Helper.getKeysAndLocks keys_and_locks

    let key_numbers = 
        keys
        |> Array.map (
            fun region ->
                let index_to_number = Dictionary<int, int>()

                for i in 0..(region[0].Length - 1) do
                    let mutable hash_count = 0
                    let mutable start = region.Length - 2

                    while region[start][i] = '#' do
                        hash_count <- hash_count + 1
                        start <- start - 1

                    index_to_number.Add(i, hash_count)

                index_to_number                 
        )

    let lock_numbers = 
        locks
        |> Array.map (
            fun region ->
                let index_to_number = Dictionary<int, int>()

                for i in 0..(region[0].Length - 1) do
                    let mutable hash_count = 0
                    let mutable start = 1

                    while region[start][i] = '#' do
                        hash_count <- hash_count + 1
                        start <- start + 1

                    index_to_number.Add (i, hash_count)

                index_to_number
        )

    

    let part_one =
        key_numbers
        |> Array.fold (
            fun acc key ->
                let unique_combo = Helper.findKeyCombo key lock_numbers (height_of_region - 2)

                acc + unique_combo
        ) 0


    ($"{part_one}", "Freebie!")