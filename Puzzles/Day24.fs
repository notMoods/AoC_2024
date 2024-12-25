module AoC_2024.Day24

open System
open System.Collections.Generic

type Gate = {
    Input1: string
    Input2: string

    Output: string
    Logic: bool -> bool -> bool
}
let solve (input: string array) : (string * string) = 
    let (first_section, second_section) = 
        let index_of_space = Array.findIndex (fun str -> str = String.Empty) input

        (input[..(index_of_space - 1)], input[(index_of_space + 1)..])

    let dict_of_wires = Dictionary<string, bool>()
    let queue = Queue<Gate> ()

    first_section
    |> Array.iter (
        fun wire ->
            let details = wire.Split(':', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)

            let wire_name = details[0]
            let value = details[1] = "1"

            dict_of_wires.Add (wire_name, value)
    )

    //modify this after solving
    let (decimal_x, decimal_y, _, _) = 
        dict_of_wires
        |> Seq.filter (fun kvp -> kvp.Key.StartsWith('x') || kvp.Key.StartsWith('y'))
        |> Seq.sortBy (fun kvp -> kvp.Key)
        |> Seq.fold (
            fun (acc_x, acc_y, index_x, index_y) kvp ->
                if kvp.Key.StartsWith('x') then
                    if kvp.Value then
                        (acc_x + int64 (1L <<< index_x), acc_y, index_x + 1, index_y)
                    else (acc_x, acc_y, index_x + 1, index_y)
                else
                    if kvp.Value then
                        (acc_x, acc_y + int64 (1L <<< index_y), index_x, index_y + 1)
                    else (acc_x, acc_y, index_x, index_y + 1)
        ) (0L, 0L, 0, 0)

    second_section
    |> Array.iter (
        fun gate ->
            //ntg XOR fgs -> mjb
            let details = gate.Split([|" "; "->"|], StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)

            let logic = 
                match details[1] with
                | "AND" -> fun x y -> x && y
                | "OR" -> fun x y -> x || y
                | "XOR" -> fun x y -> x <> y
                | _ -> fun x y -> x = y
            
            queue.Enqueue ({
                Input1 = details[0]
                Input2 = details[2]
                Output = details[3]

                Logic = logic
            })
    )

    while queue.Count > 0 do
        let gate = queue.Peek()

        if (dict_of_wires.ContainsKey gate.Input1) && (dict_of_wires.ContainsKey gate.Input2) then
            let value = gate.Logic dict_of_wires[gate.Input1] dict_of_wires[gate.Input2]
            dict_of_wires.Add (gate.Output, value)
            queue.Dequeue () |> ignore
        else 
            queue.Dequeue () |> ignore
            queue.Enqueue gate
        ()

    let (part_one, _) = 
        dict_of_wires
        |> Seq.filter (fun kvp -> kvp.Key.StartsWith('z'))
        |> Seq.sortBy (fun kvp -> kvp.Key)
        |> Seq.fold (
            fun (acc, index) kvp ->
                if kvp.Value then
                    (acc + int64 (1L <<< index), index + 1)
                else (acc, index + 1)
        ) (0L, 0)




    ($"{part_one}", "")