module AoC_2024.Day17

open System
module Helper = 
    type Computer(RegA: uint64, RegB: uint64, RegC: uint64, Prog: int array) =
        let mutable programCounter = 0
        let mutable Register_A = RegA
        let mutable Register_B = RegB
        let mutable Register_C = RegC
        let resolveCombo operand = 
            match operand with
            | 0 | 1 | 2 | 3 -> uint64 operand
            | 4 -> Register_A
            | 5 -> Register_B
            | 6 -> Register_C
            | _ -> 
                printfn "error"
                0UL


        let output_list = Collections.Generic.List<int>()
        let mutable canExecute = true

        let Program = Prog

        member this.CanExecute () =
            canExecute

        member this.PrintOutput () =
            String.Join (",", output_list)

        member this.ExecuteProgram () =
            if programCounter >= Program.Length  then
                printfn "Program done!"
                canExecute <- false
                ()
            else
                let opcode = Program[programCounter]
                let operand = Program[programCounter + 1]

                match opcode with
                | 0 ->
                    let numerator = float Register_A
                    let denominator = Math.Pow (2, float (resolveCombo operand))

                    let res = numerator / denominator
                    Register_A <- uint64 res

                    programCounter <- programCounter + 2
                | 1 ->
                    Register_B <- Register_B ^^^ (uint64 operand)

                    programCounter <- programCounter + 2
                | 2 ->
                    Register_B <- (resolveCombo operand) % 8UL

                    programCounter <- programCounter + 2
                | 3 ->
                    let mutable jumped = false
                    if Register_A <> 0UL then
                            programCounter <- operand
                            jumped <- true

                    if not jumped then programCounter <- programCounter + 2

                | 4 ->
                    Register_B <- Register_B ^^^ Register_C

                    programCounter <- programCounter + 2
                
                | 5 ->
                    let printer = (resolveCombo operand) % 8UL

                    output_list.Add (int printer)

                    programCounter <- programCounter + 2

                | 6 ->
                    let numerator = float Register_A
                    let denominator = Math.Pow (2, float (resolveCombo operand))

                    let res = numerator/ denominator
                    Register_B <- uint64 res

                    programCounter <- programCounter + 2
                | 7 ->
                    let numerator = float Register_A
                    let denominator = Math.Pow (2, float (resolveCombo operand))

                    let res = numerator/ denominator
                    Register_C <- uint64 res

                    programCounter <- programCounter + 2

                | _ -> printfn "HOOOOOOW"
            
    let solvePart2 ( program: int array ) =
        let mutable res = 0UL

        //printfn "%A" program

        let mutable res_list = Collections.Generic.List<uint64>()

        for i in 0UL..7UL do
            res_list.Add (i)

        for i in (program.Length - 1)..(-1)..0 do
            //let start = res
            //let check_stop = start + 7UL

            let new_res_list = Collections.Generic.List<uint64>()

            for a_guess in res_list do
                let r_b = (((a_guess % 8UL) ^^^ 5UL) &&& 7UL)

                let denom =  (Math.Pow(2, float r_b))
                let main_r_c = uint64 (a_guess / uint64 denom) //&&& 8UL
                let main_r_b = (r_b ^^^ 6UL) &&& 7UL

                let printed_num = (main_r_c ^^^ main_r_b) &&& 7UL

                if printed_num = (uint64 program[i]) then
                    //printfn "%A, %A, %A" printed_num (program[i]) i
                    //printfn "%A" a_guess
                    let blaa = a_guess * 8UL
                    for a in blaa..(blaa + 7UL) do
                        new_res_list.Add(a)

            res_list <- Collections.Generic.List<uint64>(new_res_list)
        res_list
let solve (input: string array) : (string * string) = 

    let program = 
        input[input.Length - 1]
        |> (fun str ->
            let ind = str.IndexOf ':'
            str[(ind + 1)..])
        |> (fun str ->
                let str' = str.Split (',', StringSplitOptions.RemoveEmptyEntries)
                [| for num in str' do int num|])
 
    let regA =
        input[0]
        |> (fun str ->
            let ind = str.IndexOf ':'
            str[(ind + 1)..])
        |> UInt64.Parse 

    let regB =
        input[1]
        |> (fun str ->
            let ind = str.IndexOf ':'
            str[(ind + 1)..])
        |> UInt64.Parse 

    let regC =
        input[2]
        |> (fun str ->
            let ind = str.IndexOf ':'
            str[(ind + 1)..])
        |> UInt64.Parse 

    //printfn "%A %A %A %A" regA regB regC program

    let comp = Helper.Computer (848691058134224UL, regB, regC, program)

    while comp.CanExecute () do
        comp.ExecuteProgram ()

    let part_1 = comp.PrintOutput ()

    let part_2_list = Helper.solvePart2 program

    let part_2 = 
        part_2_list
        |> Seq.min
            
    (part_1, $"{part_2}")