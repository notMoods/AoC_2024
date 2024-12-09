module AoC_2024.Day09

open System

module Helper = 
    type FreeBlockSpace = {
        StartIndex: int
        Length: int
    }

    type FileBlock = {
        EndIndex: int
        Length: int
        Digit: char
    }

    let getBlockSpace (pointer_to_dot: int) (block_seq: (char * char) array) : FreeBlockSpace =
        let mutable length = 1

        while (snd block_seq[pointer_to_dot + length] = '.') do
            length <- length + 1

        {
            StartIndex = pointer_to_dot
            Length = length
        }

    let getFileBlock (pointer_to_digit_from_end: int) (block_seq: (char * char) array) : FileBlock =
        let mutable length = 1
        let digit = snd block_seq[pointer_to_digit_from_end]

        while (snd block_seq[pointer_to_digit_from_end - length] = digit) do
            length <- length + 1

        {
            EndIndex = pointer_to_digit_from_end
            Length = length
            Digit = digit
        }

let solve (input: string array) : (string * string) = 

    let mutable index = 0
    let mutable should_print_num = true

    let block_seq =
        input[0]
        |> Seq.collect (fun c ->
                            seq {
                                let loop_end = int c - 48

                                let printed_char = 
                                    if should_print_num then
                                        char (index + 48)
                                    else 
                                        index <- index + 1
                                        '.'
                                should_print_num <- should_print_num <> true

                                for i in 1..loop_end do
                                    yield (printed_char, printed_char)
                            })
        |> Seq.toArray

    let mutable first_pointer = Array.findIndex (
                                    function
                                    | ('.', _) -> true
                                    | _ -> false) block_seq

    let mutable second_pointer = Array.findIndexBack (
                                function
                                |('.', _) -> false
                                |_ -> true) block_seq

    while first_pointer < second_pointer do
        let (_, dont_move) = block_seq[first_pointer]
        block_seq[first_pointer] <- (fst block_seq[second_pointer], dont_move)

        let (_, dont_move') = block_seq[second_pointer]
        block_seq[second_pointer] <- ('.', dont_move')

        while (fst block_seq[first_pointer] <> '.') do
            first_pointer <- first_pointer + 1
        while (fst block_seq[second_pointer] = '.') do
            second_pointer <- second_pointer - 1

    second_pointer <- Array.findIndexBack (
                                function
                                |(_, '.') -> false
                                |_ -> true) block_seq

    while snd block_seq[second_pointer] <> '0' do

        first_pointer <- Array.findIndex (
                                    function
                                    | (_, '.') -> true
                                    | _ -> false) block_seq

        let mutable hasNotBeenMoved = true
        let file_block = Helper.getFileBlock second_pointer block_seq

        while hasNotBeenMoved && first_pointer < second_pointer do

            let free_block_space = Helper.getBlockSpace first_pointer block_seq
            
            if (free_block_space.Length >= file_block.Length) then
                hasNotBeenMoved <- false
                for i in free_block_space.StartIndex..(free_block_space.StartIndex + file_block.Length - 1) do
                    let (dont_move, _) = block_seq[i]
                    block_seq[i] <- (dont_move, file_block.Digit)
                
                for i in file_block.EndIndex .. -1 .. (file_block.EndIndex - file_block.Length + 1) do
                    let (dont_move, _) = block_seq[i]
                    block_seq[i] <- (dont_move, '.')
            else
                first_pointer <- first_pointer + free_block_space.Length
                while ( snd block_seq[first_pointer] <> '.') do
                    first_pointer <- first_pointer + 1

            if first_pointer >= block_seq.Length then
                hasNotBeenMoved <- false
  

        second_pointer <- second_pointer - file_block.Length
        while (snd block_seq[second_pointer] = '.') do
            second_pointer <- second_pointer - 1


    let (part_1, part_2, _) =
        block_seq
        |> Array.fold (
            fun (acc1, acc2, index) (c1, c2) ->
                let add1 = if c1 <> '.' then int64 (index * (int c1 - 48)) else 0L
                let add2 = if c2 <> '.' then int64 (index * (int c2 - 48)) else 0L
                (acc1 + add1, acc2 + add2, index + 1)
        ) (0L, 0L, 0)

    ($"{part_1}", $"{part_2}")
