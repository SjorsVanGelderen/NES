(*
Copyright 2017, Sjors van Gelderen
*)

module MEM

open ASM

type MEM = byte List

let mem_zero = [ for i in 0..64 -> 0 ]

let mem_get (mem: MEM) (address: Operand) =
    let index =
        match address with
        | Absolute address -> address
        | _ -> int16 -1 // ERROR
    mem.[int index]

let mem_set (mem: MEM) (address: Operand) (value: byte) =
    let index =
        match address with
        | Absolute address -> address
        | _ -> int16 -1 // ERROR
    
    List.fold (fun (i, acc) elem ->
        if i = index then
            (i, value :: acc)
        else
            (i + int16 1, elem :: acc)) (int16 0, []) mem
