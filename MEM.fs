(*
Copyright 2017, Sjors van Gelderen
*)

module MEM

open ASM

type MEM = byte Array

let mem_zero = [ for i in 0..64 -> 0 ]

let mem_get (mem: MEM) (address: Operand) =
    let index =
        match address with
        | Absolute address -> address
        | _ -> -1 // ERROR
    List.nth mem index

let mem_set (mem: MEM) (address: Operand) (value: byte) =
    let index =
        match address with
        | Absolute address -> address
        | _ -> -1 // ERROR
    
    List.fold (fun (acc, i), elem ->
        if i = index then
            value :: acc
        else
            elem :: acc) [] mem
