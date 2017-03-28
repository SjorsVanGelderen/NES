(*
Copyright 2017, Sjors van Gelderen
*)

module Interpreter

open ASM
open CPU
open MEM

(*
type Step = AST -> CPU -> MEM -> Result
and Result =
    | Busy of AST * CPU * MEM
    | Done of AST * CPU * MEM

let bind (p: Step) (k: Result -> Step) =
    fun (ast: AST) (cpu: CPU) (mem: MEM) ->
        let result = p ast cpu mem
        match result with
        | Busy (_, cpu', mem') -> k ast cpu' mem'
        | Done (_, cpu', mem') -> Done (ast, cpu', mem')

let (>>=) = bind

type InterpreterMonad() =
    member this.Bind(p, k) = p >>= k
    member this.Return x = fun (ast: AST) (cpu: CPU) (mem: MEM) -> x
let interpreter = InterpreterMonad()
*)