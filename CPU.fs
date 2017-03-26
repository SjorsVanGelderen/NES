(*
Copyright 2017, Sjors van Gelderen
*)

module CPU

open ASM

(*
Status register masks
*)
let status_mask_sign       = 0b10000000
let status_mask_overflow   = 0b01000000
// There is an unused bit here
let status_mask_breakpoint = 0b00010000
let status_mask_decimal    = 0b00001000
let status_mask_interrupt  = 0b00000100
let status_mask_zero       = 0b00000010
let status_mask_carry      = 0b00000001

(*
CPU state
*)
type CPU =
    {
        A:  byte  // Accumulator Register
        X:  byte  // X Index Register
        Y:  byte  // Y Index Register
        SP: byte  // Stack pointer
        PC: int16 // Program counter
        SR: byte  // Status Register
    } with
    static member Zero:
    {
        A  = 0
        X  = 0
        Y  = 0
        SP = 0
        PC = 0
        SR = 0
    }

let process (cpu: CPU) (i: Instruction) =
    match i with
    | DEC -> { cpu with A = A - 1 }
    | DEX -> { cpu with X = X - 1 }
    | DEY -> { cpu with Y = Y - 1 }
    | INC -> { cpu with A = A + 1 }
    | INX -> { cpu with X = X + 1 }
    | INY -> { cpu with Y = Y + 1 }
    | JMP pc -> { cpu with PC = pc }
    | TAX -> { cpu with X = A }
    | TAY -> { cpu with Y = A }
    | TSX -> { cpu with SP = X }
    | TXA -> { cpu with A = X }
    | TXS -> { cpu with SP = X }
    | TYA -> { cpu with A = Y }
    | _ -> ()

type Step = AST -> CPU -> MEM -> Result
and Result =
    | Run  of AST * CPU * MEM
    | Halt of AST * CPU * MEM

let bind (p: Step) (k: Result -> Step) =
    fun (ast: AST) (cpu: CPU) (mem: MEM) ->
        let result = p ast cpu mem
        match result with
        | Run (_, cpu', mem') -> k ast cpu'
        | Halt (_, cpu', mem') -> Halt (ast, cpu', mem')

let (>>=) = bind

type InterpreterMonad() =
    member this.Bind(p, k) = p >>= k
    member this.Return x = fun (ast: AST) (cpu: CPU) (mem: MEM) -> x
let interpreter = InterpreterMonad()
