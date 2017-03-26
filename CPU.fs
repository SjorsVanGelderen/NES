(*
Copyright 2017, Sjors van Gelderen
*)

module CPU

open ASM
open MEM

(*
Status register masks
*)
let status_mask_sign       = 0b10000000
let status_mask_overflow   = 0b01000000
// There is an unused bit here
let status_mask_breakpoint = 0b00010000
//let status_mask_decimal    = 0b00001000
let status_mask_interrupt  = 0b00000100
let status_mask_zero       = 0b00000010
let status_mask_carry      = 0b00000001

type Register =
    | A
    | X
    | Y

type CPU =
    {
        A:  byte  // Accumulator Register
        X:  byte  // X Index Register
        Y:  byte  // Y Index Register
        SP: byte  // Stack pointer
        PC: int16 // Program counter
        SR: byte  // Status Register
    }

let cpu_zero =
    {
        A  = 0
        X  = 0
        Y  = 0
        SP = 0
        PC = 0
        SR = 0
    }

let cpu_increase_pc cpu = { cpu with PC = PC + 1 }

let cpu_set_register (r: Register) (o: Operand) =
    let value =
        match o with
        | Immediate value -> value
        | _ -> -1 // ERROR
    
    match r with
    | A -> { cpu' with A = value }
    | X -> { cpu' with X = value }
    | Y -> { cpu' with Y = value }

let process_instruction (cpu: CPU) (mem: MEM) (i: Instruction) =
    match i with
    (*
    | ADC -> cpu, mem
    | AND -> cpu, mem
    | ASL -> cpu, mem
    | BCC -> cpu, mem
    | BCS -> cpu, mem
    | BEQ -> cpu, mem
    | BIT -> cpu, mem
    | BMI -> cpu, mem
    | BNE -> cpu, mem
    | BPL -> cpu, mem
    | BRK -> cpu, mem
    | BVC -> cpu, mem
    | BVS -> cpu, mem
    | CLC -> cpu, mem
    | CLD -> cpu, mem
    | CLI -> cpu, mem
    | CLV -> cpu, mem
    | CMP -> cpu, mem
    | CPX -> cpu, mem
    | CPY -> cpu, mem
    | DEC -> { cpu with A = A - 1 }, mem
    | DEX -> { cpu with X = X - 1 }, mem
    | DEY -> { cpu with Y = Y - 1 }, mem
    | EOR -> cpu, mem
    | INC -> { cpu with A = A + 1 }, mem
    | INX -> { cpu with X = X + 1 }, mem
    | INY -> { cpu with Y = Y + 1 }, mem
    | JMP line -> { cpu with PC = line }, mem
    | JSR -> cpu, mem
    | LDA operand -> cpu_set_register A operand, mem
    *)
    | LDX operand -> cpu_increase_pc cpu_set_register X operand, mem
    | LDY operand -> cpu_increase_pc cpu_set_register Y operand, mem
    (*
    | LSR -> cpu, mem
    | NOP -> cpu, mem
    | ORA -> cpu, mem
    | PHA -> cpu, mem
    | PHP -> cpu, mem
    | PLA -> cpu, mem
    | PLP -> cpu, mem
    | ROL -> cpu, mem
    | ROR -> cpu, mem
    | RTI -> cpu, mem
    | RTS -> cpu, mem
    | SBC -> cpu, mem
    | SEC -> cpu, mem
    | SED -> cpu, mem
    | SEI -> cpu, mem
    | STA -> cpu, mem
    *)
    | STX operand -> cpu_increase_pc cpu, memory_set operand cpu.X
    | STY operand -> cpu_increase_pc cpu, memory_set operand cpu.Y
    (*
    | TAX -> { cpu with X = A }, mem
    | TAY -> { cpu with Y = A }, mem
    | TSX -> { cpu with SP = X }, mem
    | TXA -> { cpu with A = X }, mem
    | TXS -> { cpu with SP = X }, mem
    | TYA -> { cpu with A = Y }, mem
    *)
    | _ ->
        printfn "Error, unrecognized instruction: %A" instruction
        cpu, mem
