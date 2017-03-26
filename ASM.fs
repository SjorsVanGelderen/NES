(*
Copyright 2017, Sjors van Gelderen
*)

module ASM

type Operand =
    | Accumulator
    //| Implied
    | Immediate of byte
    | Relative  of byte
    | Absolute  of int16
    | ZeroPage  of byte
    | Indirect  of byte * int16
    | AbsoluteIndexed  of byte * Register
    | ZeroPagedIndexed of byte * Register
    | DirectIndexed    of byte * Register
    | IndirectIndexed

type Instruction =
    | ADC
    | AND
    | ASL
    | BCC
    | BCS
    | BEQ
    | BIT
    | BMI
    | BNE
    | BPL
    | BRK
    | BVC
    | BVS
    | CLC
    | CLD
    | CLI
    | CLV
    | CMP
    | CPX
    | CPY
    | DEC
    | DEX
    | DEY
    | EOR
    | INC
    | INX
    | INY
    | JMP
    | JSR
    | LDA of Operand
    | LDX of Operand
    | LDY of Operand
    | LSR
    | NOP
    | ORA
    | PHA
    | PHP
    | PLA
    | PLP
    | ROL
    | ROR
    | RTI
    | RTS
    | SBC
    | SEC
    | SED
    | SEI
    | STA of Operand
    | STX of Operand
    | STY of Operand
    | TAX
    | TAY
    | TSX
    | TXA
    | TXS
    | TYA

let ast =
    [
        LDX (Immediate (0xFF))
        STX (Absolute (0x0000))
    ]
