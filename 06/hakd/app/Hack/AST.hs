module Hack.AST where

import GHC.Int (Int16)

data Symbol = A 
            | D
            | M
            | SP
            | LCL
            | ARG
            | THIS
            | THAT
            | R0
            | R1
            | R2
            | R3
            | R4
            | R5
            | R6
            | R7
            | R8
            | R9
            | R10
            | R11
            | R12
            | R13
            | R14
            | R15
            | SCREEN
            | KBD
            | Custom String

data Value  = VSymbol Symbol
            | VConstant Int16

data CInstruction =
    CInstruction
        (Maybe Symbol)
        Value
        (Maybe Jump)

data Instruction = ICInstruction Instruction

type AST = [Instruction]