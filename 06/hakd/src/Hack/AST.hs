module Hack.AST where


import Hack.Symbol (Symbol)

data Value  = VSymbol Symbol
            | VConstant Integer

type Jump = Ordering

data CInstruction =
    CInstruction
        (Maybe Symbol)
        Value
        (Maybe Jump)

newtype AInstruction =
    AInstruction Value

data Instruction = ICInstruction CInstruction
                 | IAInstruction AInstruction

type AST = [Instruction]