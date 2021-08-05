module Hack.AST where

import GHC.Int (Int16)

import Hack.Symbol (Symbol)

data Value  = VSymbol Symbol
            | VConstant Int16

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