module Hack.Symbol (Symbol (..), PredefinedSymbol (..)) where

data PredefinedSymbol
  = A
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
  deriving (Show, Eq, Read)

data Symbol
  = Predefined PredefinedSymbol
  | Custom String
  deriving (Show, Eq)
