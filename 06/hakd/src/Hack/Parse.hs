module Hack.Parse where

import Hack.AST
import Text.Microparsec

value :: Parser Value
value 

aInstruction :: Parser AInstruction
aInstruction = do
    _ <- char '@'
    value