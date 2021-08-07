module Hack.Parse where

import Control.Applicative (Alternative ((<|>)))
import Hack.AST
import Text.Microparsec

constant :: Parser Int
constant = int

-- value :: Parser Value
-- value = constant <|> symbol

-- aInstruction :: Parser AInstruction
-- aInstruction = do
--   _ <- char '@'
--   value
