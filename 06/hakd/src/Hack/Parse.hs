module Hack.Parse where

import Control.Applicative (Alternative ((<|>)))
import Hack.AST
import Hack.Symbol
import Text.Microparsec
import Text.Read (readMaybe)

predefinedSymbol :: Parser PredefinedSymbol
predefinedSymbol = do
  xs <- identifier
  maybePredefinedSymbol <-
    (readMaybe :: String -> Maybe (PredefinedSymbol))
      xs

symbol :: Parser Symbol
symbol =
  (Predefined <$> try predefinedSymbol)
    <|> (Custom <$> identifier)

constant :: Parser Int
constant = int

value :: Parser Value
value = constant <|> symbol

aInstruction :: Parser AInstruction
aInstruction = do
  _ <- char '@'
  value
