{-# LANGUAGE OverloadedStrings, LambdaCase, DeriveFunctor, TupleSections #-}

module Text.Microparsec
    (Parser)
    where

import qualified Data.Text as Text
import Data.Text (Text)

data ParseableChars = SpecificChars Text | AnyChars | NoChars

data Error = Error
    { expected :: ParseableChars
    , found :: ParseableChars
    }

newtype State = State
    { remaining :: Text }

newtype Parser a = Parser
    { runParser :: State -> (State, Either Error a)
    } deriving (Functor)

instance Applicative Parser where
    pure result = Parser $ \state -> (state, Right result)

instance Monad Parser where
    (>>=) = ""

any :: Parser Char
any = Parser $ \case
    (State "") -> (State "", Left $ Error AnyChars NoChars)
    (State xs) -> (State $ Text.tail xs, Right $ Text.head xs)

eof :: Parser ()
eof = Parser $ \state ->
    case state of
        State "" -> (state, Right ())
        State xs -> (state, Left $ Error NoChars (SpecificChars xs))