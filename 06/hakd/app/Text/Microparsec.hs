{-# LANGUAGE OverloadedStrings, LambdaCase, DeriveFunctor, TupleSections #-}

module Text.Microparsec
    ( Parser
    , anyChar
    , eof
    , char )
    where

import qualified Data.Text as Text
import Data.Text (Text)

data ParseableChars = SpecificChar Char | SpecificChars Text | AnyChars | NoChars

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
    pure result = Parser (, Right result)

    pf <*> p = Parser $ \state ->
        case runParser pf state of
            -- We can't just write `error@(_, Left _)` as that will
            -- have a **more specific type**,
            -- `(State, Either Error (a -> b))`
            -- instead of `(State, Either Error c)`
            (s', Left err) -> (s', Left err)

            (s', Right f) -> fmap f <$> runParser p s'

instance Monad Parser where
    m >>= g = Parser $ \state ->
        case runParser m state of
            -- See comment in `Applicative` definition
            (s', Left err) -> (s', Left err)
            (s', Right x) -> runParser (g x) s'

anyChar :: Parser Char
anyChar = Parser $ \case
    (State "") -> (State "", Left $ Error AnyChars NoChars)
    (State xs) -> (State $ Text.tail xs, Right $ Text.head xs)

eof :: Parser ()
eof = Parser $ \state ->
    case state of
        State "" -> (state, Right ())
        State xs -> (state, Left $ Error NoChars (SpecificChars xs))

char :: Char -> Parser Char
char x = Parser $ \case
    (State "") -> (State "", Left $ Error AnyChars NoChars)
    (State ys) ->
        let
            start = Text.head ys
            end = Text.tail ys
        in
            if start == x
                then (State end, Right start)
                else (State end, Left $ Error (SpecificChar x) (SpecificChar start))