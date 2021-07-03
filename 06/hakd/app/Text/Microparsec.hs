{-# LANGUAGE OverloadedStrings, LambdaCase, TupleSections #-}

module Text.Microparsec
    ( Parser
    , anyChar
    , end)
    where

import qualified Data.Text as Text
import Data.Text (Text)
import Data.String (IsString)
import Data.Bifunctor

data ParseableChars = SpecificChar Char
                    | SpecificText Text
                    | AnyText
                    | NoText
                    | SatisfiesDescription Text

data Error = Error
    { expected :: ParseableChars
    , found :: ParseableChars
    }

newtype State = State
    { remaining :: Text }

newtype Parser a = Parser
    { runParser :: State -> (State, Either Error a)
    }

instance Functor Parser where
    fmap f (Parser g) = Parser $ \s ->
        case g s of
            (s', Left err) -> (s', Left err)
            (s', Right x) -> (s', Right $ f x)

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
    (State "") -> (State "", Left $ Error AnyText NoText)
    (State xs) -> (State $ Text.tail xs, Right $ Text.head xs)

anyText :: Parser Text
anyText = Parser $ \case
    (State "") -> (State "", Left $ Error AnyText NoText)
    (State xs) -> (State "", Right xs)

end :: Parser ()
end = Parser $ \state ->
    case state of
        State "" -> (state, Right ())
        State xs -> (state, Left $ Error NoText (SpecificText xs))

parseError :: Error -> Parser a
parseError err = Parser (, Left err)

try :: Parser a -> Parser a
try p = Parser $ \state ->
    case runParser p state of
        (_, Left err) -> (state, Left err)
        success -> success

satisfyChar :: Text -> (Char -> Bool) -> Parser Char
satisfyChar description predicate = do
    c <- anyChar
    if predicate c
        then pure c
        else parseError $ Error (SatisfiesDescription description) (SpecificChar c)

-- satisfyText :: Text -> (Text -> Bool) -> Parser Text
-- satisfyText description predicate = do


char :: Char -> Parser Char
char c = satisfyChar (Text.singleton c) (== c)

-- text :: Text -> Parser Text
-- text xs = 