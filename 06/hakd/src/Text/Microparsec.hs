{-# LANGUAGE LambdaCase, TupleSections, DerivingStrategies #-}

module Text.Microparsec
    ( Parser(..)
    , Error(..)
    , State(..)
    , ParseableChars(..)
    , anyChar
    , end
    , parseError
    , try
    , satisfyChar
    , char
    , string)
    where

import Control.Applicative ( Alternative(..) )
import Control.Monad ( MonadPlus(..) )

data ParseableChars = SpecificChar Char
                    | SpecificChars String
                    | AnyChars
                    | NoChars
                    | SatisfiesDescription String

data Error = Error
    { expected :: ParseableChars
    , found :: ParseableChars
    }

newtype State = State
    { remaining :: String }
    deriving stock (Eq)

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

instance Alternative Parser where
    empty = Parser (, Left (Error NoChars AnyChars))
    p1 <|> p2 = Parser $ \s -> case runParser p1 s of
        result@(s', Left _)
            | s == s' -> runParser p2 s
            | otherwise -> result
        
        success -> success

instance MonadPlus Parser where
    mzero = empty
    mplus = (<|>)

anyChar :: Parser Char
anyChar = Parser $ \case
    (State "") -> (State "", Left $ Error AnyChars NoChars)
    (State (x:xs)) -> (State xs, Right x)

end :: Parser ()
end = Parser $ \state ->
    case state of
        State "" -> (state, Right ())
        State xs -> (state, Left $ Error NoChars(SpecificChars xs))

parseError :: Error -> Parser a
parseError err = Parser (, Left err)

try :: Parser a -> Parser a
try p = Parser $ \state ->
    case runParser p state of
        (_, Left err) -> (state, Left err)
        success -> success

satisfyChar :: String -> (Char -> Bool) -> Parser Char
satisfyChar description predicate = do
    c <- anyChar
    if predicate c
        then pure c
        else parseError $ Error (SatisfiesDescription description) (SpecificChar c)

char :: Char -> Parser Char
char c = satisfyChar [c] (== c)

string :: String -> Parser String
string = traverse char