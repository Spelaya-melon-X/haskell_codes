{-# LANGUAGE InstanceSigs #-}

module ParserDemo where

import Data.Char
import Control.Applicative


------------------------------------------------------------
-- БАЗОВЫЙ ТИП ПАРСЕРА
------------------------------------------------------------

newtype Parser tok a = Parser { runParser :: [tok] -> Maybe ([tok], a) }


------------------------------------------------------------
-- FUNCTOR
------------------------------------------------------------

instance Functor (Parser tok) where
    fmap :: (a -> b) -> Parser tok a -> Parser tok b
    fmap f (Parser p) = Parser $ \input ->
        case p input of
          Nothing -> Nothing
          Just (rest, x) -> Just (rest, f x)


------------------------------------------------------------
-- APPLICATIVE
------------------------------------------------------------

instance Applicative (Parser tok) where
    pure :: a -> Parser tok a
    pure x = Parser $ \input -> Just (input, x)

    (<*>) :: Parser tok (a -> b) -> Parser tok a -> Parser tok b
    Parser pf <*> Parser px = Parser $ \input ->
        case pf input of
          Nothing -> Nothing
          Just (rest1, f) ->
            case px rest1 of
              Nothing -> Nothing
              Just (rest2, x) -> Just (rest2, f x)


------------------------------------------------------------
-- ALTERNATIVE
------------------------------------------------------------

instance Alternative (Parser tok) where
    empty :: Parser tok a
    empty = Parser $ \_ -> Nothing

    (<|>) :: Parser tok a -> Parser tok a -> Parser tok a
    Parser p1 <|> Parser p2 = Parser $ \input ->
        case p1 input of
          Nothing -> p2 input
          res -> res


------------------------------------------------------------
-- БАЗОВЫЕ ПАРСЕРЫ
------------------------------------------------------------

satisfy :: (tok -> Bool) -> Parser tok tok
satisfy f = Parser $ \input ->
    case input of
      [] -> Nothing
      (c:cs) | f c -> Just (cs, c)
      _ -> Nothing

char :: Char -> Parser Char Char
char c = satisfy (== c)


digit :: Parser Char Int
digit = digitToInt <$> satisfy isDigit

lower :: Parser Char Char
lower = satisfy isLower

spaces :: Parser Char String
spaces = many $ satisfy isSpace


------------------------------------------------------------
-- КОМБИНАТОРЫ
------------------------------------------------------------

string :: String -> Parser Char String
string = traverse char

token :: Parser Char a -> Parser Char a
token p = p <* spaces

symbol :: Char -> Parser Char Char
symbol c = token (char c)


------------------------------------------------------------
-- ПАРСИНГ ЧИСЕЛ
------------------------------------------------------------

number :: Parser Char Int
number = token $ foldl (\acc d -> acc * 10 + d) 0 <$> some digit


------------------------------------------------------------
-- ВЫРАЖЕНИЯ
--
-- expr   = term (('+' term) | ('-' term))*
-- term   = factor (('*' factor) | ('/' factor))*
-- factor = number | '(' expr ')'
------------------------------------------------------------

expr :: Parser Char Int
expr = term `chainl1` addop

term :: Parser Char Int
term = factor `chainl1` mulop

factor :: Parser Char Int
factor =
        number
    <|> (symbol '(' *> expr <* symbol ')')


------------------------------------------------------------
-- ОПЕРАТОРЫ
------------------------------------------------------------

addop :: Parser Char (Int -> Int -> Int)
addop =
        (symbol '+' *> pure (+))
    <|> (symbol '-' *> pure (-))

mulop :: Parser Char (Int -> Int -> Int)
mulop =
        (symbol '*' *> pure (*))
    <|> (symbol '/' *> pure div)


------------------------------------------------------------
-- chainl1 без Monad (Applicative + Alternative)
------------------------------------------------------------

chainl1 :: Parser Char a -> Parser Char (a -> a -> a) -> Parser Char a
chainl1 p op = liftA2 apply p (many pair)
  where
    pair = liftA2 (,) op p
    apply x xs = foldl (\acc (f,y) -> f acc y) x xs


------------------------------------------------------------
-- УДОБНАЯ ФУНКЦИЯ: parse всю строку
------------------------------------------------------------

parse :: Parser Char a -> String -> Maybe a
parse p s =
    case runParser (spaces *> p <* spaces) s of
      Just ("", x) -> Just x   -- parsed fully
      _ -> Nothing


------------------------------------------------------------
-- ПРИМЕРЫ ДЛЯ GHCi
--
-- parse number "12345"
-- parse expr "1+2*3"
-- parse expr "(1+2)*3 - 10/2"
-- parse expr "2 + 3 * 4 * (10 - 8)"
-- parse expr "10/2/2"
------------------------------------------------------------
