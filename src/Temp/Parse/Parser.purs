module Temp.Parse.Parser where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Data.Array (cons, elem)
import Data.Array as Array
import Data.Int (fromString, toNumber)
import Data.Maybe (maybe)
import Data.String.CodeUnits (fromCharArray)
import Data.String.CodeUnits as SCU
import Parsing (fail)
import Parsing.Combinators (try)
import Parsing.Combinators.Array (many, many1)
import Parsing.Indent (IndentParser, checkIndent, indented, withPos)
import Parsing.String (char)
import Parsing.String.Basic (alphaNum, digit, letter)
import Parsing.Token (oneOf)

type Parser a = IndentParser String a

keywords :: Array String
keywords = [ "def", "if", "else" ]

block :: forall a. Parser a -> Parser a
block e = delim ':' *> ((lines1 *> spaces *> indented *> withPos e) <|> e)

parens :: forall a. Parser a -> Parser a
parens e = delim '(' *> e <* delim ')'

align :: forall a. Parser a -> Parser a
align p = lines1 *> spaces *> checkIndent *> p

-- Identifiers and keywords
identifier :: Parser String
identifier = lexeme $ do
   first <- letter <|> char '_'
   rest <- many (alphaNum <|> oneOf [ '_', '\'' ])
   pure $ fromCharArray (cons first rest)

unreserved :: Parser String
unreserved = do
   name <- identifier
   if name `elem` keywords then fail $ "Reserved identifier: " <> name
   else pure name

reserved :: String -> Parser Unit
reserved expected = try do
   received <- identifier
   if expected /= received then fail $ "Expected `" <> expected <> "`, received `" <> received <> "`"
   else pure unit

delim :: Char -> Parser Unit
delim c = void $ lexeme $ char c

spaces :: Parser Unit
spaces = void $ many (oneOf [ ' ', '\t' ])

lines :: Parser Unit
lines = void $ many (spaces *> newline)

lines1 :: Parser Unit
lines1 = void $ many1 (spaces *> newline)

whitespace :: Parser Unit
whitespace = void $ many (oneOf [ ' ', '\t', '\n' ])

lexeme :: forall a. Parser a -> Parser a
lexeme p = p <* spaces

newline :: Parser Unit
newline = void $ char '\n'

-- type Parser s = IndentParser String s

-- whitespace :: Parser Unit
-- whitespace = skipMany (oneOf [ ' ', '\t', '\n' ])

-- eof :: Parser Unit
-- eof = optional anyChar >>= maybe (pure unit) (\_ -> fail "Expected EOF")

-- parens :: forall s. Parser s -> Parser s
-- parens p = between (symbol "(") (symbol ")") p

-- symbol :: String -> Parser String
-- symbol name = lexeme (string name)

-- lexeme :: forall a. Parser a -> Parser a
-- lexeme p = p <* whitespace

-- comma :: Parser String
-- comma = symbol ","

-- colon :: Parser String
-- colon = symbol ":"

-- reserved :: Array String
-- reserved = [ keyword.def ]

-- isReserved :: String -> Boolean
-- isReserved name = Array.elem name reserved

-- unreserved :: Parser String -> Parser String
-- unreserved p = do
--    name <- p
--    if (isReserved name) then fail ("reserved word " <> show name)
--    else pure name

-- ident :: Parser Char -> Parser Char -> Parser String
-- ident start letter = lexeme $ do
--    c <- start
--    cs <- Array.many letter
--    pure $ SCU.singleton c <> SCU.fromCharArray cs

-- var :: Parser String
-- var = unreserved $ ident (lower <|> char '_') (alphaNum <|> oneOf [ '_', '\'' ])

-- constr :: Parser String
-- constr = unreserved $ ident upper (alphaNum <|> oneOf [ '_', '\'' ])

integer :: Parser Int
integer = do
   f <- lexeme sign
   n <- number
   pure $ f n

floating :: Parser Number
floating = do
   f <- lexeme sign
   n <- toNumber <$> number
   _ <- char '.'
   n' <- toNumber <$> number
   pure $ f (n + (n' / 10.0))

number :: Parser Int
number = do
   digits <- Array.some digit
   maybe (fail "not digits") pure $ fromString (SCU.fromCharArray digits)

sign :: forall a. (Ring a) => Parser (a -> a)
sign = (char '-' $> negate)
   <|> (char '+' $> identity)
   <|> pure identity
