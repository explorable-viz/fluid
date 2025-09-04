module Temp.Parse.Parser where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Data.Array (cons, elem)
import Data.Array as Array
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.String.CodeUnits as SCU
import Data.Traversable (foldr)
import Parsing (fail)
import Parsing.Combinators (between, try, (<?>))
import Parsing.Combinators.Array (many, many1)
import Parsing.Indent (IndentParser, checkIndent, indented, withPos)
import Parsing.String (char, satisfy)
import Parsing.String.Basic (alphaNum, digit, letter, lower, upper)
import Parsing.Token (oneOf)

type Parser a = IndentParser String a

keywords :: Array String
keywords = [ "def", "if", "else" ]

block :: forall a. Parser a -> Parser a
block e = delim ':' *> ((lines1 *> spaces *> indented *> withPos e) <|> e)

-- use between
parens :: forall a. Parser a -> Parser a
parens e = delim '(' *> e <* delim ')'

align :: forall a. Parser a -> Parser a
align p = lines1 *> spaces *> checkIndent *> p

identifier :: Parser Char -> Parser Char -> Parser String
identifier start letter = lexeme $ do
   c <- start
   cs <- Array.many letter
   pure $ SCU.singleton c <> SCU.fromCharArray cs

unreserved :: Parser String -> Parser String
unreserved p = do
   name <- p
   if name `elem` keywords then fail $ "Reserved identifier: " <> name
   else pure name

variable :: Parser String
variable = unreserved $ identifier (lower <|> char '_') (alphaNum <|> oneOf [ '_', '\'' ])

constructor :: Parser String
constructor = unreserved $ identifier upper (alphaNum <|> oneOf [ '_', '\'' ])

reserved :: String -> Parser Unit
reserved expected = try do
   received <- identifier (letter <|> char '_') (alphaNum <|> oneOf [ '_', '\'' ])
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

-- TODO: this is from Parsing.Token without string escapes
stringLiteral :: Parser String
stringLiteral = lexeme (go <?> "literal string")
   where
   go :: Parser String
   go = do
      maybeChars <- between (char '"') (char '"' <?> "end of string") (many stringChar)
      pure $ SCU.fromCharArray $ foldr folder [] maybeChars

   folder :: Maybe Char -> Array Char -> Array Char
   folder Nothing chars = chars
   folder (Just c) chars = cons c chars

stringChar :: Parser (Maybe Char)
stringChar =
   (Just <$> stringLetter)
      <?> "string character"

stringLetter :: Parser Char
stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\x1A'))
