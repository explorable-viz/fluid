module Parse.Parser where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Data.Array (cons, elem)
import Data.Array as Array
import Data.List.Types (List, NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (take)
import Data.String.CodeUnits as SCU
import Data.Traversable (foldr)
import Parse.Constants (opChars)
import Parsing (ParseError(..), Position(..), fail, position, region)
import Parsing.Combinators (between, sepBy, sepBy1, skipMany, try, (<?>))
import Parsing.Combinators.Array (many)
import Parsing.Indent (IndentParser, checkIndent, sameOrIndented, withPos)
import Parsing.String (char, satisfy, string)
import Parsing.String.Basic (alphaNum, letter, lower, upper)
import Parsing.Token (oneOf)

type Parser a = IndentParser String a

keywords :: Array String
keywords = [ "def", "if", "else", "lambda", "match", "case", "for", "in" ]

context :: forall a. String -> Parser a -> Parser a
context s p = do
   pos <- position
   region (addContext pos) p
   where
   addContext :: Position -> ParseError -> ParseError
   addContext (Position { line, column }) (ParseError msg pos) =
      ParseError (take 200 (msg <> "\n " <> s <> " on line " <> show line <> ", column " <> show column)) pos

block :: forall a. Parser a -> Parser a
block e = delim ':' *> whitespace *> sameOrIndented *> withPos e

align :: forall a. Parser a -> Parser a
align p = whitespace *> checkIndent *> p

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

operator :: Parser String
operator = lexeme $ do
   cs <- Array.some $ oneOf opChars
   pure $ SCU.fromCharArray cs

reservedOperator :: String -> Parser Unit
reservedOperator expected = try do
   received <- operator
   if expected /= received then fail $ "Expected `" <> expected <> "`, received `" <> received <> "`"
   else pure unit

delim :: Char -> Parser Unit
delim c = char c *> whitespace

lexeme :: forall a. Parser a -> Parser a
lexeme p = p <* whitespace

token :: String -> Parser Unit
token t = void $ lexeme $ string t

whitespace :: Parser Unit
whitespace = skipMany (space <|> comment)
   where
   space = void $ oneOf [ ' ', '\t', '\n' ]
   comment = char '#' *> skipMany (satisfy (_ /= '\n'))

braces :: forall a. Parser a -> Parser a
braces = between (delim '{') (delim '}')

brackets :: forall a. Parser a -> Parser a
brackets = between (delim '[') (delim ']')

parens :: forall a. Parser a -> Parser a
parens = between (delim '(') (delim ')')

commas :: forall a. Parser a -> Parser (List a)
commas p = sepBy p (delim ',')

commas1 :: forall a. Parser a -> Parser (NonEmptyList a)
commas1 p = sepBy1 p (delim ',')

-----------------------------------------------------------
-- String things extracted from "Parsing.Token"
-----------------------------------------------------------
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
