
module Parse where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Text.Parsing.Parser (Parser, ParserT)
import Text.Parsing.Parser.Combinators (between)
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.String (oneOf, string)
import Text.Parsing.Parser.Token (
  GenLanguageDef(..), LanguageDef, TokenParser,
  alphaNum, letter, makeTokenParser, unGenLanguageDef
)
import Expr (Expr(..))

parens :: forall m a. Monad m => ParserT String m a -> ParserT String m a
parens = between (string "(") (string ")")

languageDef :: LanguageDef
languageDef = LanguageDef (unGenLanguageDef emptyDef)
                { commentStart    = "{-"
                , commentEnd      = "-}"
                , commentLine     = "--"
                , nestedComments  = true
                , identStart      = letter
                , identLetter     = alphaNum <|> oneOf ['_', '\'']
                , opStart         = op'
                , opLetter        = op'
                , reservedOpNames = []
                , reservedNames   = []
                , caseSensitive   = true
                }
  where
    op' :: forall m . (Monad m) => ParserT String m Char
    op' = oneOf [':', '!', '#', '$', '%', '&', '*', '+', '.', '/', '<', '=', '>', '?', '@', '\\', '^', '|', '-', '~']

tokenParser :: TokenParser
tokenParser = makeTokenParser languageDef

variable :: Parser String Expr
variable = do
  x <- tokenParser.identifier
  pure $ ExprVar x

simpleExpr :: Parser String Expr
simpleExpr = variable
{-   variable {% id %} |
   string {% id %} |
   number {% id %} |
   parenthExpr {% id %} |
   pair {% id %} |
   list {% id %} |
   constr {% id %}
-}
