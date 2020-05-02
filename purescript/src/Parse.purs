module Parse where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Text.Parsing.Parser (Parser, ParserT)
import Text.Parsing.Parser.Combinators (between)
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.String (oneOf, string)
import Text.Parsing.Parser.Token (
  GenLanguageDef(..), LanguageDef, TokenParser,
  alphaNum, letter, makeTokenParser, unGenLanguageDef
)
import Expr (Expr(..), Var)

type SParser = Parser String

-- constants
strIn = "in" :: String
strLet = "let" :: String

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

keyword ∷ String → SParser Unit
keyword = tokenParser.reserved

variable :: SParser Expr
variable = ident >>= compose pure ExprVar

-- Need to resolve constructors vs. variables (https://github.com/explorable-viz/fluid/issues/49)
ident ∷ SParser Var
ident = tokenParser.identifier

simpleExpr :: SParser Expr -> SParser Expr
simpleExpr expr' =
   variable <|> let_ expr'
{-   variable {% id %} |
   string {% id %} |
   number {% id %} |
   parenthExpr {% id %} |
   pair {% id %} |
   list {% id %} |
   constr {% id %}
-}

let_ ∷ SParser Expr -> SParser Expr
let_ term' = do
   keyword strLet
   x ← ident
   e1 ← do
      tokenParser.reservedOp "="
      term
   e2 ← do
      keyword strIn
      term'
   pure $ ExprLet x e1 e2

term :: SParser Expr
term = fix $ \p ->
   simpleExpr p
