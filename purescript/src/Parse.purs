module Parse where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (between)
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.String (oneOf, string)
import Text.Parsing.Parser.Token (
  GenLanguageDef(..), LanguageDef, TokenParser,
  alphaNum, letter, makeTokenParser, unGenLanguageDef
)
import Expr (Expr(..), Var)

type SParser = Parser String

-- constants (should also be used by prettyprinter)
strIn = "in" :: String
strLet = "let" :: String
strLParen = "(" :: String
strRParen = "(" :: String

parens :: forall a . SParser a -> SParser a
parens = between (string strLParen) (string strRParen)

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
      op' :: SParser Char
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

int :: SParser Expr
int = tokenParser.integer >>= compose pure ExprInt

-- TODO: string, float
simpleExpr :: SParser Expr -> SParser Expr
simpleExpr expr' =
   variable <|>
   let_ expr' <|>
   int <|>
   parens expr'
{- pair {% id %} |
   list {% id %} |
   constr {% id %}
-}

let_ ∷ SParser Expr -> SParser Expr
let_ term' = do
   keyword strLet
   x ← ident
   e1 ← do
      tokenParser.reservedOp "="
      term'
   e2 ← do
      keyword strIn
      term'
   pure $ ExprLet x e1 e2

expr :: SParser Expr
expr = fix $ \p ->
   simpleExpr p
