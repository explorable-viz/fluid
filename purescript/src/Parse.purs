module Parse where

import Prelude hiding (add, between)
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (try)
import Text.Parsing.Parser.Expr (Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.String (eof, oneOf)
import Text.Parsing.Parser.Token (
  GenLanguageDef(..), LanguageDef, TokenParser,
  alphaNum, letter, makeTokenParser, unGenLanguageDef
)
import Bindings (Var)
import Expr (Expr(..))

type SParser = Parser String

-- constants (should also be used by prettyprinter)
strIn = "in" :: String
strLet = "let" :: String
strLParen = "(" :: String
strRParen = ")" :: String

languageDef :: LanguageDef
languageDef = LanguageDef (unGenLanguageDef emptyDef) {
   commentStart    = "{-",
   commentEnd      = "-}",
   commentLine     = "--",
   nestedComments  = true,
   identStart      = letter,
   identLetter     = alphaNum <|> oneOf ['_', '\''],
   opStart         = op',
   opLetter        = op',
   reservedOpNames = [],
   reservedNames   = [strIn, strLet],
   caseSensitive   = true
} where
   op' :: SParser Char
   op' = oneOf [':', '!', '#', '$', '%', '&', '*', '+', '.', '/', '<', '=', '>', '?', '@', '\\', '^', '|', '-', '~']

token :: TokenParser
token = makeTokenParser languageDef

keyword ∷ String → SParser Unit
keyword = token.reserved

variable :: SParser Expr
variable = ident >>= pure <<< Var

-- Need to resolve constructors vs. variables (https://github.com/explorable-viz/fluid/issues/49)
ident ∷ SParser Var
ident = token.identifier

int :: SParser Expr
int = token.integer >>= pure <<< Int

pair :: SParser Expr -> SParser Expr
pair expr' = token.parens $ do
   e1 <- expr
   e2 <- token.comma *> expr
   pure $ Pair e1 e2

-- TODO: string, float, list
simpleExpr :: SParser Expr -> SParser Expr
simpleExpr expr' =
   variable <|>
   let_ expr' <|>
   try int <|>
   token.parens expr' <|>
   pair expr'

let_ ∷ SParser Expr -> SParser Expr
let_ term' = do
   x <- keyword strLet *> ident
   e1 <- token.reservedOp "=" *> term'
   e2 <- keyword strIn *> term'
   pure $ Let x e1 e2

add ∷ SParser (Expr → Expr → Expr)
add = token.reservedOp "+" $> Add

appChain ∷ SParser Expr -> SParser Expr
appChain expr' = do
   simpleExpr expr' >>= rest
   where
      rest ∷ Expr -> SParser Expr
      rest e1 = (simpleExpr expr' >>= (pure <<< App e1) >>= rest) <|> pure e1

-- An expression is an operator tree. An operator tree is a tree whose branches are
-- binary primitives and whose leaves are application chains. An application chain
-- is a left-associative tree of one or more simple terms. A simple term is any
-- expression other than an operator tree or an application chain.
expr :: SParser Expr
expr = fix $ \p ->
   flip buildExprParser (appChain p) [
      -- each element of the top-level list corresponds to a precedence level.
      [Infix add AssocLeft]
   ]

program ∷ SParser Expr
program = token.whiteSpace *> expr <* eof
