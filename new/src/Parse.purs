module Parse where

import Prelude hiding (add, between)
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (fromFoldable)
import Data.Function (on)
import Data.Identity (Identity)
import Data.List (groupBy, sortBy)
import Data.Map (values)
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.Combinators (sepBy1, try)
import Text.Parsing.Parser.Expr (Assoc(..), Operator(..), OperatorTable, buildExprParser)
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.String (char, eof, oneOf)
import Text.Parsing.Parser.Token (
  GenLanguageDef(..), LanguageDef, TokenParser,
  alphaNum, letter, makeTokenParser, unGenLanguageDef
)
import Bindings (Var)
import Expr (Elim(..), Expr, RawExpr(..), expr)
import Primitive (OpName(..), opNames, opPrec)
import Util (error, todo)

type SParser = Parser String

-- constants (should also be used by prettyprinter)
strFun = "fun" :: String
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
variable = do
   x <- ident
   pure $ expr $ Var x

-- Need to resolve constructors vs. variables (https://github.com/explorable-viz/fluid/issues/49)
ident ∷ SParser Var
ident = token.identifier

sign :: ∀ a . (Ring a) => SParser (a -> a)
sign =
   (char '-' $> negate) <|>
   (char '+' $> identity) <|>
   pure identity

int :: SParser Expr
int = do
   f <- sign
   n <- token.natural
   pure $ expr $ Int $ f n

pair :: SParser Expr -> SParser Expr
pair expr' = token.parens $ do
   e1 <- expr'
   e2 <- token.comma *> expr'
   pure $ expr $ Pair e1 e2

-- TODO: string, float, list
simpleExpr :: SParser Expr -> SParser Expr
simpleExpr expr' =
   variable <|>
   let_ expr' <|>
   try int <|> -- int may start with +/-
   try (token.parens expr') <|>
   try parensOp <|>
   pair expr' <|>
   lambda expr'

lambda :: SParser Expr -> SParser Expr
lambda expr' = do
   σ <- keyword strFun *> matches expr'
   pure $ expr $ Lambda σ

matches :: SParser Expr -> SParser (Elim Expr)
matches expr' = match expr' <|> token.braces (blah expr')

blah :: SParser Expr -> SParser (Elim Expr)
blah expr' = sepBy1 (match expr') token.semi
-- match (lexeme[";"] match {% ([, m]) => m %}):*

match :: SParser Expr -> SParser (Elim Expr)
match expr' =
   do
      mkElim <- pattern
      token.reservedOp "->" *> expr' >>= pure <<< mkElim
   <|>
   do
      mkElim <- pattern
      σ <- matches expr'
      pure $ mkElim $ expr $ Lambda σ

-- TODO: anonymous variables
patternVar :: forall k . SParser (k -> Elim k)
patternVar = ident >>= pure <<< ElimVar

patternPair :: forall k . (forall k . SParser (k -> Elim k)) -> SParser (k -> Elim k)
patternPair pattern' = token.parens $ do
   mkElim1 <- pattern' <* token.comma
   mkElim2 <- pattern'
   pure $ ElimPair <<< mkElim1 <<< mkElim2

-- TODO: pairs, lists
pattern :: forall k . SParser (k -> Elim k)
pattern = fix patternPair

let_ ∷ SParser Expr -> SParser Expr
let_ term' = do
   x <- keyword strLet *> ident
   e1 <- token.reservedOp "=" *> term'
   e2 <- keyword strIn *> term'
   pure $ expr $ Let x e1 e2

-- any binary operator, in parentheses
parensOp :: SParser Expr
parensOp = token.parens $ do
   op <- token.operator
   pure $ expr $ Op op

-- the specific binary operator
theBinaryOp :: Var -> SParser (Expr -> Expr -> Expr)
theBinaryOp op = try $ do
   op' <- token.operator
   if (op /= op')
   then fail $ "Expected " <> op
   else pure $ (\e1 e2 -> expr $ BinaryApp e1 op e2)

backtick :: SParser Unit
backtick = token.reservedOp "`"

appChain ∷ SParser Expr -> SParser Expr
appChain expr' = do
   simpleExpr expr' >>= rest
   where
      rest ∷ Expr -> SParser Expr
      rest e1 = (simpleExpr expr' >>= (pure <<< expr <<< App e1) >>= rest) <|> pure e1

-- each element of the top-level list corresponds to a precedence level.
operators :: OperatorTable Identity String Expr
operators =
   fromFoldable $ map fromFoldable $
   map (map (\(OpName { op }) -> Infix (theBinaryOp op) AssocLeft)) $
   groupBy (eq `on` opPrec) $ sortBy (comparing opPrec) $ values opNames

-- An expression is an operator tree. An operator tree is a tree whose branches are
-- binary primitives and whose leaves are application chains. An application chain
-- is a left-associative tree of one or more simple terms. A simple term is any
-- expression other than an operator tree or an application chain.
expr_ :: SParser Expr
expr_ = fix $ \p -> flip buildExprParser (appChain p) operators

program ∷ SParser Expr
program = token.whiteSpace *> expr_ <* eof
