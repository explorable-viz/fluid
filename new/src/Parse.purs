module Parse where

import Prelude hiding (add, between, join)
import Control.Alt ((<|>))
import Control.Lazy (defer, fix)
import Data.Array (fromFoldable)
import Data.Function (on)
import Data.Identity (Identity)
import Data.List (many, groupBy, singleton, sortBy)
import Data.Map (values)
import Data.Maybe (Maybe(..))
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
import Expr (Def(..), Elim, Expr, Module(..), RawExpr(..), RecDef(..), RecDefs, expr)
import PElim (PElim(..), join, toElim)
import Primitive (OpName(..), opNames, opPrec)
import Util (fromBool)

type SParser = Parser String

-- constants (should also be used by prettyprinter)
strFun = "fun" :: String
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
   reservedNames   = [strFun, strLet],
   caseSensitive   = true
} where
   op' :: SParser Char
   op' = oneOf [':', '!', '#', '$', '%', '&', '*', '+', '.', '/', '<', '=', '>', '?', '@', '\\', '^', '|', '-', '~']

token :: TokenParser
token = makeTokenParser languageDef

keyword ∷ String → SParser Unit
keyword = token.reserved

variable :: SParser Expr
variable = ident <#> expr <<< Var

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
   token.natural <#> f >>> Int >>> expr

pair :: SParser Expr -> SParser Expr
pair expr' = token.parens $ do
   e1 <- expr'
   token.comma *> expr' <#> Pair e1 >>> expr

-- TODO: string, float, list
simpleExpr :: SParser Expr -> SParser Expr
simpleExpr expr' =
   variable <|>
   try (let_ expr') <|>
   letRec expr' <|>
   try int <|> -- int may start with +/-
   try (token.parens expr') <|>
   try parensOp <|>
   pair expr' <|>
   lambda expr'

lambda :: SParser Expr -> SParser Expr
lambda expr' = keyword strFun *> elim expr' <#> Lambda >>> expr

maybePure :: forall a . String -> Maybe a -> SParser a
maybePure msg Nothing = fail msg
maybePure _ (Just x) = pure x

elim :: SParser Expr -> SParser (Elim Expr)
elim expr' =
   (partialElim expr' >>= maybePure "Incomplete branches" <<< toElim)
   <|>
   (do
      σs <- token.braces (sepBy1 (partialElim expr') token.semi)
      maybePure "Incompatible or incomplete branches" (join σs >>= toElim))

partialElim :: SParser Expr -> SParser (PElim Expr)
partialElim expr' = do
   mkElim <- pattern
   ((token.reservedOp "->" *> expr' <#> mkElim)
   <|>
   (elim expr' <#> Lambda >>> expr >>> mkElim))

type MkElimParser = forall k . SParser (k -> PElim k)

-- TODO: anonymous variables
patternVar :: MkElimParser
patternVar = ident <#> PElimVar

patternPair :: MkElimParser -> MkElimParser
patternPair pattern' = token.parens $ do
   mkElim1 <- pattern' <* token.comma
   mkElim2 <- pattern'
   pure $ mkElim2 >>> mkElim1 >>> PElimPair

-- TODO: lists
pattern :: MkElimParser
pattern = fixParser (\p -> patternVar <|> patternPair p)

-- fix isn't polymorphic enough
fixParser :: (MkElimParser -> MkElimParser) -> MkElimParser
fixParser f = x
   where
   -- type annotation and parentheses are essential :-o
   x = (defer \_ -> f x) :: MkElimParser

def :: SParser Expr -> SParser Def
def expr' = do
   x <- keyword strLet *> ident
   token.reservedOp "=" *> (expr' <#> Def x) <* token.semi

recDefs :: SParser Expr -> SParser RecDefs
recDefs expr' = do
   f <- keyword strLet *> ident
   (matches expr' <#> RecDef f >>> singleton) <* token.semi

let_ ∷ SParser Expr -> SParser Expr
let_ expr' = do
   d <- def expr'
   expr' <#> Let d >>> expr

letRec :: SParser Expr -> SParser Expr
letRec expr' = do
   δ <- recDefs expr'
   expr' <#> LetRec δ >>> expr

-- any binary operator, in parentheses
parensOp :: SParser Expr
parensOp = token.parens $ token.operator <#> Op >>> expr

-- the specific binary operator
theBinaryOp :: Var -> SParser (Expr -> Expr -> Expr)
theBinaryOp op = try $ do
   op' <- token.operator
   maybePure ("Expected " <> op) $
      fromBool (op == op') (\e1 e2 -> expr $ BinaryApp e1 op e2)

backtick :: SParser Unit
backtick = token.reservedOp "`"

appChain ∷ SParser Expr -> SParser Expr
appChain expr' = simpleExpr expr' >>= rest
   where
      rest ∷ Expr -> SParser Expr
      rest e = (simpleExpr expr' <#> App e >>> expr >>= rest) <|> pure e

-- each element of the top-level list corresponds to a precedence level
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

topLevel :: forall a . SParser a -> SParser a
topLevel p = token.whiteSpace *> p <* eof

program ∷ SParser Expr
program = topLevel expr_

module_ :: SParser Module
module_ = topLevel $ many (def expr_) <#> Module
