module Parse where

import Prelude hiding (add, between, join)
import Control.Alt ((<|>))
import Control.Lazy (defer, fix)
import Data.Array (fromFoldable)
import Data.Function (on)
import Data.Identity (Identity)
import Data.List (List, many, groupBy, sortBy)
import Data.Map (values)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), uncurry)
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
import Expr (Def(..), Elim, Expr, Module(..), RawExpr(..), expr)
import PElim (PElim(..), class Joinable, join, toElim)
import Primitive (OpName(..), opNames, opPrec)
import Util (error)

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
   n <- token.natural
   pure $ expr $ Int $ f n

pair :: SParser Expr -> SParser Expr
pair expr' = token.parens $ do
   e1 <- expr'
   e2 <- token.comma *> expr'
   pure $ expr $ uncurry Pair (Tuple e1 e2)

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
lambda expr' = keyword strFun *> matches expr' <#> expr <<< Lambda

blah :: forall a . String -> Maybe a -> SParser a
blah msg Nothing = fail msg
blah _ (Just x) = pure x

matches :: SParser Expr -> SParser (Elim Expr)
matches expr' =
   (match expr' >>= blah "Incomplete branches" <<< toElim)
   <|>
   (do
      σs <- token.braces (sepBy1 (match expr') token.semi)
      blah "Incompatible or incomplete branches" (join σs >>= toElim))

match :: SParser Expr -> SParser (PElim Expr)
match expr' = do
   mkElim <- pattern
   ((token.reservedOp "->" *> expr' >>= pure <<< mkElim)
   <|>
   (matches expr' >>= pure <<< mkElim <<< expr <<< Lambda))

type MkElimParser = forall k . SParser (k -> PElim k)

-- TODO: anonymous variables
patternVar :: MkElimParser
patternVar = ident >>= pure <<< PElimVar

patternPair :: MkElimParser -> MkElimParser
patternPair pattern' = token.parens $ do
   mkElim1 <- pattern' <* token.comma
   mkElim2 <- pattern'
   pure $ PElimPair <<< mkElim1 <<< mkElim2

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
   e <- token.reservedOp "=" *> expr' <* token.semi
   pure $ Def x e

let_ ∷ SParser Expr -> SParser Expr
let_ expr' = do
   d <- def expr'
   e <- expr'
   pure $ expr $ Let d e

-- any binary operator, in parentheses
parensOp :: SParser Expr
parensOp = token.parens $ token.operator <#> expr <<< Op

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

program ∷ SParser Expr
program = token.whiteSpace *> expr_ <* eof

module_ :: SParser Module
module_ = many (def expr_) <#> Module
