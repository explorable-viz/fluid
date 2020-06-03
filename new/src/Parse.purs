module Parse where

import Prelude hiding (add, between, join)
import Control.Alt ((<|>))
import Control.Lazy (defer, fix)
import Data.Array (fromFoldable)
import Data.Foldable (notElem)
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

-- helpers
pureMaybe :: forall a . String -> Maybe a -> SParser a
pureMaybe msg Nothing = fail msg
pureMaybe _ (Just x) = pure x

pureIf :: forall a . String -> Boolean -> a -> SParser a
pureIf msg b = fromBool b >>> pureMaybe msg

-- constants (should also be used by prettyprinter)
strAs = "as" :: String
strFun = "fun" :: String
strLet = "let" :: String
strMatch = "match" :: String

-- treat datatype-generically later
cTrue = "True" :: String
cFalse = "False" :: String

languageDef :: LanguageDef
languageDef = LanguageDef (unGenLanguageDef emptyDef) {
   commentStart    = "{-",
   commentEnd      = "-}",
   commentLine     = "--",
   nestedComments  = true,
   identStart      = letter,
   identLetter     = alphaNum <|> oneOf ['_', '\''],
   opStart         = opChar,
   opLetter        = opChar,
   reservedOpNames = [],
   reservedNames   = [strAs, strFun, strLet, strMatch],
   caseSensitive   = true
} where
   opChar :: SParser Char
   opChar = oneOf [
      ':', '!', '#', '$', '%', '&', '*', '+', '.', '/', '<', '=', '>', '?', '@', '\\', '^', '|', '-', '~'
   ]

constructors :: Array String
constructors = [cTrue, cFalse]

token :: TokenParser
token = makeTokenParser languageDef

keyword ∷ String → SParser Unit
keyword = token.reserved

variable :: SParser Expr
variable = try $ ident <#> expr <<< Var

-- Need to resolve constructors vs. variables (https://github.com/explorable-viz/fluid/issues/49)
ident ∷ SParser Var
ident = do
   x <- token.identifier
   pureIf ("Unexpected constructor") (notElem x constructors) x

ctr :: String -> SParser String
ctr c = do
   x <- token.identifier
   pureIf ("Expected " <> c) (x == c) x

signOpt :: ∀ a . (Ring a) => SParser (a -> a)
signOpt =
   (char '-' $> negate) <|>
   (char '+' $> identity) <|>
   pure identity

int :: SParser Expr
int = do
   sign <- signOpt
   token.natural <#> sign >>> Int >>> expr

string :: SParser Expr
string = token.stringLiteral <#> Str >>> expr

true_ :: SParser Expr
true_ = try $ ctr cTrue <#> const (expr True)

false_ :: SParser Expr
false_ = try $ ctr cFalse <#> const (expr False)

pair :: SParser Expr -> SParser Expr
pair expr' = token.parens $ do
   e1 <- expr'
   token.comma *> expr' <#> Pair e1 >>> expr

-- TODO: float, list
simpleExpr :: SParser Expr -> SParser Expr
simpleExpr expr' =
   variable <|>
   try int <|> -- int may start with +/-
   string <|>
   false_ <|>
   true_ <|>
   let_ expr' <|>
   letRec expr' <|>
   matchAs expr' <|>
   try (token.parens expr') <|>
   try parensOp <|>
   pair expr' <|>
   lambda expr'

lambda :: SParser Expr -> SParser Expr
lambda expr' = keyword strFun *> elim expr' <#> Lambda >>> expr

elim :: SParser Expr -> SParser (Elim Expr)
elim expr' =
   (partialElim expr' >>= toElim >>> pureMaybe "Incomplete branches")
   <|>
   (do
      σs <- token.braces (sepBy1 (partialElim expr') token.semi)
      pureMaybe "Incompatible or incomplete branches" (join σs >>= toElim))

partialElim :: SParser Expr -> SParser (PElim Expr)
partialElim expr' = do
   mkElim <- pattern
   ((token.reservedOp "->" *> expr' <#> mkElim)
   <|>
   (elim expr' <#> Lambda >>> expr >>> mkElim))

type MkElimParser = forall k . SParser (k -> PElim k)

-- TODO: anonymous variables
patternVar :: MkElimParser
patternVar = try $ ident <#> PElimVar

patternPair :: MkElimParser -> MkElimParser
patternPair pattern' = token.parens $ do
   mkElim1 <- pattern' <* token.comma
   mkElim2 <- pattern'
   pure $ mkElim2 >>> mkElim1 >>> PElimPair

patternTrue :: MkElimParser
patternTrue = try $ ctr cTrue <#> const PElimTrue

patternFalse :: MkElimParser
patternFalse = try $ ctr cFalse <#> const PElimFalse

-- TODO: lists
pattern :: MkElimParser
pattern = fixParser (\p ->
   patternVar <|>
   patternTrue <|>
   patternFalse <|>
   patternPair p
)

-- fix isn't polymorphic enough
fixParser :: (MkElimParser -> MkElimParser) -> MkElimParser
fixParser f = x
   where
   -- type annotation and parentheses are essential :-o
   x = (defer \_ -> f x) :: MkElimParser

def :: SParser Expr -> SParser Def
def expr' = do
   x <- try $ do
      x <- keyword strLet *> ident
      token.reservedOp "="
      pure x
   (expr' <#> Def x) <* token.semi

let_ ∷ SParser Expr -> SParser Expr
let_ expr' = do
   d <- def expr'
   expr' <#> Let d >>> expr

recDefs :: SParser Expr -> SParser RecDefs
recDefs expr' = do
   f <- keyword strLet *> ident
   (elim expr' <#> RecDef f >>> singleton) <* token.semi

letRec :: SParser Expr -> SParser Expr
letRec expr' = do
   δ <- recDefs expr'
   expr' <#> LetRec δ >>> expr

matchAs :: SParser Expr -> SParser Expr
matchAs expr' = do
   e <- keyword strMatch *> expr' <* keyword strAs
   elim expr' <#> MatchAs e >>> expr

-- any binary operator, in parentheses
parensOp :: SParser Expr
parensOp = token.parens $ token.operator <#> Op >>> expr

-- the specific binary operator
theBinaryOp :: Var -> SParser (Expr -> Expr -> Expr)
theBinaryOp op = try $ do
   op' <- token.operator
   pureMaybe ("Expected " <> op) $
      fromBool (op == op') (\e1 -> expr <<< BinaryApp e1 op)

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
