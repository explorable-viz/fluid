module Parse where

import Prelude hiding (add, between, join)
import Control.Alt ((<|>))
import Control.Lazy (defer, fix)
import Control.MonadPlus (empty)
import Data.Array (fromFoldable)
import Data.Either (choose)
import Data.Foldable (notElem)
import Data.Function (on)
import Data.Identity (Identity)
import Data.List (many, groupBy, sortBy)
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
import PElim (PElim(..), join, singleBranch, toElim)
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
strArrow = "->" :: String
strAs = "as" :: String
strEquals = "=" :: String
strFun = "fun" :: String
strLet = "let" :: String
strMatch = "match" :: String

-- treat datatype-generically later
cFalse = "False" :: String
cTrue = "True" :: String
cNil = "Nil" :: String
cCons = "Cons" :: String

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
constructors = [cTrue, cFalse, cNil, cCons]

token :: TokenParser
token = makeTokenParser languageDef

keyword ∷ String → SParser Unit
keyword = token.reserved

variable :: SParser Expr
variable = ident <#> Var >>> expr

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
true_ = ctr cTrue <#> const (expr True)

false_ :: SParser Expr
false_ = ctr cFalse <#> const (expr False)

nil :: SParser Expr
nil = ctr cNil <#> const (expr Nil)

cons :: SParser Expr -> SParser Expr
cons expr' = do
   e <- ctr cCons *> expr'
   expr' <#> Cons e >>> expr

constr :: SParser Expr -> SParser Expr
constr expr' =
   try false_ <|>
   try true_ <|>
   try nil

pair :: SParser Expr -> SParser Expr
pair expr' = token.parens $ do
   e1 <- expr'
   token.comma *> expr' <#> Pair e1 >>> expr

-- TODO: float, list
simpleExpr :: SParser Expr -> SParser Expr
simpleExpr expr' =
   try variable <|>
   try int <|> -- int may start with +/-
   string <|>
   constr expr' <|>
   let_ expr' <|>
   letRec expr' <|>
   matchAs expr' <|>
   try (token.parens expr') <|>
   try parensOp <|>
   pair expr' <|>
   lambda expr'

simplePattern :: MkElimParser -> MkElimParser
simplePattern pattern' =
   patternVar <|>
   try (token.parens pattern') <|>
   patternPair pattern'

lambda :: SParser Expr -> SParser Expr
lambda expr' = keyword strFun *> elim expr' true <#> Lambda >>> expr

arrow :: SParser Unit
arrow = token.reservedOp strArrow

equals :: SParser Unit
equals = token.reservedOp strEquals

-- "nest" controls whether nested (curried) functions are permitted in this context
elim :: SParser Expr -> Boolean -> SParser (Elim Expr)
elim expr' nest =
   pureMaybe "Incompatible or incomplete branches" =<<
   (partialElim expr' nest (arrow <|> equals) <#> toElim) <|>
   (token.braces $ sepBy1 (partialElim expr' nest arrow) token.semi <#> (join >=> toElim))

nestedFun :: Boolean -> SParser Expr -> SParser Expr
nestedFun true expr' = elim expr' true <#> Lambda >>> expr
nestedFun false _ = empty

partialElim :: SParser Expr -> Boolean -> SParser Unit -> SParser (PElim Expr)
partialElim expr' nest delim = do
   mkElim <- pattern
   (delim *> expr' <|> nestedFun nest expr') <#> mkElim

type MkElimParser = forall k . SParser (k -> PElim k)

-- TODO: anonymous variables
patternVar :: MkElimParser
patternVar = ident <#> PElimVar

patternPair :: MkElimParser -> MkElimParser
patternPair pattern' =
   token.parens $ do
      mkElim1 <- pattern' <* token.comma
      mkElim2 <- pattern'
      pure $ mkElim2 >>> mkElim1 >>> PElimPair

patternTrue :: MkElimParser
patternTrue = ctr cTrue <#> const PElimTrue

patternFalse :: MkElimParser
patternFalse = ctr cFalse <#> const PElimFalse

-- TODO: lists
pattern :: MkElimParser
pattern = fixParser (\p ->
   try patternVar <|>
   try patternTrue <|>
   try patternFalse <|>
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
   σ <- try $ keyword strLet *> elim expr' false <* token.semi
   pureMaybe "Singleton eliminator expected" $ singleBranch σ <#> Def (σ <#> const unit)

let_ ∷ SParser Expr -> SParser Expr
let_ expr' = do
   d <- def expr'
   expr' <#> Let d >>> expr

recDef :: SParser Expr -> SParser RecDef
recDef expr' = do
   f <- ident
   (elim expr' true <#> RecDef f) <* token.semi

recDefs :: SParser Expr -> SParser RecDefs
recDefs expr' =
   keyword strLet *> many (try $ recDef expr')

letRec :: SParser Expr -> SParser Expr
letRec expr' = do
   δ <- recDefs expr'
   expr' <#> LetRec δ >>> expr

matchAs :: SParser Expr -> SParser Expr
matchAs expr' = do
   e <- keyword strMatch *> expr' <* keyword strAs
   elim expr' false <#> MatchAs e >>> expr

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
   map (map (\(OpName op _) -> Infix (theBinaryOp op) AssocLeft)) $
   groupBy (eq `on` opPrec) $ sortBy (comparing opPrec) $ values opNames

-- An expression is an operator tree. An operator tree is a tree whose branches are
-- binary primitives and whose leaves are application chains. An application chain
-- is a left-associative tree of one or more simple terms. A simple term is any
-- expression other than an operator tree or an application chain.
expr_ :: SParser Expr
expr_ = fix $ appChain >>> buildExprParser operators

topLevel :: forall a . SParser a -> SParser a
topLevel p = token.whiteSpace *> p <* eof

program ∷ SParser Expr
program = topLevel expr_

module_ :: SParser Module
module_ = topLevel $ many (choose (def expr_) (recDefs expr_)) <#> Module
