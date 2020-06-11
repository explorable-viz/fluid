module Parse where

import Prelude hiding (absurd, add, between, join)
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Control.MonadPlus (empty)
import Data.Array (fromFoldable)
import Data.Char.Unicode (isUpper)
import Data.Either (choose)
import Data.Function (on)
import Data.Identity (Identity)
import Data.List (many, groupBy, sortBy)
import Data.Map (values)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (charAt)
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
import Elim (Elim)
import Expr (Def(..), Expr, Module(..), RawExpr(..), RecDef(..), RecDefs, expr)
import PElim (PElim(..), join, singleBranch, toElim)
import Primitive (OpName(..), opNames, opPrec)
import Util (absurd, error, fromBool)

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
   commentStart = "{-",
   commentEnd = "-}",
   commentLine = "--",
   nestedComments = true,
   identStart = letter,
   identLetter = alphaNum <|> oneOf ['_', '\''],
   opStart = opChar,
   opLetter = opChar,
   reservedOpNames = [],
   reservedNames = [strAs, strFun, strLet, strMatch],
   caseSensitive = true
} where
   opChar :: SParser Char
   opChar = oneOf [
      ':', '!', '#', '$', '%', '&', '*', '+', '.', '/', '<', '=', '>', '?', '@', '\\', '^', '|', '-', '~'
   ]

token :: TokenParser
token = makeTokenParser languageDef

keyword ∷ String → SParser Unit
keyword = token.reserved

variable :: SParser Expr
variable = ident <#> Var >>> expr

-- TODO: anonymous variables
patternVariable :: SParser (PElim Unit)
patternVariable = ident <#> flip PElimVar unit

-- Distinguish constructors from identifiers syntactically, a la Haskell. In particular this is useful
-- for distinguishing pattern variables from nullary constructors when parsing patterns.
isCtr ∷ String → Boolean
isCtr str = case charAt 0 str of
   Nothing -> error absurd
   Just ch -> isUpper ch

ident ∷ SParser Var
ident = do
   x <- token.identifier
   pureIf ("Unexpected constructor") (not (isCtr x)) x

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
true_ = ctr cTrue $> expr True

patternTrue :: SParser (PElim Unit)
patternTrue = ctr cTrue $> PElimTrue unit

false_ :: SParser Expr
false_ = ctr cFalse $> expr False

patternFalse :: SParser (PElim Unit)
patternFalse = ctr cFalse $> PElimFalse unit

nil :: SParser Expr
nil = ctr cNil $> expr Nil

patternNil :: SParser (PElim Unit)
patternNil = ctr cNil $> PElimNil unit

cons :: SParser Expr -> SParser Expr
cons expr' = do
   e <- ctr cCons *> simpleExpr expr'
   simpleExpr expr' <#> Cons e >>> expr

patternCons :: SParser (PElim Unit) -> SParser (PElim Unit)
patternCons pattern' = do
   σ <- ctr cCons *> simplePattern pattern'
   simplePattern pattern' <#> const >>> (<#>) σ >>> PElimCons

constr :: SParser Expr -> SParser Expr
constr expr' =
   try false_ <|>
   try true_ <|>
   try nil

pair :: SParser Expr -> SParser Expr
pair expr' =
   token.parens $ do
      e1 <- expr' <* token.comma
      expr' <#> Pair e1 >>> expr

patternPair :: SParser (PElim Unit) -> SParser (PElim Unit)
patternPair pattern' =
   token.parens $ do
      σ <- pattern' <* token.comma
      pattern' <#> const >>> (<#>) σ >>> PElimPair

-- TODO: float
simpleExpr :: SParser Expr -> SParser Expr
simpleExpr expr' =
   try variable <|>
   try true_ <|>
   try false_ <|>
   try nil <|>
   try int <|> -- int may start with +/-
   string <|>
   let_ expr' <|>
   letRec expr' <|>
   matchAs expr' <|>
   try (token.parens expr') <|>
   try parensOp <|>
   pair expr' <|>
   lambda expr'

simplePattern :: SParser (PElim Unit) -> SParser (PElim Unit)
simplePattern pattern' =
   try patternVariable <|>
   try patternTrue <|>
   try patternFalse <|>
   try patternNil <|>
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
elim expr' nest = elimSingle expr' nest <|> elimBraces expr' nest

elimSingle :: SParser Expr -> Boolean -> SParser (Elim Expr)
elimSingle expr' nest = do
   σ <- partialElim expr' nest (arrow <|> equals)
   case toElim σ of
      Nothing -> error "Incomplete branches"
      Just σ' -> pure σ'

elimBraces :: SParser Expr -> Boolean -> SParser (Elim Expr)
elimBraces expr' nest =
   token.braces $ do
      σs <- sepBy1 (partialElim expr' nest arrow) token.semi
      case join σs of
         Nothing -> error "Incompatible branches"
         Just σ -> case toElim σ of
            Nothing -> error "Incomplete branches"
            Just σ' -> pure σ'

nestedFun :: Boolean -> SParser Expr -> SParser Expr
nestedFun true expr' = elim expr' true <#> Lambda >>> expr
nestedFun false _ = empty

partialElim :: SParser Expr -> Boolean -> SParser Unit -> SParser (PElim Expr)
partialElim expr' nest delim = do
   σ <- pattern
   (delim *> expr' <|> nestedFun nest expr') <#> const >>> (<#>) σ

def :: SParser Expr -> SParser Def
def expr' = do
   σ <- try $ keyword strLet *> elim expr' false <* token.semi
   pureMaybe "Singleton eliminator expected" $ singleBranch σ <#> Def (σ $> unit)

let_ ∷ SParser Expr -> SParser Expr
let_ expr' = expr <$> (Let <$> def expr' <*> expr')

recDef :: SParser Expr -> SParser RecDef
recDef expr' = RecDef <$> ident <*> (elim expr' true <* token.semi)

recDefs :: SParser Expr -> SParser RecDefs
recDefs expr' = keyword strLet *> many (try $ recDef expr')

letRec :: SParser Expr -> SParser Expr
letRec expr' = expr <$>
   (LetRec <$> recDefs expr' <*> expr')

matchAs :: SParser Expr -> SParser Expr
matchAs expr' = expr <$>
   (MatchAs <$> (keyword strMatch *> expr' <* keyword strAs) <*> elim expr' false)

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
appChain expr' =
   (simpleExpr expr' >>= rest) <|> cons expr'
   where
      rest ∷ Expr -> SParser Expr
      rest e = (simpleExpr expr' <#> App e >>> expr >>= rest) <|> pure e

-- Singleton eliminator. Will be more similar to appChain when we have arbitrary data types.
appChain_pattern :: SParser (PElim Unit) -> SParser (PElim Unit)
appChain_pattern pattern' =
   simplePattern pattern' <|> patternCons pattern'

-- TODO: allow infix constructors, via buildExprParser
pattern :: SParser (PElim Unit)
pattern = fix appChain_pattern

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
