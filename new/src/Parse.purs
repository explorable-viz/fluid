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
import Data.List ((:), many, groupBy, sortBy)
import Data.List (List(..)) as L
import Data.Map (singleton, values)
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
import DataType (Ctr(..), cCons, cFalse, cNil, cPair, cTrue)
import Expr (Cont(..), Def(..), Elim(..), Expr, Module(..), RawExpr(..), RecDef(..), RecDefs, expr)
import PElim (joinAll, mapCont)
import Primitive (OpName(..), opNames, opPrec)
import Util (absurd, error, fromBool, fromJust)

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
patternVariable :: SParser Elim
patternVariable = ElimVar <$> ident <@> CNone

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

ctr :: SParser Ctr
ctr = do
   x <- token.identifier
   pureIf ("Unexpected identifier") (isCtr x) $ Ctr x

-- Parse a constructor name as a nullary constructor pattern.
ctr_pattern :: SParser Elim
ctr_pattern = ElimConstr <$> (singleton <$> ctr <@> CNone)

theCtr :: Ctr -> SParser Ctr
theCtr c = do
   c' <- ctr
   pureIf ("Expected " <> show c) (c' == c) c

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

constr_pattern :: SParser Elim -> SParser Elim
constr_pattern pattern' = ctr_pattern >>= rest
   where
      rest ∷ Elim -> SParser Elim
      rest σ = do
         σ' <- simplePattern pattern' <|> ctr_pattern
         rest $ fromJust absurd $ mapCont (CElim σ') σ
         <|> pure σ

true_ :: SParser Expr
true_ = theCtr cTrue $> expr (Constr cTrue L.Nil)

false_ :: SParser Expr
false_ = theCtr cFalse $> expr (Constr cFalse L.Nil)

nil :: SParser Expr
nil = theCtr cNil $> expr (Constr cNil L.Nil)

cons :: SParser Expr -> SParser Expr
cons expr' = do
   e <- theCtr cCons *> simpleExpr expr'
   e' <- simpleExpr expr'
   pure $ expr $ Constr cCons (e : e' : L.Nil)

pair :: SParser Expr -> SParser Expr
pair expr' =
   token.parens $ do
      e <- expr' <* token.comma
      e' <- expr'
      pure $ expr $ Constr cPair (e : e' : L.Nil)

patternPair :: SParser Elim -> SParser Elim
patternPair pattern' =
   token.parens $ do
      σ <- pattern' <* token.comma
      τ <- pattern'
      pure $ ElimConstr $ singleton cPair $ CElim $ fromJust absurd $ mapCont (CElim τ) σ

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

simplePattern :: SParser Elim -> SParser Elim
simplePattern pattern' =
   try patternVariable <|>
   try (token.parens pattern') <|>
   patternPair pattern'

lambda :: SParser Expr -> SParser Expr
lambda expr' = keyword strFun *> elim expr' true <#> Lambda >>> expr

arrow :: SParser Unit
arrow = token.reservedOp strArrow

equals :: SParser Unit
equals = token.reservedOp strEquals

patternDelim :: SParser Unit
patternDelim = arrow <|> equals

-- "nest" controls whether nested (curried) functions are permitted in this context
elim :: SParser Expr -> Boolean -> SParser Elim
elim expr' nest =
   partialElim expr' nest patternDelim <|>
   elimBraces expr' nest

elimBraces :: SParser Expr -> Boolean -> SParser Elim
elimBraces expr' nest =
   token.braces $ do
      σs <- sepBy1 (partialElim expr' nest arrow) token.semi
      pure $ fromJust "Incompatible branches" $ joinAll σs

nestedFun :: Boolean -> SParser Expr -> SParser Expr
nestedFun true expr' = elim expr' true <#> Lambda >>> expr
nestedFun false _ = empty

partialElim :: SParser Expr -> Boolean -> SParser Unit -> SParser Elim
partialElim expr' nest delim = do
   σ <- pattern
   e <- delim *> expr' <|> nestedFun nest expr'
   pure $ fromJust absurd $ mapCont (CExpr e) σ

def :: SParser Expr -> SParser Def
def expr' = do
   Def <$> try (keyword strLet *> pattern <* patternDelim) <*> expr' <* token.semi

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

-- Singleton eliminator. Analogous in some way to app_chain, but there is nothing higher-order here:
-- there are no explicit application nodes, non-saturated constructor applications, or patterns other
-- than constructors in the function position.
appChain_pattern :: SParser Elim -> SParser Elim
appChain_pattern pattern' = simplePattern pattern' <|> constr_pattern pattern'

-- TODO: allow infix constructors, via buildExprParser
pattern :: SParser Elim
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
