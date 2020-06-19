module Parse where

import Prelude hiding (absurd, add, between, join)
import Control.Alt ((<|>))
import Control.Apply (lift2)
import Control.Lazy (fix)
import Control.MonadPlus (empty)
import Data.Array (fromFoldable)
import Data.Bitraversable (bisequence)
import Data.Char.Unicode (isUpper)
import Data.Either (Either, choose)
import Data.Function (on)
import Data.Identity (Identity)
import Data.List (List, (:), concat, foldr, many, groupBy, sortBy)
import Data.List (some) as L
import Data.List.NonEmpty (NonEmptyList, fromList, head, toList)
import Data.Map (values)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.NonEmpty ((:|))
import Data.Ordering (invert)
import Data.String.CodeUnits (charAt)
import Data.Tuple (fst, snd)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (manyTill, try)
import Text.Parsing.Parser.Combinators (sepBy1) as P
import Text.Parsing.Parser.Expr (Assoc(..), Operator(..), OperatorTable, buildExprParser)
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.String (char, eof, oneOf)
import Text.Parsing.Parser.Token (
  GenLanguageDef(..), LanguageDef, TokenParser,
  alphaNum, letter, makeTokenParser, unGenLanguageDef
)
import Bindings (Var)
import DataType (Ctr(..), cPair)
import Expr (Def(..), Elim, Expr(..), Module(..), RawExpr(..), RecDef(..), RecDefs, expr)
import PElim (Pattern(..), PCont(..), joinAll, mapCont, toElim)
import Primitive (OpName(..), opNames, opPrec)
import Util (type (×), (×), absurd, fromBool, fromJust)

type SParser = Parser String

-- helpers (could generalise further)
pureMaybe :: forall a . Maybe a -> SParser a
pureMaybe Nothing    = empty
pureMaybe (Just x)   = pure x

pureIf :: forall a . Boolean -> a -> SParser a
pureIf b = fromBool b >>> pureMaybe

sepBy1 :: forall a sep . SParser a -> SParser sep -> SParser (NonEmptyList a)
sepBy1 p sep = fromJust absurd <$> (fromList <$> P.sepBy1 p sep)

some :: forall a . SParser a → SParser (NonEmptyList a)
some p = fromJust absurd <$> (fromList <$> L.some p)

-- constants (should also be used by prettyprinter)
strArrow       = "->" :: String
strAs          = "as" :: String
strBackslash   = "\\" :: String
strEquals      = "=" :: String
strFun         = "fun" :: String
strLet         = "let" :: String
strMatch       = "match" :: String

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

-- Distinguish constructors from identifiers syntactically, a la Haskell. In particular this is useful
-- for distinguishing pattern variables from nullary constructors when parsing patterns.
isCtr ∷ String → Boolean
isCtr str = isUpper $ fromJust absurd $ charAt 0 str

ident ∷ SParser Var
ident = do
   x <- token.identifier
   pureIf (not (isCtr x)) x

ctr :: SParser Ctr
ctr = do
   x <- token.identifier
   pureIf (isCtr x) $ Ctr x

-- Singleton eliminator with no continuation.
simplePattern :: SParser Pattern -> SParser Pattern
simplePattern pattern' =
   try ctr_pattern <|>
   try patternVariable <|>
   try (token.parens pattern') <|>
   patternPair

   where
   -- Constructor name as a nullary constructor pattern.
   ctr_pattern :: SParser Pattern
   ctr_pattern = PattConstr <$> ctr <@> PNone

      -- TODO: anonymous variables
   patternVariable :: SParser Pattern
   patternVariable = PattVar <$> ident <@> PNone

   patternPair :: SParser Pattern
   patternPair =
      token.parens $ do
         π <- pattern' <* token.comma
         π' <- pattern'
         pure $ PattConstr cPair $ PArg 0 $ mapCont (PArg 1 π') π

arrow :: SParser Unit
arrow = token.reservedOp strArrow

equals :: SParser Unit
equals = token.reservedOp strEquals

patternDelim :: SParser Unit
patternDelim = arrow <|> equals

-- "nest" controls whether nested (curried) functions are permitted in this context
elim :: Boolean -> SParser Expr -> SParser Elim
elim curried expr' = fromJust "Incompatible branches" <$> (joinAll <$> patterns)
   where
   patterns :: SParser (NonEmptyList Pattern)
   patterns = pure <$> patternOne curried expr' patternDelim <|> patternMany
      where
      patternMany :: SParser (NonEmptyList Pattern)
      patternMany = token.braces $ sepBy1 (patternOne curried expr' arrow) token.semi

patternOne :: Boolean -> SParser Expr -> SParser Unit -> SParser Pattern
patternOne curried expr' delim = pattern' >>= rest
   where
   rest :: Pattern -> SParser Pattern
   rest π = mapCont <$> body' <@> π
      where
      body' = if curried then body <|> PLambda <$> (pattern' >>= rest) else body

   pattern' = if curried then simplePattern pattern else pattern
   body = PBody <$> (delim *> expr')

sepBy1Then :: forall a . SParser a -> SParser Unit -> SParser Unit -> SParser (NonEmptyList a)
sepBy1Then p sep end = do
   x <- p
   xs <- manyTill (sep *> p) end
   pure $ wrap $ x :| xs

letDefs :: SParser Expr -> SParser (NonEmptyList Def)
letDefs expr' = keyword strLet *> (some $ try clause <* token.semi)
   where
   clause :: SParser Def
   clause = Def <$> ((toElim <$> pattern) <* patternDelim) <*> expr'

recDefs :: SParser Expr -> SParser RecDefs
recDefs expr' = do
   fπs <- keyword strLet *> (some $ try clause <* token.semi)
   let fπss = groupBy (eq `on` fst) $ toList fπs
   pure $ map toRecDef fπss
   where
   toRecDef :: NonEmptyList (String × Pattern) -> RecDef
   toRecDef fπs =
      let f = fst $ head fπs in
      RecDef f $ fromJust ("Incompatible branches for '" <> f <> "'") $ joinAll $ map snd fπs

   clause :: SParser (Var × Pattern)
   clause = ident `lift2 (×)` (patternOne true expr' equals)

-- Tree whose branches are binary primitives and whose leaves are application chains.
expr_ :: SParser Expr
expr_ = fix $ appChain >>> buildExprParser operators
   where
   -- Left-associative tree of applications of one or more simple terms.
   appChain :: SParser Expr -> SParser Expr
   appChain expr' = simpleExpr >>= rest
      where
      rest :: Expr -> SParser Expr
      rest e@(Expr _ (Constr c es)) = ctrArgs <|> pure e
         where
         ctrArgs :: SParser Expr
         ctrArgs = simpleExpr >>= \e' -> rest (expr $ Constr c (es <> (e' : empty)))
      rest e = (expr <$> (App e <$> simpleExpr) >>= rest) <|> pure e

      -- Any expression other than an operator tree or an application chain.
      simpleExpr :: SParser Expr
      simpleExpr =
         try constrExpr <|>
         try variable <|>
         try int <|> -- int may start with +/-
         string <|>
         try let_ <|>
         letRec <|>
         matchAs <|>
         try (token.parens expr') <|>
         try parensOp <|>
         pair <|>
         lambda

         where
         constrExpr :: SParser Expr
         constrExpr = expr <$> (Constr <$> ctr <@> empty)

         variable :: SParser Expr
         variable = ident <#> Var >>> expr

         int :: SParser Expr
         int = do
            sign <- signOpt
            (sign >>> Int >>> expr) <$> token.natural
            where
            signOpt :: ∀ a . (Ring a) => SParser (a -> a)
            signOpt = (char '-' $> negate) <|> (char '+' $> identity) <|> pure identity

         string :: SParser Expr
         string = expr <$> (Str <$> token.stringLiteral)

         let_ ∷ SParser Expr
         let_ = do
            defs <- unwrap <$> try (letDefs expr')
            foldr (\def -> expr <<< Let def) <$> expr' <@> defs

         letRec :: SParser Expr
         letRec = expr <$> (LetRec <$> recDefs expr' <*> expr')

         matchAs :: SParser Expr
         matchAs = expr <$> (MatchAs <$> (keyword strMatch *> expr' <* keyword strAs) <*> elim false expr')

         -- any binary operator, in parentheses
         parensOp :: SParser Expr
         parensOp = expr <$> (Op <$> token.parens token.operator)

         pair :: SParser Expr
         pair = token.parens $
            expr <$> (lift2 $ \e e' -> Constr cPair (e : e' : empty)) (expr' <* token.comma) expr'

         lambda :: SParser Expr
         lambda = expr <$> (Lambda <$> (keyword strFun *> elim true expr'))

-- TODO: allow infix constructors, via buildExprParser
pattern :: SParser Pattern
pattern = fix appChain_pattern
   where
   -- Analogous in some way to app_chain, but nothing higher-order here: no explicit application nodes,
   -- non-saturated constructor applications, or patterns other than constructors in the function position.
   appChain_pattern :: SParser Pattern -> SParser Pattern
   appChain_pattern pattern' = simplePattern pattern' >>= rest 0
      where
         rest ∷ Int -> Pattern -> SParser Pattern
         rest n π@(PattConstr _ _) = ctrArgs <|> pure π
            where
            ctrArgs :: SParser Pattern
            ctrArgs = simplePattern pattern' >>= \π' -> rest (n + 1) $ mapCont (PArg n π') π
         rest _ π@(PattVar _ _) = pure π

-- each element of the top-level list corresponds to a precedence level
operators :: OperatorTable Identity String Expr
operators =
   fromFoldable $ map fromFoldable $
   map (map (\(OpName op _) -> Infix (try $ theBinaryOp op) AssocLeft)) $
   groupBy (eq `on` opPrec) $ sortBy (\x -> comparing opPrec x >>> invert) $ values opNames
   where
   -- specific binary operator
   theBinaryOp :: Var -> SParser (Expr -> Expr -> Expr)
   theBinaryOp op = do
      op' <- token.operator
      pureMaybe $ fromBool (op == op') (\e1 -> expr <<< BinaryApp e1 op)

topLevel :: forall a . SParser a -> SParser a
topLevel p = token.whiteSpace *> p <* eof

program ∷ SParser Expr
program = topLevel expr_

module_ :: SParser Module
module_ = Module <$> (topLevel $ concat <$> many defs)
   where
      defs :: SParser (List (Either Def RecDefs))
      defs = (bisequence <$> choose (toList <$> try (letDefs expr_)) (pure <$> recDefs expr_))
