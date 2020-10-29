module Parse where

import Prelude hiding (absurd, add, between, join)
import Control.Alt ((<|>))
import Control.Apply (lift2)
import Control.Lazy (fix)
import Control.MonadPlus (empty)
import Data.Array (elem, fromFoldable)
import Data.Bitraversable (bisequence)
import Data.Either (choose)
import Data.Function (on)
import Data.Identity (Identity)
import Data.List (List(..), (:), concat, foldr, groupBy, singleton, snoc, sortBy)
import Data.List.NonEmpty (NonEmptyList(..), toList)
import Data.Map (values)
import Data.NonEmpty ((:|))
import Data.Ordering (invert)
import Data.Profunctor.Choice ((|||))
import Text.Parsing.Parser.Combinators (try)
import Text.Parsing.Parser.Expr (Operator(..), OperatorTable, buildExprParser)
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.String (char, eof, oneOf)
import Text.Parsing.Parser.Token (
  GenLanguageDef(..), LanguageDef, TokenParser, alphaNum, letter, makeTokenParser, unGenLanguageDef
)
import DataType (Ctr(..), cPair, isCtrName, isCtrOp)
import Desugar (Branch, Clause)
import Desugar (Expr(..), Module(..), Pattern(..), RawExpr(..), RecDefs, VarDef, VarDefs, expr) as S
import Expr (Var)
import Lattice (𝔹)
import Primitive (opDefs)
import Util (Endo, (×), type (+), error, onlyIf)
import Util.Parse (SParser, sepBy_try, sepBy1, sepBy1_try, some)

-- constants (should also be used by prettyprinter)
strArrow       = "->"      :: String
strAs          = "as"      :: String
strBackslash   = "\\"      :: String
strEquals      = "="       :: String
strFun         = "fun"     :: String
strIn          = "in"      :: String
strLet         = "let"     :: String
strMatch       = "match"   :: String

languageDef :: LanguageDef
languageDef = LanguageDef (unGenLanguageDef emptyDef) {
   commentStart = "{-",
   commentEnd = "-}",
   commentLine = "--",
   nestedComments = true,
   identStart = letter <|> char '_',
   identLetter = alphaNum <|> oneOf ['_', '\''],
   opStart = opChar,
   opLetter = opChar,
   reservedOpNames = [],
   reservedNames = [strAs, strFun, strIn, strLet, strMatch],
   caseSensitive = true
} where
   opChar :: SParser Char
   opChar = oneOf [
      ':', '!', '#', '$', '%', '&', '*', '+', '.', '/', '<', '=', '>', '?', '@', '\\', '^', '|', '-', '~'
   ]

token :: TokenParser
token = makeTokenParser languageDef

-- 'reserved' parser only checks that str isn't a prefix of a valid identifier, not that it's in reservedNames.
keyword ∷ String → SParser Unit
keyword str =
   if str `elem` (unGenLanguageDef languageDef).reservedNames
   then token.reserved str
   else error $ str <> " is not a reserved word"

ident ∷ SParser Var
ident = do
   x <- token.identifier
   onlyIf (not $ isCtrName x) x

ctr :: SParser Ctr
ctr = do
   x <- token.identifier
   onlyIf (isCtrName x) $ Ctr x

simplePattern :: Endo (SParser S.Pattern)
simplePattern pattern' =
   try ctr_pattern <|>
   try var_pattern <|>
   try (token.parens pattern') <|>
   pair_pattern

   where
   -- Constructor name as a nullary constructor pattern.
   ctr_pattern :: SParser S.Pattern
   ctr_pattern = S.PConstr <$> ctr <@> Nil

   -- TODO: anonymous variables
   var_pattern :: SParser S.Pattern
   var_pattern = S.PVar <$> ident

   pair_pattern :: SParser S.Pattern
   pair_pattern =
      token.parens $ do
         π <- pattern' <* token.comma
         π' <- pattern'
         pure $ S.PConstr cPair (π : π' : Nil)

arrow :: SParser Unit
arrow = token.reservedOp strArrow

equals :: SParser Unit
equals = token.reservedOp strEquals

patternDelim :: SParser Unit
patternDelim = arrow <|> equals

-- "curried" controls whether nested functions are permitted in this context
branch :: Boolean -> SParser (S.Expr 𝔹) -> SParser Unit -> SParser (Branch 𝔹)
branch curried expr' delim = do
   πs <- if curried
         then some $ simplePattern pattern
         else NonEmptyList <$> pattern `lift2 (:|)` pure Nil
   e <- delim *> expr'
   pure $ πs × e

branches :: Boolean -> SParser (S.Expr 𝔹) -> SParser (NonEmptyList (Branch 𝔹))
branches curried expr' =
   pure <$> branch curried expr' patternDelim <|> branchMany
   where
   branchMany :: SParser (NonEmptyList (Branch 𝔹))
   branchMany = token.braces $ sepBy1 (branch curried expr' arrow) token.semi

varDefs :: SParser (S.Expr 𝔹) -> SParser (S.VarDefs 𝔹)
varDefs expr' = keyword strLet *> sepBy1_try clause token.semi <#> toList
   where
   clause :: SParser (S.VarDef 𝔹)
   clause = (pattern <* patternDelim) `lift2 (×)` expr'

recDefs :: SParser (S.Expr 𝔹) -> SParser (S.RecDefs 𝔹)
recDefs expr' = do
   keyword strLet *> sepBy1_try clause token.semi
   where
   clause :: SParser (Clause 𝔹)
   clause = ident `lift2 (×)` (branch true expr' equals)

defs :: SParser (S.Expr 𝔹) -> SParser (List (S.VarDef 𝔹 + S.RecDefs 𝔹))
defs expr' = bisequence <$> choose (try $ varDefs expr') (singleton <$> recDefs expr')

-- Tree whose branches are binary primitives and whose leaves are application chains.
expr :: SParser (S.Expr 𝔹)
expr = fix $ appChain >>> buildExprParser (operators binaryOp)
   where
   -- Syntactically distinguishing infix constructors from other operators (a la Haskell) allows us to
   -- optimise an application tree into a (potentially partial) constructor application.
   binaryOp :: String -> SParser (S.Expr 𝔹 -> S.Expr 𝔹 -> S.Expr 𝔹)
   binaryOp op = do
      op' <- token.operator
      onlyIf (op == op') $
         if isCtrOp op'
         then \e e' -> S.expr $ S.Constr (Ctr op') (e : e' : empty)
         else \e e' -> S.expr $ S.BinaryApp e op e'

   -- Left-associative tree of applications of one or more simple terms.
   appChain :: Endo (SParser (S.Expr 𝔹))
   appChain expr' = simpleExpr >>= rest
      where
      rest :: S.Expr 𝔹 -> SParser (S.Expr 𝔹)
      rest e@(S.Expr _ (S.Constr c es)) = ctrArgs <|> pure e
         where
         ctrArgs :: SParser (S.Expr 𝔹)
         ctrArgs = simpleExpr >>= \e' -> rest (S.expr $ S.Constr c (es <> (e' : empty)))
      rest e = (S.expr <$> (S.App e <$> simpleExpr) >>= rest) <|> pure e

      -- Any expression other than an operator tree or an application chain.
      simpleExpr :: SParser (S.Expr 𝔹)
      simpleExpr =
         try ctrExpr <|>
         try variable <|>
         try float <|>
         try int <|> -- int may start with +/-
         string <|>
         defsExpr <|>
         matchAs <|>
         try (token.parens expr') <|>
         try parensOp <|>
         pair <|>
         lambda

         where
         ctrExpr :: SParser (S.Expr 𝔹)
         ctrExpr = S.expr <$> (S.Constr <$> ctr <@> empty)

         variable :: SParser (S.Expr 𝔹)
         variable = ident <#> S.Var >>> S.expr

         signOpt :: ∀ a . Ring a => SParser (a -> a)
         signOpt = (char '-' $> negate) <|> (char '+' $> identity) <|> pure identity

         -- built-in integer/float parsers don't seem to allow leading signs.
         int :: SParser (S.Expr 𝔹)
         int = do
            sign <- signOpt
            (sign >>> S.Int >>> S.expr) <$> token.natural

         float :: SParser (S.Expr 𝔹)
         float = do
            sign <- signOpt
            (sign >>> S.Float >>> S.expr) <$> token.float

         string :: SParser (S.Expr 𝔹)
         string = (S.Str >>> S.expr) <$> token.stringLiteral

         defsExpr :: SParser (S.Expr 𝔹)
         defsExpr = do
            defs' <- concat <<< toList <$> sepBy1 (defs expr') token.semi
            foldr (\def -> S.expr <<< (S.Let ||| S.LetRec) def) <$> (keyword strIn *> expr') <@> defs'

         matchAs :: SParser (S.Expr 𝔹)
         matchAs = S.expr <$> (S.MatchAs <$> (keyword strMatch *> expr' <* keyword strAs) <*> branches false expr')

         -- any binary operator, in parentheses
         parensOp :: SParser (S.Expr 𝔹)
         parensOp = S.expr <$> (S.Op <$> token.parens token.operator)

         pair :: SParser (S.Expr 𝔹)
         pair = token.parens $
            S.expr <$> (lift2 $ \e e' -> S.Constr cPair (e : e' : empty)) (expr' <* token.comma) expr'

         lambda :: SParser (S.Expr 𝔹)
         lambda = S.expr <$> (S.Lambda <$> (keyword strFun *> branches true expr'))

-- each element of the top-level list corresponds to a precedence level
operators :: forall a . (String -> SParser (a -> a -> a)) -> OperatorTable Identity String a
operators binaryOp =
   fromFoldable $ fromFoldable <$>
   (map (\({ op, assoc }) -> Infix (try $ binaryOp op) assoc)) <$>
   groupBy (eq `on` _.prec) (sortBy (\x -> comparing _.prec x >>> invert) $ values opDefs)

-- Pattern with no continuation.
pattern :: SParser S.Pattern
pattern = fix $ appChain_pattern >>> buildExprParser (operators infixCtr)
   where
   -- Analogous in some way to app_chain, but nothing higher-order here: no explicit application nodes,
   -- non-saturated constructor applications, or patterns other than constructors in the function position.
   appChain_pattern :: Endo (SParser S.Pattern)
   appChain_pattern pattern' = simplePattern pattern' >>= rest
      where
         rest ∷ S.Pattern -> SParser S.Pattern
         rest π@(S.PConstr c πs) = ctrArgs <|> pure π
            where
            ctrArgs :: SParser S.Pattern
            ctrArgs = simplePattern pattern' >>= \π' -> rest $ S.PConstr c (πs `snoc` π')
         rest π@(S.PVar _) = pure π

   infixCtr :: String -> SParser (S.Pattern -> S.Pattern -> S.Pattern)
   infixCtr op = do
      op' <- token.operator
      onlyIf (isCtrOp op' && op == op') \π π' -> S.PConstr (Ctr op') (π : π' : Nil)

topLevel :: forall a . Endo (SParser a)
topLevel p = token.whiteSpace *> p <* eof

program ∷ SParser (S.Expr 𝔹)
program = topLevel expr

module_ :: SParser (S.Module 𝔹)
module_ = S.Module <<< concat <$> topLevel (sepBy_try (defs expr) token.semi <* token.semi)
