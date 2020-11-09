module Parse where

import Prelude hiding (absurd, add, between, join)
import Control.Alt ((<|>))
import Control.Apply (lift2)
import Control.Lazy (fix)
import Control.MonadPlus (empty)
import Data.Array (elem, fromFoldable)
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
import Expr (Var)
import Lattice (𝔹)
import Primitive (opDefs)
import SExpr (
   Branch, Clause, Expr(..), ListRest(..), ListPatternRest(..), Module(..), Pattern(..), Qualifier(..),
   RawExpr(..), RecDefs, VarDef, VarDefs, expr
)
import Util (Endo, (×), type (+), error, onlyIf)
import Util.Parse (SParser, sepBy_try, sepBy1, sepBy1_try, some)

-- constants (should also be used by prettyprinter)
strAs          = "as"      :: String
strBackslash   = "\\"      :: String
strBar         = "|"       :: String
strEquals      = "="       :: String
strFun         = "fun"     :: String
strIn          = "in"      :: String
strLBracket    = "["       :: String
strLet         = "let"     :: String
strMatch       = "match"   :: String
strLArrow      = "<-"      :: String
strRArrow      = "->"      :: String
strRBracket    = "]"       :: String

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

lArrow :: SParser Unit
lArrow = token.reservedOp strLArrow

lBracket :: SParser Unit
lBracket = void $ token.symbol strLBracket

bar :: SParser Unit
bar = token.reservedOp strBar

rBracket :: SParser Unit
rBracket = void $ token.symbol strRBracket

rArrow :: SParser Unit
rArrow = token.reservedOp strRArrow

equals :: SParser Unit
equals = token.reservedOp strEquals

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

simplePattern :: Endo (SParser Pattern)
simplePattern pattern' =
   try listEmpty <|>
   listNonEmpty <|>
   try constr <|>
   try var <|>
   try (token.parens pattern') <|>
   pair

   where
   listEmpty :: SParser Pattern
   listEmpty = token.brackets $ pure $ PListEmpty

   listNonEmpty :: SParser Pattern
   listNonEmpty =
      lBracket *> (PListNonEmpty <$> pattern' <*> fix listRest)

         where
         listRest :: Endo (SParser ListPatternRest)
         listRest listRest' =
            rBracket *> pure PEnd <|>
            token.comma *> (PNext <$> pattern' <*> listRest')

   -- Constructor name as a nullary constructor pattern.
   constr :: SParser Pattern
   constr = PConstr <$> ctr <@> Nil

   -- TODO: anonymous variables
   var :: SParser Pattern
   var = PVar <$> ident

   pair :: SParser Pattern
   pair =
      token.parens $ do
         π <- pattern' <* token.comma
         π' <- pattern'
         pure $ PConstr cPair (π : π' : Nil)

patternDelim :: SParser Unit
patternDelim = rArrow <|> equals

-- "curried" controls whether nested functions are permitted in this context
branch :: Boolean -> SParser (Expr 𝔹) -> SParser Unit -> SParser (Branch 𝔹)
branch curried expr' delim = do
   πs <- if curried
         then some $ simplePattern pattern
         else NonEmptyList <$> pattern `lift2 (:|)` pure Nil
   e <- delim *> expr'
   pure $ πs × e

branches :: Boolean -> SParser (Expr 𝔹) -> SParser (NonEmptyList (Branch 𝔹))
branches curried expr' =
   pure <$> branch curried expr' patternDelim <|> branchMany
   where
   branchMany :: SParser (NonEmptyList (Branch 𝔹))
   branchMany = token.braces $ sepBy1 (branch curried expr' rArrow) token.semi

varDefs :: SParser (Expr 𝔹) -> SParser (VarDefs 𝔹)
varDefs expr' = keyword strLet *> sepBy1_try clause token.semi
   where
   clause :: SParser (VarDef 𝔹)
   clause = (pattern <* equals) `lift2 (×)` expr'

recDefs :: SParser (Expr 𝔹) -> SParser (RecDefs 𝔹)
recDefs expr' = do
   keyword strLet *> sepBy1_try clause token.semi
   where
   clause :: SParser (Clause 𝔹)
   clause = ident `lift2 (×)` (branch true expr' equals)

defs :: SParser (Expr 𝔹) -> SParser (List (VarDefs 𝔹 + RecDefs 𝔹))
defs expr' = singleton <$> choose (try $ varDefs expr') (recDefs expr')

-- Tree whose branches are binary primitives and whose leaves are application chains.
expr_ :: SParser (Expr 𝔹)
expr_ = fix $ appChain >>> buildExprParser (operators binaryOp)
   where
   -- Syntactically distinguishing infix constructors from other operators (a la Haskell) allows us to
   -- optimise an application tree into a (potentially partial) constructor application.
   binaryOp :: String -> SParser (Expr 𝔹 -> Expr 𝔹 -> Expr 𝔹)
   binaryOp op = do
      op' <- token.operator
      onlyIf (op == op') $
         if isCtrOp op'
         then \e e' -> expr $ Constr (Ctr op') (e : e' : empty)
         else \e e' -> expr $ BinaryApp e op e'

   -- Left-associative tree of applications of one or more simple terms.
   appChain :: Endo (SParser (Expr 𝔹))
   appChain expr' = simpleExpr >>= rest
      where
      rest :: Expr 𝔹 -> SParser (Expr 𝔹)
      rest e@(Expr _ (Constr c es)) = ctrArgs <|> pure e
         where
         ctrArgs :: SParser (Expr 𝔹)
         ctrArgs = simpleExpr >>= \e' -> rest (expr $ Constr c (es <> (e' : empty)))
      rest e = (expr <$> (App e <$> simpleExpr) >>= rest) <|> pure e

      -- Any expression other than an operator tree or an application chain.
      simpleExpr :: SParser (Expr 𝔹)
      simpleExpr =
         try listEmpty <|>
         listNonEmpty <|>
         listComp <|>
         try constr <|>
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
         listEmpty :: SParser (Expr 𝔹)
         listEmpty = token.brackets $ pure $ expr ListEmpty

         listNonEmpty :: SParser (Expr 𝔹)
         listNonEmpty =
            lBracket *> (expr <$> (ListNonEmpty <$> expr' <*> fix listRest))

            where
            listRest :: Endo (SParser (ListRest 𝔹))
            listRest listRest' =
               rBracket *> pure End <|>
               token.comma *> (Next <$> expr' <*> listRest')

         listComp :: SParser (Expr 𝔹)
         listComp = token.brackets $
            expr <$> lift2 ListComp (expr' <* bar) (sepBy1 qualifier token.comma)

            where
            qualifier :: SParser (Qualifier 𝔹)
            qualifier =
               Guard <$> expr' <|>
               lift2 Generator (pattern <* lArrow) expr' <|>
               lift2 Declaration (pattern <* equals) expr'

         constr :: SParser (Expr 𝔹)
         constr = expr <$> (Constr <$> ctr <@> empty)

         variable :: SParser (Expr 𝔹)
         variable = ident <#> Var >>> expr

         signOpt :: ∀ a . Ring a => SParser (a -> a)
         signOpt = (char '-' $> negate) <|> (char '+' $> identity) <|> pure identity

         -- built-in integer/float parsers don't seem to allow leading signs.
         int :: SParser (Expr 𝔹)
         int = do
            sign <- signOpt
            (sign >>> Int >>> expr) <$> token.natural

         float :: SParser (Expr 𝔹)
         float = do
            sign <- signOpt
            (sign >>> Float >>> expr) <$> token.float

         string :: SParser (Expr 𝔹)
         string = (Str >>> expr) <$> token.stringLiteral

         defsExpr :: SParser (Expr 𝔹)
         defsExpr = do
            defs' <- concat <<< toList <$> sepBy1 (defs expr') token.semi
            foldr (\def -> expr <<< (Let ||| LetRec) def) <$> (keyword strIn *> expr') <@> defs'

         matchAs :: SParser (Expr 𝔹)
         matchAs =  expr <$> (MatchAs <$> (keyword strMatch *> expr' <* keyword strAs) <*> branches false expr')

         -- any binary operator, in parentheses
         parensOp :: SParser (Expr 𝔹)
         parensOp = expr <$> (Op <$> token.parens token.operator)

         pair :: SParser (Expr 𝔹)
         pair = token.parens $
            expr <$> (lift2 $ \e e' -> Constr cPair (e : e' : empty)) (expr' <* token.comma) expr'

         lambda :: SParser (Expr 𝔹)
         lambda = expr <$> (Lambda <$> (keyword strFun *> branches true expr'))

-- each element of the top-level list corresponds to a precedence level
operators :: forall a . (String -> SParser (a -> a -> a)) -> OperatorTable Identity String a
operators binaryOp =
   fromFoldable $ fromFoldable <$>
   (map (\({ op, assoc }) -> Infix (try $ binaryOp op) assoc)) <$>
   groupBy (eq `on` _.prec) (sortBy (\x -> comparing _.prec x >>> invert) $ values opDefs)

-- Pattern with no continuation.
pattern :: SParser Pattern
pattern = fix $ appChain_pattern >>> buildExprParser (operators infixCtr)
   where
   -- Analogous in some way to app_chain, but nothing higher-order here: no explicit application nodes,
   -- non-saturated constructor applications, or patterns other than constructors in the function position.
   appChain_pattern :: Endo (SParser Pattern)
   appChain_pattern pattern' = simplePattern pattern' >>= rest
      where
         rest ∷ Pattern -> SParser Pattern
         rest π@(PConstr c πs) = ctrArgs <|> pure π
            where
            ctrArgs :: SParser Pattern
            ctrArgs = simplePattern pattern' >>= \π' -> rest $ PConstr c (πs `snoc` π')
         rest π = pure π

   infixCtr :: String -> SParser (Pattern -> Pattern -> Pattern)
   infixCtr op = do
      op' <- token.operator
      onlyIf (isCtrOp op' && op == op') \π π' -> PConstr (Ctr op') (π : π' : Nil)

topLevel :: forall a . Endo (SParser a)
topLevel p = token.whiteSpace *> p <* eof

program ∷ SParser (Expr 𝔹)
program = topLevel expr_

module_ :: SParser (Module 𝔹)
module_ = Module <<< concat <$> topLevel (sepBy_try (defs expr_) token.semi <* token.semi)
