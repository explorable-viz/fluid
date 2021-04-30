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
import Text.Parsing.Parser.Combinators (between, try)
import Text.Parsing.Parser.Expr (Assoc(..), Operator(..), OperatorTable, buildExprParser)
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.String (char, eof, oneOf)
import Text.Parsing.Parser.Token (
  GenLanguageDef(..), LanguageDef, TokenParser, alphaNum, letter, makeTokenParser, unGenLanguageDef
)
import Bindings (Var)
import DataType (Ctr(..), cPair, isCtrName, isCtrOp)
import Lattice (𝔹)
import Primitive.Parse (opDefs)
import SExpr (
   Branch, Clause, Expr(..), ListRest(..), ListRestPattern(..), Module(..), Pattern(..), Qualifier(..),
   RecDefs, VarDef(..), VarDefs
)
import Util (Endo, type (×), (×), type (+), error, onlyIf)
import Util.Parse (SParser, sepBy_try, sepBy1, sepBy1_try, some)

-- Constants (should also be used by prettyprinter). Haven't found a way to avoid the type definition.
str :: {
   arrayLBracket  :: String,
   arrayRBracket  :: String,
   as             :: String,
   backslash      :: String,
   bar            :: String,
   ellipsis       :: String,
   else_          :: String,
   equals         :: String,
   fun            :: String,
   if_            :: String,
   in_            :: String,
   lBracket       :: String,
   let_           :: String,
   match          :: String,
   lArrow         :: String,
   rArrow         :: String,
   rBracket       :: String,
   then_          :: String
}
str = {
   arrayLBracket: "[|",
   arrayRBracket: "|]",
   as:            "as",
   backslash:     "\\",
   bar:           "|",
   ellipsis:      "..",
   else_:         "else",
   equals:        "=",
   fun:           "fun",
   if_:           "if",
   in_:           "in",
   lBracket:      "[",
   let_:          "let",
   match:         "match",
   lArrow:        "<-",
   rArrow:        "->",
   rBracket:      "]",
   then_:         "then"
}

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
   reservedOpNames = [str.bar, str.ellipsis, str.equals, str.lArrow, str.rArrow],
   reservedNames = [str.as, str.else_, str.fun, str.if_, str.in_, str.let_, str.match, str.then_],
   caseSensitive = true
} where
   opChar :: SParser Char
   opChar = oneOf [
      ':', '!', '#', '$', '%', '&', '*', '+', '.', '/', '<', '=', '>', '?', '@', '\\', '^', '|', '-', '~'
   ]

token :: TokenParser
token = makeTokenParser languageDef

lArrow :: SParser Unit
lArrow = token.reservedOp str.lArrow

lBracket :: SParser Unit
lBracket = void $ token.symbol str.lBracket

bar :: SParser Unit
bar = token.reservedOp str.bar

ellipsis :: SParser Unit
ellipsis = token.reservedOp str.ellipsis

equals :: SParser Unit
equals = token.reservedOp str.equals

rBracket :: SParser Unit
rBracket = void $ token.symbol str.rBracket

rArrow :: SParser Unit
rArrow = token.reservedOp str.rArrow

-- 'reserved' parser only checks that str isn't a prefix of a valid identifier, not that it's in reservedNames.
keyword ∷ String → SParser Unit
keyword str' =
   if str' `elem` (unGenLanguageDef languageDef).reservedNames
   then token.reserved str'
   else error $ str' <> " is not a reserved word"

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
   listNonEmpty = lBracket *> (PListNonEmpty <$> pattern' <*> fix listRest)
      where
      listRest :: Endo (SParser ListRestPattern)
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

branch_curried :: SParser (Expr 𝔹) -> SParser Unit -> SParser (Branch 𝔹)
branch_curried expr' delim =
   some (simplePattern pattern) `lift2 (×)` (delim *> expr')

branch_uncurried :: SParser (Expr 𝔹) -> SParser Unit -> SParser (Pattern × Expr 𝔹)
branch_uncurried expr' delim =
   pattern `lift2 (×)` (delim *> expr')

branchMany :: forall b . SParser (Expr 𝔹) ->
              (SParser (Expr 𝔹) -> SParser Unit -> SParser b) ->
              SParser (NonEmptyList b)
branchMany expr' branch_ = token.braces $ sepBy1 (branch_ expr' rArrow) token.semi

branches :: forall b . SParser (Expr 𝔹) -> (SParser (Expr 𝔹) -> SParser Unit -> SParser b) -> SParser (NonEmptyList b)
branches expr' branch_ =
   (pure <$> branch_ expr' patternDelim) <|> branchMany expr' branch_

varDefs :: SParser (Expr 𝔹) -> SParser (VarDefs 𝔹)
varDefs expr' = keyword str.let_ *> sepBy1_try clause token.semi
   where
   clause :: SParser (VarDef 𝔹)
   clause = VarDef <$> (pattern <* equals) <*> expr'

recDefs :: SParser (Expr 𝔹) -> SParser (RecDefs 𝔹)
recDefs expr' = do
   keyword str.let_ *> sepBy1_try clause token.semi
   where
   clause :: SParser (Clause 𝔹)
   clause = ident `lift2 (×)` (branch_curried expr' equals)

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
         then \e e' -> Constr false (Ctr op') (e : e' : empty)
         else \e e' -> BinaryApp e op e'

   -- Left-associative tree of applications of one or more simple terms.
   appChain :: Endo (SParser (Expr 𝔹))
   appChain expr' = simpleExpr >>= rest
      where
      rest :: Expr 𝔹 -> SParser (Expr 𝔹)
      rest e@(Constr α c es) = ctrArgs <|> pure e
         where
         ctrArgs :: SParser (Expr 𝔹)
         ctrArgs = simpleExpr >>= \e' -> rest (Constr α c (es <> (e' : empty)))
      rest e = ((App e <$> simpleExpr) >>= rest) <|> pure e

      -- Any expression other than an operator tree or an application chain.
      simpleExpr :: SParser (Expr 𝔹)
      simpleExpr =
         matrix <|> -- before list
         try nil <|>
         listNonEmpty <|>
         listComp <|>
         listEnum <|>
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
         lambda <|>
         ifElse

         where
         matrix :: SParser (Expr 𝔹)
         matrix =
            between (token.symbol str.arrayLBracket) (token.symbol str.arrayRBracket) $
               Matrix false <$>
                  (expr' <* bar) <*>
                  token.parens (ident `lift2 (×)` (token.comma *> ident)) <*>
                  (keyword str.in_ *> expr')

         nil :: SParser (Expr 𝔹)
         nil = token.brackets $ pure (ListEmpty false)

         listNonEmpty :: SParser (Expr 𝔹)
         listNonEmpty =
            lBracket *> (ListNonEmpty false <$> expr' <*> fix listRest)

            where
            listRest :: Endo (SParser (ListRest 𝔹))
            listRest listRest' =
               rBracket *> pure (End false) <|>
               token.comma *> (Next false <$> expr' <*> listRest')

         listComp :: SParser (Expr 𝔹)
         listComp = token.brackets $
            pure (ListComp false) <*> expr' <* bar <*> sepBy1 qualifier (token.comma)

            where
            qualifier :: SParser (Qualifier 𝔹)
            qualifier =
               Generator <$> pattern <* lArrow <*> expr' <|>
               Declaration <$> (VarDef <$> (keyword str.let_ *> pattern <* equals) <*> expr') <|>
               Guard <$> expr'

         listEnum :: SParser (Expr 𝔹)
         listEnum = token.brackets $
            pure ListEnum <*> expr' <* ellipsis <*> expr'

         constr :: SParser (Expr 𝔹)
         constr = Constr false <$> ctr <@> empty

         variable :: SParser (Expr 𝔹)
         variable = ident <#> Var

         signOpt :: ∀ a . Ring a => SParser (a -> a)
         signOpt = (char '-' $> negate) <|> (char '+' $> identity) <|> pure identity

         -- built-in integer/float parsers don't seem to allow leading signs.
         int :: SParser (Expr 𝔹)
         int = do
            sign <- signOpt
            (sign >>> Int false) <$> token.natural

         float :: SParser (Expr 𝔹)
         float = do
            sign <- signOpt
            (sign >>> Float false) <$> token.float

         string :: SParser (Expr 𝔹)
         string = Str false <$> token.stringLiteral

         defsExpr :: SParser (Expr 𝔹)
         defsExpr = do
            defs' <- concat <<< toList <$> sepBy1 (defs expr') token.semi
            foldr (\def -> (Let ||| LetRec) def) <$> (keyword str.in_ *> expr') <@> defs'

         matchAs :: SParser (Expr 𝔹)
         matchAs =
            MatchAs <$> (keyword str.match *> expr' <* keyword str.as) <*> branches expr' branch_uncurried

         -- any binary operator, in parentheses
         parensOp :: SParser (Expr 𝔹)
         parensOp = Op <$> token.parens token.operator

         pair :: SParser (Expr 𝔹)
         pair = token.parens $
            (pure $ \e e' -> Constr false cPair (e : e' : empty)) <*> (expr' <* token.comma) <*> expr'

         lambda :: SParser (Expr 𝔹)
         lambda = Lambda <$> (keyword str.fun *> branches expr' branch_curried)

         ifElse :: SParser (Expr 𝔹)
         ifElse = pure IfElse <*> (keyword str.if_ *> expr') <* keyword str.then_ <*> expr' <* keyword str.else_ <*> expr'

backtickOp :: Operator Identity String (Expr 𝔹)
backtickOp = Infix p AssocNone
   where
   p :: SParser (Expr 𝔹 -> Expr 𝔹 -> Expr 𝔹)
   p = do
      op <- token.operator
      pure $ if isCtrOp op
         then \e e' -> Constr false (Ctr op) (e : e' : empty)
         else \e e' -> BinaryApp e op e'

-- each element of the top-level list opDefs corresponds to a precedence level
operators :: forall a . (String -> SParser (a -> a -> a)) -> OperatorTable Identity String a
operators binaryOp =
   fromFoldable $ fromFoldable <$>
   (<$>) (\({ op, assoc }) -> Infix (try (binaryOp op)) assoc) <$>
   groupBy (eq `on` _.prec) (sortBy (\x -> comparing _.prec x >>> invert) (values opDefs))

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
