module Parse where

import Prelude hiding (absurd, add, between, join)

import Bind (Bind, Var, (↦))
import Control.Alt ((<|>))
import Control.Apply (lift2)
import Control.Lazy (fix)
import Control.MonadPlus (empty)
import Data.Array (cons, elem, fromFoldable)
import Data.Array as Array
import Data.CodePoint.Unicode (isSpace)
import Data.Either (choose)
import Data.Function (on)
import Data.Identity (Identity)
import Data.List (List(..), (:), concat, foldr, groupBy, singleton, snoc, sortBy)
import Data.List as List
import Data.List.NonEmpty (NonEmptyList(..), toList)
import Data.Map (values)
import Data.NonEmpty ((:|))
import Data.Ordering (invert)
import Data.Profunctor.Choice ((|||))
import Data.String (codePointFromChar)
import Data.String.CodeUnits as SCU
import DataType (Ctr, cPair, isCtrName, isCtrOp)
import Doc (ParagraphElem(..), DocOpt(..), Paragraph)
import Lattice (Raw)
import Parse.Constants (str)
import Parsing.Combinators (between, notFollowedBy, option, sepBy, sepBy1, try)
import Parsing.Expr (Assoc(..), Operator(..), OperatorTable, buildExprParser)
import Parsing.Language (emptyDef)
import Parsing.String (char, eof, satisfy, string)
import Parsing.String.Basic (oneOf)
import Parsing.Token (GenLanguageDef(..), LanguageDef, TokenParser, alphaNum, letter, makeTokenParser, unGenLanguageDef)
import Pretty (prettyP)
import Primitive.Parse (OpDef, opDefs)
import SExpr (Branch, Clause(..), Clauses(..), DictEntry(..), Expr(..), ListRest(..), ListRestPattern(..), Module(..), Pattern(..), Qualifier(..), RecDefs, VarDef(..), VarDefs, setDocOpt, BaseExpr(..))
import Util (type (+), type (×), Endo, error, onlyIf, (×))
import Util.Parse (SParser, sepBy_try, sepBy1_try, some)

languageDef :: LanguageDef
languageDef = LanguageDef (unGenLanguageDef emptyDef)
   { commentStart = "{-"
   , commentEnd = "-}"
   , commentLine = "--"
   , nestedComments = true
   , identStart = letter <|> char '_'
   , identLetter = alphaNum <|> oneOf [ '_', '\'' ]
   , opStart = opChar
   , opLetter = opChar
   , reservedOpNames = [ str.bar, str.ellipsis, str.equals, str.lArrow, str.rArrow ]
   , reservedNames = [ str.as, str.else_, str.fun, str.if_, str.in_, str.let_, str.match, str.then_ ]
   , caseSensitive = true
   }
   where
   opChar :: SParser Char
   opChar = oneOf
      [ ':'
      , '!'
      , '#'
      , '$'
      , '%'
      , '&'
      , '*'
      , '+'
      , '.'
      , '/'
      , '<'
      , '='
      , '>'
      , '?'
      , '\\'
      , '^'
      , '|'
      , '-'
      , '~'
      ]

token :: TokenParser
token = makeTokenParser languageDef

lArrow :: SParser Unit
lArrow = token.reservedOp str.lArrow

lBracket :: SParser Unit
lBracket = void (token.symbol str.lBracket)

backtick :: SParser Unit
backtick = void (token.symbol str.backtick)

bar :: SParser Unit
bar = token.reservedOp str.bar

colonEq :: SParser Unit
colonEq = token.reservedOp str.colonEq

ellipsis :: SParser Unit
ellipsis = token.reservedOp str.ellipsis

equals :: SParser Unit
equals = token.reservedOp str.equals

rBracket :: SParser Unit
rBracket = void $ token.symbol str.rBracket

rArrow :: SParser Unit
rArrow = token.reservedOp str.rArrow

paragraphDelim :: SParser Unit
paragraphDelim = void $ string str.triplequote

docComment :: SParser (Raw Expr) -> SParser (DocOpt Expr Unit)
docComment expr' = option None do
   p <- try do
      _ <- token.symbol "@doc"
      paragraph expr'
   pure (Doc p)

paragraph :: SParser (Raw Expr) -> SParser (Paragraph Expr Unit)
paragraph expr' = token.lexeme (paragraphBody)
   where
   paragraphBody :: SParser (Paragraph Expr Unit)
   paragraphBody = between paragraphDelim paragraphDelim (token.whiteSpace *> (List.many $ paragraphElem expr'))

paragraphElem :: SParser (Raw Expr) -> SParser (ParagraphElem Expr Unit)
paragraphElem expr' =
   token.lexeme (try paragraphToken <|> paragraphExpr expr')

paragraphToken :: SParser (ParagraphElem Expr Unit)
paragraphToken = Token <$> (SCU.fromCharArray <$> Array.some paragraphLette)

paragraphExpr :: SParser (Raw Expr) -> SParser (ParagraphElem Expr Unit)
paragraphExpr expr' = string str.dollar *> (Unquote <$> (expr' # between (string str.curlylBrace) (string str.curlyrBrace)))

paragraphLette :: SParser Char
paragraphLette = satisfy $ \c -> (c /= '"' && c /= '$' && not (isSpace (codePointFromChar c)))

-- 'reserved' parser only checks that str isn't a prefix of a valid identifier, not that it's in reservedNames.
keyword ∷ String → SParser Unit
keyword str' =
   if str' `elem` (unGenLanguageDef languageDef).reservedNames then token.reserved str'
   else error $ str' <> " is not a reserved word"

ident ∷ SParser Var
ident = do
   x <- token.identifier
   onlyIf (not $ isCtrName x) x

ctr :: SParser Ctr
ctr = do
   x <- token.identifier
   onlyIf (isCtrName x) x

field :: forall a. SParser a -> SParser (Bind a)
field p = ident `lift2 (↦)` (token.colon *> p)

simplePattern :: Endo (SParser Pattern)
simplePattern pattern' =
   try listEmpty
      <|> listNonEmpty
      <|> try constr
      <|> try record
      <|> try var
      <|> try (token.parens pattern')
      <|> pair

   where
   listEmpty :: SParser Pattern
   listEmpty = token.brackets $ pure $ PListEmpty

   listNonEmpty :: SParser Pattern
   listNonEmpty = lBracket *> (PListNonEmpty <$> pattern' <*> fix listRest)
      where
      listRest :: Endo (SParser ListRestPattern)
      listRest listRest' =
         rBracket *> pure PListEnd <|>
            token.comma *> (PListNext <$> pattern' <*> listRest')

   -- Constructor name as a nullary constructor pattern.
   constr :: SParser Pattern
   constr = PConstr <$> ctr <@> Nil

   record :: SParser Pattern
   record = sepBy (field pattern') token.comma <#> PRecord # token.braces

   -- TODO: anonymous variables
   var :: SParser Pattern
   var = PVar <$> ident

   pair :: SParser Pattern
   pair =
      token.parens do
         π <- pattern' <* token.comma
         π' <- pattern'
         pure $ PConstr cPair (π : π' : Nil)

patternDelim :: SParser Unit
patternDelim = rArrow <|> equals

-- "curried" controls whether nested functions are permitted in this context
clause :: Boolean -> SParser (Raw Expr) -> SParser Unit -> SParser (Raw Clause)
clause curried expr' delim = do
   πs <-
      if curried then some $ simplePattern pattern
      else NonEmptyList <$> pattern `lift2 (:|)` pure Nil
   e <- delim *> expr'
   pure $ Clause (πs × e)

clause_curried :: SParser (Raw Expr) -> SParser Unit -> SParser (Raw Clause)
clause_curried expr' delim =
   Clause <$> some (simplePattern pattern) `lift2 (×)` (delim *> expr')

clause_uncurried :: SParser (Raw Expr) -> SParser Unit -> SParser (Pattern × Raw Expr)
clause_uncurried expr' delim =
   pattern `lift2 (×)` (delim *> expr')

branchMany
   :: forall b
    . SParser (Raw Expr)
   -> (SParser (Raw Expr) -> SParser Unit -> SParser b)
   -> SParser (NonEmptyList b)
branchMany expr' branch_ = token.braces $ sepBy1 (branch_ expr' rArrow) token.semi

branches :: forall b. SParser (Raw Expr) -> (SParser (Raw Expr) -> SParser Unit -> SParser b) -> SParser (NonEmptyList b)
branches expr' branch_ =
   (pure <$> branch_ expr' patternDelim) <|> branchMany expr' branch_

varDefs :: SParser (Raw Expr) -> SParser (Raw VarDefs)
varDefs expr' = keyword str.let_ *> sepBy1_try branch token.semi
   where
   branch :: SParser (Raw VarDef)
   branch = VarDef <$> (pattern <* equals) <*> expr'

recDefs :: SParser (Raw Expr) -> SParser (Raw RecDefs)
recDefs expr' = do
   keyword str.let_ *> sepBy1_try branch token.semi
   where
   branch :: SParser (Raw Branch)
   branch = ident `lift2 (×)` (clause_curried expr' equals)

defs :: SParser (Raw Expr) -> SParser (List (Raw VarDefs + Raw RecDefs))
defs expr' = singleton <$> choose (try $ varDefs expr') (recDefs expr')

-- Tree whose branches are binary primitives and whose leaves are op tree leaves.
expr_ :: SParser (Raw Expr)
expr_ = fix exprParser
   where
   -- Pushing this to front of operator table to give it higher precedence than any other binary op.
   -- (Reasonable approximation to Haskell, where backticked functions have default precedence 9.)
   exprParser :: Endo (SParser (Raw Expr))
   exprParser expr' = do
      doc <- docComment expr'
      e <- buildExprParser ([ backtickOp ] `cons` operators binaryOp) (opTreeLeaf expr')
      pure $ setDocOpt doc e

   backtickOp :: Operator Identity String (Raw Expr)
   backtickOp = flip Infix AssocLeft do
      x <- between backtick backtick ident
      pure (\e e' -> Expr' None (BinaryApp e x e'))

   -- Syntactically distinguishing infix constructors from other operators (a la Haskell) allows us to
   -- optimise an application tree into a (potentially partial) constructor application. We also treat
   -- record lookup syntactically like a binary operator, although the second argument must always be a
   -- variable.
   binaryOp :: String -> SParser (Raw Expr -> Raw Expr -> Raw Expr)
   binaryOp op = do
      op' <- token.operator
      onlyIf (op == op') $
         if op == str.dot then \e e' -> case e' of
            Expr' doc (Var x) -> Expr' doc (Project e x)
            _ -> error $ "Field names are not first class; got \"" <> prettyP e' <> "\"."
         else if isCtrOp op' then \e e' -> Expr' None (Constr unit op' (e : e' : empty))
         else \e e' -> Expr' None (BinaryApp e op e')

   opTreeLeaf :: Endo (SParser (Raw Expr))
   opTreeLeaf expr' = matchAs <|> ifElse <|> lambda <|> defsExpr <|> appChain
      where
      matchAs :: SParser (Raw Expr)
      matchAs =
         Expr' None <$> (MatchAs <$> (keyword str.match *> expr' <* keyword str.as) <*> branches expr' clause_uncurried)

      ifElse :: SParser (Raw Expr)
      ifElse = do
         e <- pure IfElse
            <*> (keyword str.if_ *> expr')
            <* keyword str.then_
            <*> expr'
            <* keyword str.else_
            <*> expr'
         pure $ Expr' None e

      lambda :: SParser (Raw Expr)
      lambda = (Expr' None <$> Lambda <<< Clauses) <$> (keyword str.fun *> branches expr' clause_curried)

      defsExpr :: SParser (Raw Expr)
      defsExpr = do
         defs' <- concat <<< toList <$> sepBy1 (defs expr') token.semi
         foldr (\def e -> Expr' None ((Let ||| LetRec) def e)) <$> (keyword str.in_ *> expr') <@> defs'

      -- Left-associative tree of applications of one or more simple terms.
      appChain :: SParser (Raw Expr)
      appChain = simpleExprOrProjection >>= rest
         where
         rest :: Raw Expr -> SParser (Raw Expr)
         rest e@(Expr' doc' (Constr α c es)) = ctrArgs <|> pure e
            where
            ctrArgs :: SParser (Raw Expr)
            ctrArgs = simpleExprOrProjection >>= \e' -> rest (Expr' doc' (Constr α c (es <> (e' : empty))))
         rest e =
            (simpleExprOrProjection >>= \e' -> rest (Expr' None (App e e'))) <|> pure e

         -- An expression that may need wrapping in parentheses to disambiguate.
         simpleExprOrProjection :: SParser (Raw Expr)
         simpleExprOrProjection =
            simpleExpr >>= projection
            where
            projection :: Raw Expr -> SParser (Raw Expr)
            projection e = dprojection e <|> rprojection e

            rprojection :: Raw Expr -> SParser (Raw Expr)
            rprojection e = (Expr' None <$> Project e <$> (token.reservedOp str.dot *> ident)) <|> pure e

            dprojection :: Raw Expr -> SParser (Raw Expr)
            dprojection e = (Expr' None <$> DProject e <$> (token.reservedOp str.dot *> token.brackets expr_))

         -- An "atomic" expression that never needs wrapping in parentheses to disambiguate.
         simpleExpr :: SParser (Raw Expr)
         simpleExpr =
            -- matrix before list
            ( try paragraphLiteral
                 <|> matrix
                 <|> try nil
                 <|> listNonEmpty
                 <|> try constr
                 <|> dict
                 <|> try float
                 <|> try int -- int may start with +/-
                 <|> stringLiteral
                 <|> try pair
                 <|> listComp
            )
               <|> try variable
               <|> try (token.parens expr_)
               <|> listEnum
               <|> try parensOp

            where
            matrix :: SParser (Raw Expr)
            matrix = between (token.symbol str.arrayLBracket) (token.symbol str.arrayRBracket) $
               Expr' None <$>
                  ( Matrix unit
                       <$> (expr_ <* bar)
                       <*> token.parens (ident `lift2 (×)` (token.comma *> ident))
                       <*> (keyword str.in_ *> expr_)
                  )

            nil :: SParser (Raw Expr)
            nil = token.brackets $ pure (Expr' None (ListEmpty unit))

            listNonEmpty :: SParser (Raw Expr)
            listNonEmpty = lBracket *> (Expr' None <$> (ListNonEmpty unit <$> expr_ <*> fix listRest))
               where
               listRest :: Endo (SParser (Raw ListRest))
               listRest listRest' =
                  rBracket *> pure (End unit) <|>
                     token.comma *> (Next unit <$> expr_ <*> listRest')

            listComp :: SParser (Raw Expr)
            listComp = token.brackets $
               Expr' None <$> (ListComp unit <$> expr_ <* bar <*> (toList <$> sepBy1 qualifier token.comma))
               where
               qualifier :: SParser (Raw Qualifier)
               qualifier =
                  ListCompGen None <$> pattern <* lArrow <*> expr_
                     <|> ListCompDecl <$> (VarDef <$> (keyword str.let_ *> pattern <* equals) <*> expr_)
                     <|> ListCompGuard <$> expr_

            listEnum :: SParser (Raw Expr)
            listEnum = token.brackets do
               start <- expr_
               _ <- ellipsis
               end <- expr_
               pure (Expr' None (ListEnum start end))

            constr :: SParser (Raw Expr)
            constr = Expr' None <$> (Constr unit <$> ctr <@> empty)

            dict :: SParser (Raw Expr)
            dict = sepBy kvPair token.comma <#> Expr' None <$> Dictionary unit # token.braces
               where
               kvPair :: SParser ((Raw DictEntry) × (Raw Expr))
               kvPair = (((ExprKey <$> expr') # token.brackets) <* token.colon) `lift2 (×)` expr_ <|> ((VarKey unit <$> ident) <* token.colon) `lift2 (×)` expr_

            variable :: SParser (Raw Expr)
            variable = ident <#> Expr' None <$> Var

            signOpt :: ∀ a. Ring a => SParser (a -> a)
            signOpt = (char '-' $> negate) <|> (char '+' $> identity) <|> pure identity

            -- built-in integer/float parsers don't seem to allow leading signs.
            int :: SParser (Raw Expr)
            int = do
               sign <- signOpt
               n <- token.natural
               pure $ Expr' None (Int unit (sign n))

            float :: SParser (Raw Expr)
            float = do
               sign <- signOpt
               f <- token.float
               pure $ Expr' None (Float unit (sign f))

            stringLiteral :: SParser (Raw Expr)
            stringLiteral = Expr' None <$> (Str unit) <$> (try (notFollowedBy paragraphDelim) *> token.stringLiteral)

            paragraphLiteral :: SParser (Raw Expr)
            paragraphLiteral = do
               p <- paragraph expr'
               pure (Expr' None (Paragraph p))

            -- any binary operator, in parentheses
            parensOp :: SParser (Raw Expr)
            parensOp = Expr' None <$> Op <$> token.parens token.operator

            pair :: SParser (Raw Expr)
            pair = token.parens $
               (pure $ \e e' -> Expr' None (Constr unit cPair (e : e' : empty))) <*> (expr' <* token.comma) <*> expr_

-- each element of the top-level list opDefs corresponds to a precedence level
operators :: forall a. (String -> SParser (a -> a -> a)) -> OperatorTable Identity String a
operators binaryOp =
   fromFoldable $
      fromFoldable <$>
         ops <#> (<$>) (\({ op, assoc }) -> Infix (try (binaryOp op)) assoc)
   where
   ops :: List (NonEmptyList OpDef)
   ops = groupBy (eq `on` _.prec) (sortBy (\x -> comparing _.prec x >>> invert) (values opDefs))

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
      onlyIf (isCtrOp op' && op == op') \π π' -> PConstr op' (π : π' : Nil)

topLevel :: forall a. Endo (SParser a)
topLevel p = token.whiteSpace *> p <* eof

program ∷ SParser (Raw Expr)
program = topLevel expr_

module_ :: SParser (Raw Module)
module_ = Module <<< concat <$> topLevel (sepBy_try (defs expr_) token.semi <* token.semi)
