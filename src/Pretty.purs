module Pretty (class Pretty, PrettyShow(..), pretty, prettyP) where

import Prelude hiding (absurd, between)

import Bindings (Bind, key, val, Var, (↦))
import Data.Array (foldl)
import Data.Foldable (class Foldable)
import Data.List (List(..), fromFoldable, null, uncons, (:))
import Data.List.NonEmpty (NonEmptyList, groupBy, singleton, toList)
import Data.Map (lookup)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Profunctor.Choice ((|||))
import Data.Profunctor.Strong (first)
import Data.Set (toUnfoldable) as S
import Data.String (Pattern(..), Replacement(..), contains) as DS
import Data.String (drop, replaceAll)
import DataType (Ctr, cCons, cNil, cPair, showCtr)
import Dict (Dict)
import Dict (toUnfoldable) as D
import Expr (Cont(..), Elim(..))
import Expr (Expr(..), RecDefs(..), VarDef(..)) as E
import Graph (showGraph)
import Graph.GraphImpl (GraphImpl)
import Parse.Constants (str)
import Primitive.Parse (opDefs)
import SExpr (Branch, Clause(..), Clauses(..), Expr(..), ListRest(..), ListRestPattern(..), Pattern(..), Qualifier(..), RecDefs, VarDef(..), VarDefs)
import Util (type (+), type (×), Endo, assert, intersperse, (×))
import Util.Pair (Pair(..), toTuple)
import Util.Pretty (Doc(..), atop, beside, empty, hcat, render, text)
import Val (BaseVal(..), Fun(..)) as V
import Val (class Ann, class Highlightable, BaseVal, DictRep(..), ForeignOp(..), Fun, MatrixRep(..), Val(..), highlightIf)

class Pretty p where
   pretty :: p -> Doc

newtype PrettyShow a = PrettyShow a

derive instance Newtype (PrettyShow a) _

instance Pretty a => Show (PrettyShow a) where
   show (PrettyShow x) = pretty x # render

replacement :: Array (String × String)
replacement =
   [ "( " × "("
   , " )" × ")"
   , "[ " × "["
   , " ]" × "]"
   , "{ " × "{"
   , " }" × "}"
   , ". " × "."
   , " ." × "."
   , ". " × "."
   , " ," × ","
   , " ;" × ";"
   , "| " × "|"
   , " |" × "|"
   , "⸨ " × "⸨"
   , " ⸩" × "⸩"
   ]

pattRepPairs :: Array (DS.Pattern × DS.Replacement)
pattRepPairs = map (\(x × y) -> (DS.Pattern x × DS.Replacement y)) replacement

newtype FirstGroup a = First (RecDefs a)
data ExprType = Simple | Expression
type Sep = Doc -> Doc -> Doc

exprType :: forall a. Expr a -> ExprType
exprType (Var _) = Simple
exprType (Op _) = Simple
exprType (Int _ _) = Simple
exprType (Float _ _) = Simple
exprType (Str _ _) = Simple
exprType (Constr _ _ Nil) = Simple
exprType (Constr _ _ _) = Expression
exprType (Record _ _) = Simple
exprType (Dictionary _ _) = Simple
exprType (Matrix _ _ _ _) = Simple
exprType (Lambda _) = Simple
exprType (Project _ _) = Simple
exprType (App _ _) = Expression
exprType (BinaryApp _ _ _) = Expression
exprType (MatchAs _ _) = Simple
exprType (IfElse _ _ _) = Simple
exprType (ListEmpty _) = Simple
exprType (ListNonEmpty _ _ _) = Simple
exprType (ListEnum _ _) = Simple
exprType (ListComp _ _ _) = Simple
exprType (Let _ _) = Expression
exprType (LetRec _ _) = Expression

prettySimple :: forall a. Ann a => Expr a -> Doc
prettySimple s = case exprType s of
   Simple -> pretty s
   Expression -> parentheses (pretty s)

prettyAppChain :: forall a. Ann a => Expr a -> Doc
prettyAppChain (App s s') = prettyAppChain s .<>. prettySimple s'
prettyAppChain s = prettySimple s

prettyBinApp :: forall a. Ann a => Int -> Expr a -> Doc
prettyBinApp n (BinaryApp s op s') =
   case getPrec op of
      -1 -> prettyBinApp prec' s .<>. (text ("`" <> op <> "`")) .<>. prettyBinApp prec' s'
      _ ->
         if prec' <= n then
            parentheses (prettyBinApp prec' s .<>. text op .<>. prettyBinApp prec' s')
         else
            prettyBinApp prec' s .<>. text op .<>. prettyBinApp prec' s'
   where
   prec' = getPrec op
prettyBinApp _ s = prettyAppChain s

getPrec :: String -> Int
getPrec x = case lookup x opDefs of
   Just y -> y.prec
   Nothing -> -1

infixl 5 beside as .<>.
infixl 5 atop as .-.

removeLineWS :: String -> String
removeLineWS str = foldl (\curr (x × y) -> replaceAll x y curr) str pattRepPairs

removeDocWS :: Doc -> Doc
removeDocWS (Doc d) = Doc
   { width: d.width
   , height: d.height
   , lines: map (\x -> removeLineWS (drop 1 x)) d.lines
   }

instance Ann a => Pretty (Expr a) where
   pretty (Var x) = text x
   pretty (Op op) = parentheses (text op)
   pretty (Int α n) = highlightIf α $ text (show n)
   pretty (Float α n) = highlightIf α $ text (show n)
   pretty (Str α str) = highlightIf α $ (text ("\"" <> str <> "\""))
   pretty (Constr α c x) = highlightIf α $ prettyConstr c x
   pretty (Record α xss) = highlightIf α $ curlyBraces (prettyOperator (.-.) xss)
   pretty (Dictionary α sss) = highlightIf α $ dictBrackets (pretty sss)
   pretty (Matrix α e (x × y) e') =
      highlightIf α $ arrayBrackets
         ( pretty e .<>. text str.bar .<>. parentheses (text x .<>. text str.comma .<>. text y)
              .<>. text str.in_
              .<>. pretty e'
         )
   pretty (Lambda cs) = parentheses (text str.fun .<>. pretty cs)
   pretty (Project s x) = prettySimple s .<>. text str.dot .<>. text x
   pretty (App s s') = prettyAppChain (App s s')
   pretty (BinaryApp s op s') = prettyBinApp 0 (BinaryApp s op s')
   pretty (MatchAs s cs) = ((text str.match .<>. pretty s .<>. text str.as)) .-. curlyBraces (pretty cs)
   pretty (IfElse s1 s2 s3) = text str.if_ .<>. pretty s1 .<>. text str.then_ .<>. pretty s2 .<>. text str.else_ .<>. pretty s3
   pretty (ListEmpty ann) = (highlightIf ann $ brackets empty)
   pretty (ListNonEmpty ann (Record _ xss) l) =
      (((highlightIf ann $ text str.lBracket)) .<>. ((highlightIf ann $ curlyBraces (prettyOperator (.<>.) xss)))) .-. pretty l
   pretty (ListNonEmpty ann e l) = ((highlightIf ann $ text str.lBracket)) .<>. pretty e .<>. pretty l
   pretty (ListEnum s s') = brackets (pretty s .<>. text str.ellipsis .<>. pretty s')
   pretty (ListComp ann s qs) = (highlightIf ann $ brackets (pretty s .<>. text str.bar .<>. pretty qs))
   pretty (Let ds s) = (text str.let_ .<>. pretty ds .<>. text str.in_) .-. pretty s
   pretty (LetRec h s) = (text str.let_ .<>. pretty (First h) .<>. text str.in_) .-. pretty s

prettyOperator :: forall a. Ann a => (Doc -> Doc -> Doc) -> List (Bind (Expr a)) -> Doc
prettyOperator _ (Cons s Nil) = text (key s) .<>. text str.colon .<>. pretty (val s)
prettyOperator sep (Cons s xss) = sep (prettyOperator sep (toList (singleton s)) .<>. text str.comma) (prettyOperator sep xss)
prettyOperator _ Nil = empty

instance Ann a => Pretty (ListRest a) where
   pretty (Next ann (Record _ xss) l) = ((highlightIf ann $ text str.comma)) .<>. ((highlightIf ann $ curlyBraces (prettyOperator (.<>.) xss))) .-. pretty l
   pretty (Next ann s l) = ((highlightIf ann $ text str.comma)) .<>. pretty s .<>. pretty l
   pretty (End ann) = (highlightIf ann $ text str.rBracket)

instance Ann a => Pretty (List (Pair (Expr a))) where
   pretty (Cons (Pair e e') Nil) = prettyPairs (Pair e e')
   pretty (Cons (Pair e e') sss) = prettyPairs (Pair e e') .<>. text str.comma .<>. pretty sss
   pretty Nil = empty

prettyPairs :: forall a. Ann a => (Pair (Expr a)) -> Doc
prettyPairs (Pair e e') = pretty e .<>. text str.colonEq .<>. pretty e'

instance Pretty Pattern where
   pretty (PVar x) = text x
   pretty (PRecord xps) = curlyBraces (pretty xps)
   pretty (PConstr c ps) = case uncons ps of
      Just { head: p, tail: Nil } -> pretty c .<>. pretty p
      _ ->
         if c == cPair then (parentheses (prettyPattConstr (text str.comma) ps))
         else if c == cCons then (parentheses (prettyPattConstr (text str.colon) ps))
         else
            parentheses (text c .<>. prettyPattConstr empty ps)

   pretty (PListEmpty) = brackets empty
   pretty (PListNonEmpty p l) = text str.lBracket .<>. pretty p .<>. pretty l

instance Pretty (List (Bind Pattern)) where
   pretty (Cons xp Nil) = text (key xp) .<>. text str.colon .<>. pretty (val xp)
   pretty (Cons xp xps) = text (key xp) .<>. text str.colon .<>. pretty (val xp) .<>. text str.comma .-. pretty xps
   pretty Nil = empty

prettyPattConstr :: Doc -> List Pattern -> Doc
prettyPattConstr _ Nil = empty
prettyPattConstr _ (Cons p Nil) = pretty p
prettyPattConstr sep (Cons p ps) = pretty p .<>. sep .<>. prettyPattConstr sep ps

instance Pretty ListRestPattern where
   pretty (PNext p l) = text str.comma .<>. pretty p .<>. pretty l
   pretty PEnd = text str.rBracket

prettyClause :: forall a. Ann a => Doc -> Clause a -> Doc
prettyClause sep (Clause (ps × e)) = prettyPattConstr empty (toList ps) .<>. sep .<>. pretty e

instance Ann a => Pretty (Clauses a) where
   pretty (Clauses cs) = intersperse' (toList (map (prettyClause (text str.equals)) (cs))) (text str.semiColon)

instance Ann a => Pretty (NonEmptyList (Branch a)) where
   pretty h = intersperse' (toList (map pretty h)) (text str.semiColon)

instance Ann a => Pretty (NonEmptyList (NonEmptyList (Branch a))) where
   pretty hs = intersperse' (toList (map pretty hs)) (text str.semiColon)

instance Ann a => Pretty (FirstGroup a) where
   pretty (First h) = pretty (groupBy (\p q -> key p == key q) h)

instance Ann a => Pretty (NonEmptyList (Pattern × Expr a)) where
   pretty pss =
      intersperse' (prettyClause (text str.rArrow) <$> (Clause <$> toList (helperMatch pss))) (text str.semiColon)

instance Ann a => Pretty (VarDef a) where
   pretty (VarDef p s) = pretty p .<>. text str.equals .<>. pretty s

instance Ann a => Pretty (VarDefs a) where
   pretty ds = intersperse' (toList (map pretty ds)) (text str.semiColon)

instance Ann a => Pretty (List (Expr a)) where
   pretty (Cons s Nil) = pretty s
   pretty (Cons s ss) = pretty s .<>. pretty ss
   pretty Nil = empty

instance Ann a => Pretty (List (Qualifier a)) where
   pretty (Cons (Guard s) Nil) = pretty s
   pretty (Cons (Declaration d) Nil) = text str.let_ .<>. pretty d
   pretty (Cons (Generator p s) Nil) = pretty p .<>. text str.lArrow .<>. pretty s
   pretty (Cons q qs) = pretty (toList (singleton q)) .<>. text str.comma .<>. pretty qs
   pretty Nil = empty

intersperse' :: List Doc -> Doc -> Doc
intersperse' (Cons dc Nil) _ = dc
intersperse' (Cons dc dcs) dc' = dc .<>. dc' .-. intersperse' dcs dc'
intersperse' Nil _ = empty

helperMatch :: forall a. NonEmptyList (Pattern × Expr a) -> NonEmptyList (NonEmptyList Pattern × Expr a)
helperMatch pss = map (\(x × y) -> singleton x × y) pss

prettyP :: forall d. Pretty d => d -> String
prettyP x = render (removeDocWS (pretty x))

between :: Doc -> Doc -> Endo Doc
between l r doc = l .<>. doc .<>. r

brackets :: Endo Doc
brackets = between (text str.lBracket) (text str.rBracket)

dictBrackets :: Endo Doc
dictBrackets = between (text str.dictLBracket) (text str.dictRBracket)

parentheses :: Endo Doc
parentheses = between (text str.lparenth) (text str.rparenth)

curlyBraces :: Endo Doc
curlyBraces = between (text str.curlylBrace) (text str.curlyrBrace)

arrayBrackets :: Endo Doc
arrayBrackets = between (text str.arrayLBracket) (text str.arrayRBracket)

comma :: Doc
comma = text str.comma

semi :: Doc
semi = text str.semiColon

hcomma :: forall f. Foldable f => f Doc -> Doc
hcomma = fromFoldable >>> intersperse comma >>> hcat

parens :: Endo Doc
parens = between (text "(") (text ")")

class ToList a where
   toList2 :: a -> List a

instance Pretty String where
   pretty = text

vert :: forall f. Foldable f => Doc -> f Doc -> Doc
vert delim = fromFoldable >>> vert'
   where
   vert' :: List Doc -> Doc
   vert' Nil = empty
   vert' (x : Nil) = x
   vert' (x : y : xs) = atop (x .<>. delim) (vert' (y : xs))

prettyCtr :: Ctr -> Doc
prettyCtr = showCtr >>> text

-- Cheap hack; revisit.
prettyParensOpt :: forall a. Pretty a => a -> Doc
prettyParensOpt x =
   if DS.contains (DS.Pattern " ") (render doc) then parens doc
   else doc
   where
   doc = pretty x

nil :: Doc
nil = text (str.lBracket <> str.rBracket)

prettyConstr :: forall d. Pretty d => Ctr -> List d -> Doc
prettyConstr c (x : y : ys)
   | c == cPair = assert (null ys) $ parens (hcomma [ pretty x, pretty y ])
prettyConstr c ys
   | c == cNil = assert (null ys) nil
prettyConstr c (x : y : ys)
   | c == cCons = assert (null ys) $ parens (hcat [ pretty x, text str.colon, pretty y ])
prettyConstr c (x : Nil) = prettyCtr c .<>. pretty x
prettyConstr c xs = hcat (prettyCtr c : (prettyParensOpt <$> xs))

prettyRecordOrDict
   :: forall d b
    . Pretty d
   => Doc
   -> Endo Doc
   -> (b -> Doc)
   -> List (b × d)
   -> Doc
prettyRecordOrDict sep bracify prettyKey xvs =
   xvs <#> first prettyKey <#> (\(x × v) -> hcat [ x .<>. sep, pretty v ])
      # hcomma >>> bracify

prettyDict :: forall d b. Pretty d => (b -> Doc) -> List (b × d) -> Doc
prettyDict = between (text str.dictLBracket) (text str.dictRBracket) # prettyRecordOrDict (text str.colonEq)

prettyRecord :: forall d b. Pretty d => (b -> Doc) -> List (b × d) -> Doc
prettyRecord = curlyBraces # prettyRecordOrDict (text str.colon)

prettyMatrix :: forall a. Highlightable a => E.Expr a -> Var -> Var -> E.Expr a -> Doc
prettyMatrix e1 i j e2 = arrayBrackets (pretty e1 .<>. text str.lArrow .<>. text (i <> "×" <> j) .<>. text str.in_ .<>. pretty e2)

instance Highlightable a => Pretty (E.Expr a) where
   pretty (E.Var x) = text x
   pretty (E.Int α n) = highlightIf α (text (show n))
   pretty (E.Float _ n) = text (show n)
   pretty (E.Str _ str) = text (show str)
   pretty (E.Record α xes) = highlightIf α $ prettyRecord text (xes # D.toUnfoldable)
   pretty (E.Dictionary α ees) = highlightIf α $ prettyDict pretty (ees <#> toTuple)
   pretty (E.Constr α c es) = highlightIf α $ prettyConstr c es
   pretty (E.Matrix α e1 (i × j) e2) = (highlightIf α (prettyMatrix e1 i j e2))
   pretty (E.Lambda _ σ) = hcat [ text str.fun, pretty σ ]
   pretty (E.Op op) = parens (text op)
   pretty (E.Let (E.VarDef σ e) e') = atop (hcat [ text str.let_, pretty σ, text str.equals, pretty e, text str.in_ ])
      (pretty e')
   pretty (E.LetRec (E.RecDefs _ ρ) e) = atop (hcat [ text str.let_, pretty ρ, text str.in_ ]) (pretty e)
   pretty (E.Project e x) = pretty e .<>. text str.dot .<>. pretty x
   pretty (E.App e e') = hcat [ pretty e, pretty e' ]

instance Highlightable a => Pretty (Dict (Elim a)) where
   pretty ρ = go (D.toUnfoldable ρ)
      where
      go :: List (Var × Elim a) -> Doc
      go Nil = empty
      go (xσ : Nil) = pretty xσ
      go (xσ : δ) = atop (go δ .<>. semi) (pretty xσ)

instance Highlightable a => Pretty (Dict (Val a)) where
   pretty γ = brackets $ go (D.toUnfoldable γ)
      where
      go :: List (Var × Val a) -> Doc
      go Nil = empty
      go ((x × v) : rest) =
         (text x .<>. text str.rArrow .<>. pretty v .<>. text str.comma) `atop` go rest

instance Highlightable a => Pretty (Bind (Elim a)) where
   pretty (x ↦ σ) = hcat [ text x, text str.equals, pretty σ ]
else instance Ann a => Pretty (Branch a) where
   pretty (x × Clause (ps × e)) = text x .<>. prettyClause (text str.equals) (Clause (ps × e))
else instance Highlightable a => Pretty (Ctr × Cont a) where
   pretty (c × κ) = hcat [ text (showCtr c), text str.rArrow, pretty κ ]
else instance (Pretty a, Pretty b) => Pretty (a × b) where
   pretty (a × b) = parentheses $ pretty a .<>. text str.comma .<>. pretty b

instance Highlightable a => Pretty (Cont a) where
   pretty ContNone = empty
   pretty (ContExpr e) = pretty e
   pretty (ContElim σ) = pretty σ

instance Highlightable a => Pretty (Elim a) where
   pretty (ElimVar x κ) = hcat [ text x, text str.rArrow, pretty κ ]
   pretty (ElimConstr κs) = hcomma (pretty <$> κs) -- looks dodgy
   pretty (ElimRecord xs κ) = hcat [ curlyBraces $ hcomma (text <$> (S.toUnfoldable xs :: List String)), text str.rArrow, curlyBraces (pretty κ) ]

instance Highlightable a => Pretty (Val a) where
   pretty (Val α v) = highlightIf α $ pretty v

instance Highlightable a => Pretty (BaseVal a) where
   pretty (V.Int n) = text (show n)
   pretty (V.Float n) = text (show n)
   pretty (V.Str str) = text (show str)
   pretty (V.Record xvs) = prettyRecord text (xvs # D.toUnfoldable)
   pretty (V.Dictionary (DictRep svs)) = prettyDict
      (\(s × β) -> highlightIf β (text (show s)))
      (svs # D.toUnfoldable <#> \(s × (β × v)) -> (s × β) × v)
   pretty (V.Constr c vs) = prettyConstr c vs
   pretty (V.Matrix (MatrixRep (vss × _ × _))) = vert comma (((<$>) pretty >>> hcomma) <$> vss)
   pretty (V.Fun φ) = pretty φ

instance Highlightable a => Pretty (Fun a) where
   pretty (V.Closure γ ρ σ) =
      text "cl" .<>.
         parentheses (pretty γ .<>. text str.comma .<>. pretty ρ .<>. text str.comma .<>. pretty σ)
   pretty (V.Foreign φ _) = pretty φ
   pretty (V.PartialConstr c vs) = prettyConstr c vs

instance Pretty ForeignOp where
   pretty (ForeignOp (s × _)) = text s

instance (Pretty a, Pretty b) => Pretty (a + b) where
   pretty = pretty ||| pretty

instance Pretty GraphImpl where
   pretty = showGraph >>> text
