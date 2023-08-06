module Pretty (class Pretty, pretty, prettyP) where

import Prelude hiding (absurd, between)

import Bindings (Bind, key, val, Var, (↦))
import Data.Exists (runExists)
import Data.Foldable (class Foldable)
import Data.List (List(..), fromFoldable, (:), null)
import Data.List.NonEmpty (NonEmptyList, groupBy, singleton, toList)
import Data.Map (lookup)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Choice ((|||))
import Data.Profunctor.Strong (first)
import Data.String (Pattern(..), contains) as Data.String
import DataType (Ctr, cCons, cNil, cPair, showCtr)
import Dict (Dict)
import Dict (toUnfoldable) as D
import Expr (Cont(..), Elim(..))
import Expr (Expr(..), VarDef(..)) as E
import Parse (str)
import Primitive.Parse (opDefs)
import SExpr (Branch, Clause(..), Clauses(..), Expr(..), ListRest(..), ListRestPattern(..), Pattern(..), VarDef(..), VarDefs, Qualifier(..), RecDefs)
import Util (type (+), type (×), Endo, absurd, assert, error, intersperse, (×))
import Util.Pair (Pair(..), toTuple)
import Util.Pretty (Doc, atop, beside, empty, space, text, render, hcat)
import Val (Fun(..), Val(..)) as V
import Val (class Ann, class Highlightable, DictRep(..), ForeignOp', Fun, MatrixRep(..), Val, highlightIf)

emptyDoc :: Doc
emptyDoc = empty 0 0

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
exprType (ListEmpty _) = Simple -- try
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
   let
      prec' = getPrec op
   in
      case getPrec op of
         -1 -> prettyBinApp prec' s :--: backTicks (text op) :--: prettyBinApp prec' s'
         _ -> case prec' <= n of
            false -> prettyBinApp prec' s :--: text op :--: prettyBinApp prec' s'
            true -> parentheses (prettyBinApp prec' s :--: text op :--: prettyBinApp prec' s')
prettyBinApp _ s = prettyAppChain s

getPrec :: String -> Int
getPrec x = case lookup x opDefs of
   Just y -> y.prec
   Nothing -> -1

infixl 5 beside as .<>.
infixl 5 space as :--:
infixl 5 atop as .-.

class Pretty p where
   pretty :: p -> Doc

instance Ann a => Pretty (Expr a) where
   pretty (Var x) = emptyDoc :--: text x :--: emptyDoc
   pretty (Op op) = parentheses (text op)
   pretty (Int ann n) = highlightIf ann $ text (show n)
   pretty (Float ann n) = highlightIf ann $ text (show n)
   pretty (Str ann str) = highlightIf ann $ slashes (text str)
   pretty (Constr ann c x) = prettyConstr ann c x
   pretty (Record ann xss) = highlightIf ann $ curlyBraces (prettyOperator (.-.) xss)
   pretty (Dictionary ann sss) = highlightIf ann $ dictBrackets (pretty sss)
   pretty (Matrix ann e (x × y) e') = highlightIf ann $ arrayBrackets (pretty e .<>. text str.bar .<>. text str.lparenth .<>. text x .<>. text str.comma .<>. text y .<>. text str.rparenth :--: text str.in_ :--: pretty e')
   pretty (Lambda cs) = parentheses (text str.fun :--: pretty cs)
   pretty (Project s x) = pretty s .<>. text str.dot .<>. text x
   pretty (App s s') = prettyAppChain (App s s')
   pretty (BinaryApp s op s') = prettyBinApp 0 (BinaryApp s op s')
   pretty (MatchAs s cs) = ((text str.match :--: pretty s :--: text str.as)) .-. curlyBraces (pretty cs)
   pretty (IfElse s1 s2 s3) = emptyDoc :--: text str.if_ :--: pretty s1 :--: text str.then_ :--: pretty s2 :--: text str.else_ :--: pretty s3
   pretty (ListEmpty ann) = highlightIf ann $ brackets emptyDoc
   pretty (ListNonEmpty ann (Record _ xss) l) = emptyDoc :--: (((highlightIf ann $ text str.lBracket) .<>. (highlightIf ann $ curlyBraces (prettyOperator (.<>.) xss))) .-. pretty l)
   pretty (ListNonEmpty ann e l) = emptyDoc :--: (highlightIf ann $ text str.lBracket) .<>. pretty e .<>. pretty l
   pretty (ListEnum s s') = brackets (pretty s .<>. text str.ellipsis .<>. pretty s')
   pretty (ListComp ann s qs) = highlightIf ann $ brackets (pretty s .<>. text str.bar .<>. pretty qs)
   pretty (Let ds s) = text str.let_ :--: pretty ds :--: text str.in_ :--: pretty s
   pretty (LetRec h s) = (text str.let_ :--: pretty (First h)) .-. text str.in_ :--: pretty s

prettyOperator :: forall a. Ann a => (Doc -> Doc -> Doc) -> List (Bind (Expr a)) -> Doc
prettyOperator _ (Cons s Nil) = text (key s) .<>. text str.colon .<>. pretty (val s)
prettyOperator sep (Cons s xss) = sep (prettyOperator sep (toList (singleton s)) .<>. text str.comma) (prettyOperator sep xss)
prettyOperator _ Nil = emptyDoc

instance Ann a => Pretty (ListRest a) where
   pretty (Next ann (Record _ xss) l) = (highlightIf ann $ text str.comma) .<>. (highlightIf ann $ curlyBraces (prettyOperator (.<>.) xss)) .-. pretty l
   pretty (Next ann s l) = (highlightIf ann $ text str.comma) .<>. pretty s .<>. pretty l
   pretty (End ann) = highlightIf ann $ text str.rBracket

instance Ann a => Pretty (List (Pair (Expr a))) where
   pretty (Cons (Pair e e') Nil) = prettyPairs (Pair e e')
   pretty (Cons (Pair e e') sss) = prettyPairs (Pair e e') .<>. text str.comma :--: pretty sss
   pretty Nil = emptyDoc

prettyPairs :: forall a. Ann a => (Pair (Expr a)) -> Doc
prettyPairs (Pair e e') = pretty e :--: text str.colonEq :--: pretty e'

instance Pretty Pattern where
   pretty (PVar x) = text x
   pretty (PRecord xps) = curlyBraces (pretty xps)
   pretty (PConstr c ps) = case c == cPair of
      true -> parentheses (prettyPattConstr (text str.comma) ps)
      false -> case c == "Empty" of
         true -> text c .<>. prettyPattConstr emptyDoc ps
         false -> case c == str.colon of
            true -> parentheses (prettyPattConstr (text str.colon) ps)
            false -> parentheses (text c :--: prettyPattConstr emptyDoc ps)
   pretty (PListEmpty) = brackets emptyDoc
   pretty (PListNonEmpty p l) = text str.lBracket .<>. pretty p .<>. pretty l

instance Pretty (List (Bind (Pattern))) where
   pretty (Cons xp Nil) = text (key xp) .<>. text str.colon .<>. pretty (val xp)
   pretty (Cons x xps) = text (key x) .<>. text str.colon .<>. pretty (val x) .<>. text str.comma .-. pretty xps
   pretty Nil = emptyDoc

prettyPattConstr :: Doc -> List (Pattern) -> Doc
prettyPattConstr _ Nil = emptyDoc
prettyPattConstr _ (Cons p Nil) = pretty p
prettyPattConstr sep (Cons p ps) = case sep == emptyDoc of
   true -> pretty p :--: prettyPattConstr sep ps
   false -> pretty p .<>. sep .<>. prettyPattConstr sep ps

instance Pretty ListRestPattern where
   pretty (PNext p l) = text str.comma .<>. pretty p .<>. pretty l
   pretty PEnd = text str.rBracket

prettyClause :: forall a. Ann a => Doc -> Clause a -> Doc
prettyClause sep (Clause (ps × e)) = prettyPattConstr emptyDoc (toList ps) :--: sep :--: pretty e

instance Ann a => Pretty (Clauses a) where
   pretty (Clauses cs) = intersperse' (toList (map (prettyClause (text str.equal)) (cs))) (text str.semiColon)

instance Ann a => Pretty (Branch a) where
   pretty (x × Clause (ps × e)) = text x :--: prettyClause (text str.equal) (Clause (ps × e))

instance Ann a => Pretty (NonEmptyList (Branch a)) where
   pretty h = intersperse' (toList (map pretty h)) (text str.semiColon)

instance Ann a => Pretty (NonEmptyList (NonEmptyList (Branch a))) where
   pretty hs = intersperse' (toList (map pretty hs)) (text str.semiColon)

instance Ann a => Pretty (FirstGroup a) where
   pretty (First h) = pretty (groupBy (\p q -> key p == key q) h)

instance Ann a => Pretty (NonEmptyList (Pattern × Expr a)) where
   pretty pss = intersperse' (map (prettyClause (text str.rArrow)) (map Clause (toList (helperMatch pss)))) (text str.semiColon)

instance Ann a => Pretty (VarDef a) where
   pretty (VarDef p s) = pretty p :--: text str.equal :--: pretty s

instance Ann a => Pretty (VarDefs a) where
   pretty ds = intersperse' (toList (map pretty ds)) (text str.semiColon)

instance Ann a => Pretty (List (Expr a)) where
   pretty (Cons s Nil) = pretty s
   pretty (Cons s ss) = pretty s :--: pretty ss
   pretty Nil = emptyDoc

instance Ann a => Pretty (List (Qualifier a)) where
   pretty (Cons (Guard s) Nil) = pretty s
   pretty (Cons (Declaration d) Nil) = text str.let_ :--: pretty d
   pretty (Cons (Generator p s) Nil) = pretty p :--: text str.lArrow :--: pretty s
   pretty (Cons q qs) = pretty (toList (singleton q)) .<>. text str.comma :--: pretty qs
   pretty Nil = emptyDoc

intersperse' :: List Doc -> Doc -> Doc
intersperse' (Cons dc Nil) _ = dc
intersperse' (Cons dc dcs) dc' = dc .<>. dc' .-. intersperse' dcs dc'
intersperse' Nil _ = emptyDoc

helperMatch :: forall a. NonEmptyList (Pattern × Expr a) -> NonEmptyList (NonEmptyList Pattern × Expr a)
helperMatch pss = map (\(x × y) -> singleton x × y) pss

-- ======================================
-- Legacy Implementation : to be replaced
-- ======================================

prettyP :: forall d. Pretty d => d -> String
prettyP x = render (pretty x)

between :: Doc -> Doc -> Endo Doc
between l r doc = l .<>. doc .<>. r

brackets :: Endo Doc
brackets = between (text str.lBracket) (text str.rBracket)

dictBrackets :: Endo Doc
dictBrackets = between (text str.dictLBracket) (text str.dictRBracket)

parentheses :: Endo Doc
parentheses = between (text str.lparenth) (text str.rparenth)

slashes :: Endo Doc
slashes = between (text str.slash) (text str.slash)

backTicks :: Endo Doc
backTicks = between (text str.backtick) (text str.backtick)

curlyBraces :: Endo Doc
curlyBraces = between (text str.curlylBrace) (text str.curlyrBrace)

arrayBrackets :: Endo Doc
arrayBrackets = between (text str.arrayLBracket) (text str.arrayRBracket)

comma :: Doc
comma = text str.comma

semi :: Doc
semi = text ";"

space2 :: Doc
space2 = text " "

hspace :: forall f. Foldable f => f Doc -> Doc
hspace = fromFoldable >>> intersperse space2 >>> hcat

hcomma :: forall f. Foldable f => f Doc -> Doc
hcomma = fromFoldable >>> intersperse (comma .<>. space2) >>> hcat

parens :: Endo Doc
parens = between (text "(") (text ")")

class ToList a where
   toList2 :: a -> List a

class ToPair a where
   toPair :: a -> a × a

instance ToPair (E.Expr a) where
   toPair (E.Constr _ c (e : e' : Nil)) | c == cPair = e × e'
   toPair _ = error absurd

instance ToPair (Val a) where
   toPair (V.Constr _ c (v : v' : Nil)) | c == cPair = v × v'
   toPair _ = error absurd

instance Pretty String where
   pretty = text

vert :: forall f. Foldable f => Doc -> f Doc -> Doc
vert delim = fromFoldable >>> vert'
   where
   vert' :: List Doc -> Doc
   vert' Nil = emptyDoc
   vert' (x : Nil) = x
   vert' (x : y : xs) = atop (x .<>. delim) (vert' (y : xs))

prettyCtr :: Ctr -> Doc
prettyCtr = showCtr >>> text

-- Cheap hack; revisit.
prettyParensOpt :: forall a. Pretty a => a -> Doc
prettyParensOpt x =
   if Data.String.contains (Data.String.Pattern " ") (render doc) then parens doc
   else doc
   where
   doc = pretty x

nil :: Doc
nil = text (str.lBracket <> str.rBracket)

prettyConstr :: forall d a. Pretty d => Highlightable a => a -> Ctr -> List d -> Doc
prettyConstr α c (x : y : ys)
   | c == cPair = assert (null ys) $ highlightIf α $ parens (hcomma [ pretty x, pretty y ])
prettyConstr α c ys
   | c == cNil = assert (null ys) $ highlightIf α nil
prettyConstr α c (x : y : ys)
   | c == cCons = assert (null ys) $ parens (hspace [ pretty x, highlightIf α $ text ":", pretty y ])
prettyConstr α c xs = hspace (highlightIf α (prettyCtr c) : (prettyParensOpt <$> xs))

prettyRecordOrDict
   :: forall d b a
    . Pretty d
   => Highlightable a
   => Doc
   -> Endo Doc
   -> (b -> Doc)
   -> a
   -> List (b × d)
   -> Doc
prettyRecordOrDict sep bracify prettyKey α xvs =
   xvs <#> first prettyKey <#> (\(x × v) -> hspace [ x .<>. sep, pretty v ])
      # hcomma >>> bracify >>> highlightIf α

prettyDict :: forall d b a. Pretty d => Highlightable a => (b -> Doc) -> a -> List (b × d) -> Doc
prettyDict = between (text str.dictLBracket) (text str.dictRBracket) # prettyRecordOrDict (text str.colonEq)

prettyRecord :: forall d b a. Pretty d => Highlightable a => (b -> Doc) -> a -> List (b × d) -> Doc
prettyRecord = between (text "{") (text "}") # prettyRecordOrDict (text str.colon)

instance Highlightable a => Pretty (E.Expr a) where
   pretty (E.Var x) = text x
   pretty (E.Int α n) = highlightIf α (text (show n))
   pretty (E.Float _ n) = text (show n)
   pretty (E.Str _ str) = text (show str)
   pretty (E.Record α xes) = prettyRecord text α (xes # D.toUnfoldable)
   pretty (E.Dictionary α ees) = prettyDict pretty α (ees <#> toTuple)
   pretty (E.Constr α c es) = prettyConstr α c es
   pretty (E.Matrix _ _ _ _) = error "todo"
   pretty (E.Lambda σ) = hspace [ text str.fun, pretty σ ]
   pretty (E.Op op) = parens (text op)
   pretty (E.Let (E.VarDef σ e) e') = atop (hspace [ text str.let_, pretty σ, text str.equals, pretty e, text str.in_ ])
      (pretty e')
   pretty (E.LetRec δ e) = atop (hspace [ text str.let_, pretty δ, text str.in_ ]) (pretty e)
   pretty (E.Project e x) = pretty e .<>. text str.dot .<>. pretty x
   pretty (E.App e e') = hspace [ pretty e, pretty e' ]

instance Highlightable a => Pretty (Dict (Elim a)) where
   pretty x = go (D.toUnfoldable x)
      where
      go :: List (Var × Elim a) -> Doc
      go Nil = error absurd -- non-empty
      go (xσ : Nil) = pretty xσ
      go (xσ : δ) = atop (go δ .<>. semi) (pretty xσ)

instance Highlightable a => Pretty (Bind (Elim a)) where
   pretty (x ↦ σ) = hspace [ text x, text str.equals, pretty σ ]

instance Highlightable a => Pretty (Cont a) where
   pretty ContNone = emptyDoc
   pretty (ContExpr e) = pretty e
   pretty (ContElim σ) = pretty σ

instance Highlightable a => Pretty (Ctr × Cont a) where
   pretty (c × κ) = hspace [ text (showCtr c), text str.rArrow, pretty κ ]

instance Highlightable a => Pretty (Elim a) where
   pretty (ElimVar x κ) = hspace [ text x, text str.rArrow, pretty κ ]
   pretty (ElimConstr κs) = hcomma (pretty <$> κs) -- looks dodgy
   pretty (ElimRecord _ _) = error "todo"

instance Highlightable a => Pretty (Val a) where
   pretty (V.Int α n) = highlightIf α (text (show n))
   pretty (V.Float α n) = highlightIf α (text (show n))
   pretty (V.Str α str) = highlightIf α (text (show str))
   pretty (V.Record α xvs) = prettyRecord text α (xvs # D.toUnfoldable)
   pretty (V.Dictionary α (DictRep svs)) = prettyDict
      (\(s × β) -> highlightIf β (text (show s)))
      α
      (svs # D.toUnfoldable <#> \(s × (β × v)) -> (s × β) × v)
   pretty (V.Constr α c vs) = prettyConstr α c vs
   pretty (V.Matrix _ (MatrixRep (vss × _ × _))) = vert comma (((<$>) pretty >>> hcomma) <$> vss)
   pretty (V.Fun φ) = pretty φ

instance Highlightable a => Pretty (Fun a) where
   pretty (V.Closure _ _ _ _) = text "<closure>"
   pretty (V.Foreign φ _) = parens (runExists pretty φ)
   pretty (V.PartialConstr α c vs) = prettyConstr α c vs

instance Pretty (ForeignOp' t) where
   pretty _ = text "<extern op>" -- TODO

instance (Pretty a, Pretty b) => Pretty (a + b) where
   pretty = pretty ||| pretty
