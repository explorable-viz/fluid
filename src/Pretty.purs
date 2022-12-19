module Pretty (class Highlightable, class Pretty, class ToList, highlightIf, pretty, prettyP, toList, module P) where

import Prelude hiding (absurd, between)

import Bindings (Bind, Var, (↦))
import Data.Foldable (class Foldable)
import Data.List (List(..), (:), fromFoldable, null)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty (toList) as NEL
import Data.Profunctor.Choice ((|||))
import Data.Profunctor.Strong (first)
import Data.String (Pattern(..), contains) as Data.String
import DataType (Ctr, cCons, cNil, cPair, showCtr)
import Dict (Dict)
import Dict (toUnfoldable) as D
import Expr (Cont(..), Elim(..))
import Expr (Expr(..), VarDef(..)) as E
import Lattice (𝔹)
import Parse (str)
import SExpr (Expr(..), ListRest(..), ListRestPattern(..), Pattern(..), Qualifier(..), VarDef(..)) as S
import Text.Pretty (Doc, atop, beside, empty, hcat, render, text)
import Text.Pretty (render) as P
import Util (type (+), type (×), Endo, absurd, assert, error, intersperse, (×))
import Util.Pair (toTuple)
import Val (PrimOp, Val)
import Val (Val(..)) as V

infixl 5 beside as :<>:

prettyP :: forall d. Pretty d => d -> String
prettyP = pretty >>> render

between :: Doc -> Doc -> Endo Doc
between l r doc = l :<>: doc :<>: r

brackets :: Endo Doc
brackets = between (text str.lBracket) (text str.rBracket)

class Highlightable a where
   highlightIf :: a -> Endo Doc

instance Highlightable Boolean where
   highlightIf false = identity
   highlightIf true = between (text "_") (text "_")

colon :: Doc
colon = text str.colon

comma :: Doc
comma = text ","

semi :: Doc
semi = text ";"

space :: Doc
space = text " "

hspace :: forall f. Foldable f => f Doc -> Doc
hspace = fromFoldable >>> intersperse space >>> hcat

hcomma :: forall f. Foldable f => f Doc -> Doc
hcomma = fromFoldable >>> intersperse (comma :<>: space) >>> hcat

parens :: Endo Doc
parens = between (text "(") (text ")")

emptyDoc :: Doc
emptyDoc = empty 0 0

class ToList a where
   toList :: a -> List a

class ToPair a where
   toPair :: a -> a × a

instance ToPair (E.Expr a) where
   toPair (E.Constr _ c (e : e' : Nil)) | c == cPair = e × e'
   toPair _ = error absurd

instance ToPair (Val a) where
   toPair (V.Constr _ c (v : v' : Nil)) | c == cPair = v × v'
   toPair _ = error absurd

class Pretty p where
   pretty :: p -> Doc

instance Pretty String where
   pretty = text

vert :: forall f. Foldable f => Doc -> f Doc -> Doc
vert delim = fromFoldable >>> vert'
   where
   vert' :: List Doc -> Doc
   vert' Nil = emptyDoc
   vert' (x : Nil) = x
   vert' (x : y : xs) = atop (x :<>: delim) (vert' (y : xs))

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

prettyRecordOrDict :: forall d b a. Pretty d => Highlightable a => Endo Doc -> (b -> Doc) -> a -> List (b × d) -> Doc
prettyRecordOrDict bracify prettyKey α xvs =
   xvs <#> first prettyKey <#> (\(x × v) -> hspace [ x :<>: colon, pretty v ])
      # hcomma >>> bracify >>> highlightIf α

prettyDict :: forall d b a. Pretty d => Highlightable a => (b -> Doc) -> a -> List (b × d) -> Doc
prettyDict = between (text "{|") (text "|}") # prettyRecordOrDict

prettyRecord :: forall d b a. Pretty d => Highlightable a => (b -> Doc) -> a -> List (b × d) -> Doc
prettyRecord = between (text "{") (text "}") # prettyRecordOrDict

instance Pretty (E.Expr Boolean) where
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
   pretty (E.Project _ _) = error "todo"
   pretty (E.App e e') = hspace [ pretty e, pretty e' ]

instance Pretty (Dict (Elim Boolean)) where
   pretty = D.toUnfoldable >>> go
      where
      go :: List (Var × Elim 𝔹) -> Doc
      go Nil = error absurd -- non-empty
      go (xσ : Nil) = pretty xσ
      go (xσ : δ) = atop (go δ :<>: semi) (pretty xσ)

instance Pretty (Bind (Elim Boolean)) where
   pretty (x ↦ σ) = hspace [ text x, text str.equals, pretty σ ]

instance Pretty (Cont Boolean) where
   pretty ContNone = emptyDoc
   pretty (ContExpr e) = pretty e
   pretty (ContElim σ) = pretty σ

instance Pretty (Ctr × Cont Boolean) where
   pretty (c × κ) = hspace [ text (showCtr c), text str.rArrow, pretty κ ]

instance Pretty (Elim Boolean) where
   pretty (ElimVar x κ) = hspace [ text x, text str.rArrow, pretty κ ]
   pretty (ElimConstr κs) = hcomma (pretty <$> κs) -- looks dodgy
   pretty (ElimRecord _ _) = error "todo"

instance Pretty (Val Boolean) where
   pretty (V.Int α n) = highlightIf α (text (show n))
   pretty (V.Float α n) = highlightIf α (text (show n))
   pretty (V.Str α str) = highlightIf α (text (show str))
   pretty (V.Record α xvs) = prettyRecord text α (xvs # D.toUnfoldable)
   pretty (V.Dictionary α svs) = prettyDict (text <<< show) α (svs # D.toUnfoldable)
   pretty (V.Constr α c vs) = prettyConstr α c vs
   pretty (V.Matrix _ (vss × _ × _)) = vert comma (((<$>) pretty >>> hcomma) <$> vss)
   pretty (V.Closure _ _ _ _) = text "<closure>"
   pretty (V.Primitive φ _) = parens (pretty φ)

instance Pretty PrimOp where
   pretty _ = text "<prim op>" -- TODO

-- Surface language

instance Highlightable a => ToPair (S.Expr a) where
   toPair (S.Constr _ c (s : s' : Nil)) | c == cPair = s × s'
   toPair s = error ("Not a pair: " <> prettyP s)

instance Highlightable a => Pretty (S.Expr a) where
   pretty (S.Var x) = text x
   pretty (S.Op op) = parens (text op)
   pretty (S.Int α n) = highlightIf α (text (show n))
   pretty (S.Float α n) = highlightIf α (text (show n))
   pretty (S.Str α str) = highlightIf α (text (show str))
   pretty (S.Constr α c ss) = prettyConstr α c ss
   pretty (S.Record α xss) = prettyRecord text α xss
   pretty (S.Dictionary α sss) = prettyDict pretty α (sss <#> toTuple)
   pretty (S.Matrix α e (x × y) e') = highlightIf α (hspace (init <> quant))
      where
      init = [ text str.arrayLBracket, pretty e, text str.bar ]
      quant = [ parens (hcomma [ text x, text y ]), text (str.in_), pretty e', text str.arrayRBracket ]
   pretty (S.Lambda bs) = hspace [ text str.fun, vert semi (pretty <$> bs) ]
   pretty (S.Project s x) = pretty s :<>: text (str.dot <> x)
   pretty (S.App s s') = hspace [ pretty s, pretty s' ]
   pretty (S.BinaryApp s op s') = parens (hspace [ pretty s, text op, pretty s' ])
   pretty (S.MatchAs s bs) = atop (hspace [ text str.match, pretty s, text str.as ]) (vert semi (pretty <$> bs))
   pretty (S.IfElse s1 s2 s3) =
      hspace [ text str.if_, pretty s1, text str.then_, pretty s2, text str.else_, pretty s3 ]
   pretty (S.ListEmpty α) = highlightIf α nil
   pretty (S.ListNonEmpty α e l) = highlightIf α (text str.lBracket) :<>: pretty e :<>: pretty l
   pretty (S.ListEnum s s') = brackets (hspace [ pretty s, text str.ellipsis, pretty s' ])
   pretty (S.ListComp α s qs) = highlightIf α $ brackets (hspace [ pretty s, text str.bar, hcomma (pretty <$> qs) ])
   pretty (S.Let ds s) = atop (hspace [ text str.let_, vert semi (pretty <$> ds) ])
      (hspace [ text str.in_, pretty s ])
   pretty (S.LetRec h s) = atop (hspace [ text str.let_, vert semi (pretty <$> h) ])
      (hspace [ text str.in_, pretty s ])

instance Highlightable a => Pretty (S.ListRest a) where
   pretty (S.End α) = highlightIf α (text str.rBracket)
   pretty (S.Next α s l) = hspace [ highlightIf α comma, pretty s :<>: pretty l ]

instance Highlightable a => Pretty (String × (NonEmptyList S.Pattern × S.Expr a)) where
   pretty (x × b) = hspace [ text x, pretty b ]

instance Highlightable a => Pretty (NonEmptyList S.Pattern × S.Expr a) where
   pretty (ps × s) = hspace ((pretty <$> NEL.toList ps) <> (text str.equals : pretty s : Nil))

instance Highlightable a => Pretty (S.VarDef a) where
   pretty (S.VarDef p s) = hspace [ pretty p, text str.equals, pretty s ]

instance Highlightable a => Pretty (S.Pattern × S.Expr a) where
   pretty (p × s) = pretty p :<>: text str.lArrow :<>: pretty s

instance Highlightable a => Pretty (S.Qualifier a) where
   pretty (S.Guard e) = pretty e
   pretty (S.Generator p e) = hspace [ pretty p, text str.lArrow, pretty e ]
   pretty (S.Declaration (S.VarDef p e)) = hspace [ text str.let_, pretty p, text str.equals, pretty e ]

instance (Pretty a, Pretty b) => Pretty (a + b) where
   pretty = pretty ||| pretty

instance Pretty S.Pattern where
   pretty (S.PVar x) = text x
   pretty (S.PConstr c ps) = prettyConstr false c ps
   pretty (S.PRecord xps) = prettyRecord text false xps
   pretty (S.PListEmpty) = nil
   pretty (S.PListNonEmpty s l) = text str.lBracket :<>: pretty s :<>: pretty l

instance ToList S.Pattern where
   toList (S.PConstr c (p : p' : Nil)) | c == cCons = p : toList p'
   toList (S.PConstr c Nil) | c == cNil = Nil
   toList _ = error absurd

instance ToPair S.Pattern where
   toPair (S.PConstr c (p : p' : Nil)) | c == cPair = p × p'
   toPair _ = error absurd

instance Pretty S.ListRestPattern where
   pretty S.PEnd = text str.rBracket
   pretty (S.PNext s l) = hspace [ comma, pretty s :<>: pretty l ]
