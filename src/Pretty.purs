module Pretty (class Pretty, pretty, prettyP, module P) where

import Prelude hiding (absurd,between)

import Bindings (Bindings, Bind, (↦))
import Data.Foldable (class Foldable)
import Data.List (List(..), (:), fromFoldable)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty (toList) as NEL
import Data.Profunctor.Choice ((|||))
import Data.String (Pattern(..), contains) as Data.String
import DataType (Ctr, cCons, cNil, cPair)
import Expr (Cont(..), Elim(..))
import Expr (Expr(..), VarDef(..)) as E
import Parse (str)
import SExpr (Expr(..), ListRest(..), ListRestPattern(..), Pattern(..), Qualifier(..), VarDef(..)) as S
import Text.Pretty (Doc, atop, beside, empty, hcat, render, text)
import Text.Pretty (render) as P
import Util (Endo, type (×), (×), type (+), absurd, error, intersperse)
import Util.SnocList (SnocList(..), (:-))
import Util.SnocList (reverse) as S
import Val (PrimOp, Val)
import Val (Val(..)) as V

infixl 5 beside as :<>:

prettyP :: forall a . Pretty a => a -> String
prettyP = pretty >>> render

between :: Doc -> Doc -> Endo Doc
between l r doc = l :<>: doc :<>: r

brackets :: Endo Doc
brackets = between (text str.lBracket) (text str.rBracket)

highlightIf :: Boolean -> Endo Doc
highlightIf false   = identity
highlightIf true    = between (text "_") (text "_")

colon :: Doc
colon = text str.colon

comma :: Doc
comma = text ","

semi :: Doc
semi = text ";"

space :: Doc
space = text " "

hspace :: forall f . Foldable f => f Doc -> Doc
hspace = fromFoldable >>> intersperse space >>> hcat

hcomma :: forall f . Foldable f => f Doc -> Doc
hcomma = fromFoldable >>> intersperse (comma :<>: space) >>> hcat

parens :: Endo Doc
parens = between (text "(") (text ")")

hole :: Doc
hole = text "□"

null :: Doc
null = empty 0 0

class ToList a where
   toList :: a -> List a

instance toListExpr :: ToList (E.Expr Boolean) where
   toList (E.Constr _ c (e : e' : Nil))   | c == cCons   = e : toList e'
   toList (E.Constr _ c Nil)              | c == cNil    = Nil
   toList e                                              = error absurd

instance toListVal :: ToList (Val Boolean) where
   toList (V.Constr _ c (v : v' : Nil)) | c == cCons  = v : toList v'
   toList (V.Constr _ c Nil)            | c == cNil   = Nil
   toList v                                           = error absurd

class ToPair a where
   toPair :: a -> a × a

instance toPairExpr :: ToPair (E.Expr Boolean) where
   toPair (E.Constr _ c (e : e' : Nil))   | c == cPair   = e × e'
   toPair e                                              = error absurd

instance toPairVal :: ToPair (Val Boolean) where
   toPair (V.Constr _ c (v : v' : Nil))   | c == cPair   = v × v'
   toPair v                                              = error absurd

class Pretty p where
   pretty :: p -> Doc

instance prettyString :: Pretty String where
   pretty = text

instance prettyBool :: Pretty Boolean where
   pretty = show >>> pretty

vert :: forall f . Foldable f => Doc -> f Doc -> Doc
vert delim = fromFoldable >>> vert'
   where vert' :: List Doc -> Doc
         vert' Nil          = null
         vert' (x : Nil)    = x
         vert' (x : y : xs) = atop (x :<>: delim) (vert' (y : xs))

-- Render a user-level list reflected as a PureScript list.
prettyList :: forall a . Pretty a => List a -> Doc
prettyList xs = brackets (hcomma (pretty <$> xs))

-- Render a user-level pair reflected as a PureScript pair.
prettyPair :: forall a . Pretty a => a × a -> Doc
prettyPair (x × y) = parens (hcomma [pretty x, pretty y])

instance prettyCtr :: Pretty Ctr where
   pretty = show >>> pretty

-- Cheap hack; revisit.
prettyParensOpt :: forall a . Pretty a => a -> Doc
prettyParensOpt x =
   let doc = pretty x in
   if Data.String.contains (Data.String.Pattern " ") (render doc)
   then parens doc
   else doc

nil :: Doc
nil = text (str.lBracket <> str.rBracket)

prettyConstr :: forall a . Pretty a => Ctr -> List a -> Doc
prettyConstr c (x : y : Nil)  | c == cPair   = parens (hcomma [pretty x, pretty y])
prettyConstr c Nil            | c == cNil    = nil
prettyConstr c (x : y : Nil)  | c == cCons   = parens (hspace [pretty x, pretty cCons, pretty y])
prettyConstr c xs                            = hspace (pretty c : (prettyParensOpt <$> xs))

prettyRecord :: forall a . Pretty a => Bindings a -> Doc
prettyRecord xvs =
   xvs <#> (\(x ↦ v) -> hspace [text x :<>: colon, pretty v])
   # S.reverse >>> hcomma >>> between (text "{") (text "}")

instance prettyExpr :: Pretty (E.Expr Boolean) where
   pretty (E.Hole α)                = hole
   pretty (E.Var x)                 = text x
   pretty (E.Int α n)               = highlightIf α (text (show n))
   pretty (E.Float _ n)             = text (show n)
   pretty (E.Str _ str)             = text (show str)
   pretty (E.Record _ xes)          = prettyRecord xes
   pretty (E.Constr _ c es)         = prettyConstr c es
   pretty (E.Matrix _ _ _ _)        = error "todo"
   pretty (E.Lambda σ)              = hspace [text str.fun, pretty σ]
   pretty (E.Op op)                 = parens (text op)
   pretty (E.Let (E.VarDef σ e) e') = atop (hspace [text str.let_, pretty σ, text str.equals, pretty e, text str.in_])
                                           (pretty e')
   pretty (E.LetRec δ e)            = atop (hspace [text str.let_, pretty δ, text str.in_]) (pretty e)
   pretty (E.RecordLookup e x)      = error "todo"
   pretty (E.App e e')              = hspace [pretty e, pretty e']

instance prettyRecDefs :: Pretty (SnocList (Bind (Elim Boolean))) where
   pretty Lin          = error absurd -- non-empty
   pretty (Lin :- xσ)  = pretty xσ
   pretty (δ :- xσ)    = atop (pretty δ :<>: semi) (pretty xσ)

instance prettyRecDef :: Pretty (Bind (Elim Boolean)) where
   pretty (x ↦ σ) = hspace [text x, text str.equals, pretty σ]

instance prettyRecordVal :: Pretty (SnocList (Bind (Val Boolean))) where
   pretty = prettyRecord

instance prettyCont :: Pretty (Cont Boolean) where
   pretty (ContHole α)  = hole
   pretty (ContExpr e)  = pretty e
   pretty (ContElim σ)  = pretty σ

instance prettyBranch :: Pretty (Ctr × Cont Boolean) where
   pretty (c × κ) = hspace [text (show c), text str.rArrow, pretty κ]

instance prettyElim :: Pretty (Elim Boolean) where
   pretty (ElimHole α)        = hole
   pretty (ElimVar x κ)       = hspace [text x, text str.rArrow, pretty κ]
   pretty (ElimConstr κs)     = hcomma (pretty <$> κs) -- looks dodgy
   pretty (ElimRecord xs κ)   = error "todo"

instance prettyVal :: Pretty (Val Boolean) where
   pretty (V.Hole _)                   = hole
   pretty (V.Int α n)                  = highlightIf α (text (show n))
   pretty (V.Float α n)                = highlightIf α (text (show n))
   pretty (V.Str α str)                = highlightIf α (text (show str))
   pretty (V.Record α xvs)             = highlightIf α (pretty xvs)
   pretty u@(V.Constr _ c vs)
      | c == cNil || c == cCons        = prettyList (toList u) -- list values always printed using list notation
      | otherwise                      = prettyConstr c vs
   pretty (V.Matrix _ (vss × _ × _))   = vert comma (((<$>) pretty >>> hcomma) <$> vss)
   pretty (V.Closure ρ δ σ)            = text "<closure>"
   pretty (V.Primitive φ _)            = parens (pretty φ)

instance prettyPrimOp :: Pretty PrimOp where
   pretty _ = text "<prim op>" -- TODO

-- Surface language

instance toPairSExpr :: ToPair (S.Expr Boolean) where
   toPair (S.Constr _ c (s : s' : Nil))   | c == cPair   = s × s'
   toPair s                                              = error ("Not a pair: " <> prettyP s)

instance prettySExpr :: Pretty (S.Expr Boolean) where
   pretty (S.Var x)                    = text x
   pretty (S.Op op)                    = parens (text op)
   pretty (S.Int α n)                  = highlightIf α (text (show n))
   pretty (S.Float α n)                = highlightIf α (text (show n))
   pretty (S.Str α str)                = highlightIf α (text (show str))
   pretty (S.Constr α c es)            = prettyConstr c es
   pretty (S.Record α xes)             = prettyRecord xes
   pretty (S.Matrix α e (x × y) e')    = highlightIf α (hspace (init <> quant))
      where
      init = [text str.arrayLBracket, pretty e, text str.bar]
      quant = [parens (hcomma [text x, text y]), text (str.in_), pretty e', text str.arrayRBracket]
   pretty (S.Lambda bs)                = text str.fun :<>: vert semi (pretty <$> bs)
   pretty (S.RecordLookup _ _)         = error "todo"
   pretty (S.App s s')                 = hspace [pretty s, pretty s']
   pretty (S.BinaryApp s op s')        = parens (hspace [pretty s, text op, pretty s'])
   pretty (S.MatchAs s bs)             = atop (hspace [text str.match, pretty s, text str.as]) (vert semi (pretty <$> bs))
   pretty (S.IfElse s1 s2 s3)          =
      hspace [text str.if_, pretty s1, text str.then_, pretty s2, text str.else_, pretty s3]
   pretty (S.ListEmpty α)              = highlightIf α nil
   pretty (S.ListNonEmpty α e l)       = highlightIf α (text str.lBracket) :<>: pretty e :<>: pretty l
   pretty (S.ListEnum s s')            = brackets (hspace [pretty s, text str.ellipsis, pretty s'])
   pretty (S.ListComp _ s qs)          = brackets (hspace [pretty s, text str.bar, hcomma (pretty <$> qs)])
   pretty (S.Let ds s)                 = atop (hspace [text str.let_, vert semi (pretty <$> ds)])
                                              (hspace [text str.in_, pretty s])
   pretty (S.LetRec h s)               = atop (hspace [text str.let_, vert semi (pretty <$> h)])
                                              (hspace [text str.in_, pretty s])

instance prettyListRest :: Pretty (S.ListRest Boolean) where
   pretty (S.End α)        = highlightIf α (text str.rBracket)
   pretty (S.Next α s l)   = hspace [highlightIf α comma, pretty s :<>: pretty l]

instance prettyClause :: Pretty (String × (NonEmptyList S.Pattern × S.Expr Boolean)) where
   pretty (x × b) = hspace [text x, pretty b]

instance prettySBranch :: Pretty (NonEmptyList S.Pattern × S.Expr Boolean) where
   pretty (ps × s) = hspace ((pretty <$> NEL.toList ps) <> (text str.equals : pretty s : Nil))

instance prettySVarDef :: Pretty (S.VarDef Boolean) where
   pretty (S.VarDef p s) = hspace [pretty p, text str.equals, pretty s]

instance prettyPatternExpr :: Pretty (S.Pattern × S.Expr Boolean) where
   pretty (p × s) = pretty p :<>: text str.lArrow :<>: pretty s

instance prettyQualifier :: Pretty (S.Qualifier Boolean) where
   pretty (S.Guard e)                     = pretty e
   pretty (S.Generator p e)               = hspace [pretty p, text str.lArrow, pretty e]
   pretty (S.Declaration (S.VarDef p e))  = hspace [text str.let_, pretty p, text str.equals, pretty e]

instance prettyEither :: (Pretty a, Pretty b) => Pretty (a + b) where
   pretty = pretty ||| pretty

instance prettyPattern :: Pretty S.Pattern where
   pretty (S.PVar x)             = text x
   pretty p@(S.PConstr c ps)
      | c == cNil || c == cCons  = prettyList (toList p)
      | c == cPair               = prettyPair (toPair p)
      | otherwise                = prettyConstr c ps
   pretty (S.PRecord xps)        = prettyRecord xps
   pretty (S.PListEmpty)         = nil
   pretty (S.PListNonEmpty s l)  = text str.lBracket :<>: pretty s :<>: pretty l

instance toListPattern :: ToList S.Pattern  where
   toList (S.PConstr c (p : p' : Nil)) | c == cCons   = p : toList p'
   toList (S.PConstr c Nil)            | c == cNil    = Nil
   toList _                                           = error absurd

instance toPairPattern :: ToPair S.Pattern where
   toPair (S.PConstr c (p : p' : Nil)) | c == cPair   = p × p'
   toPair _                                           = error absurd

instance prettyListPatternRest :: Pretty S.ListRestPattern where
   pretty S.PEnd        = text str.rBracket
   pretty (S.PNext s l) = hspace [comma, pretty s :<>: pretty l]
