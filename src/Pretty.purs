module Pretty (class Pretty, pretty, prettyP, module P) where

import Prelude hiding (absurd, between)
import Data.Foldable (class Foldable)
import Data.List (List(..), (:), fromFoldable)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty (toList) as NEL
import Data.Map (toUnfoldable)
import Data.Profunctor.Choice ((|||))
import Data.String (Pattern(..), contains) as Data.String
import Text.Pretty (Doc, atop, beside, empty, hcat, render, text)
import Text.Pretty (render) as P
import Bindings (Binding, Bindings(..), (↦))
import DataType (Ctr, cCons, cNil, cPair)
import Expr (Cont(..), Elim(..))
import Expr (Expr(..), VarDef(..)) as E
import SExpr (Expr(..), ListRest(..), ListRestPattern(..), Pattern(..), Qualifier(..), VarDef(..)) as S
import Parse (str)
import Util (Endo, type (×), (×), type (+), absurd, error, intersperse)
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

comma :: Doc
comma = rspace (text ",")

semi :: Doc
semi = text ";"

space :: Doc
space = text " "

lspace :: Endo Doc
lspace = (:<>:) space

rspace :: Endo Doc
rspace = flip (:<>:) space

lrspace :: Endo Doc
lrspace = between space space -- or: lspace >>> rspace

hspace :: forall f . Foldable f => f Doc -> Doc
hspace = fromFoldable >>> intersperse space >>> hcat

hcomma :: forall f . Foldable f => f Doc -> Doc
hcomma = fromFoldable >>> intersperse comma >>> hcat

operator :: String -> Doc
operator = text >>> lrspace

parens :: Endo Doc
parens = between (text "(") (text ")")

hole :: Doc
hole = text "□"

null :: Doc
null = empty 0 0

class ToList a where
   toList :: a -> List a

instance toListExpr :: ToList (E.Expr Boolean)  where
   toList (E.Constr _ c (e : e' : Nil))   | c == cCons   = e : toList e'
   toList (E.Constr _ c Nil)              | c == cNil    = Nil
   toList e                                              = error absurd

instance toListVal :: ToList (Val Boolean)  where
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

instance prettyListRest :: Pretty (S.ListRest Boolean) where
   pretty (S.End _)        = pretty str.rBracket
   pretty (S.Next _ s l)   = comma :<>: hspace [pretty s, pretty l]

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
prettyConstr c (x : y : Nil)  | c == cPair   = parens (pretty x :<>: comma :<>: pretty y)
prettyConstr c Nil            | c == cNil    = nil
prettyConstr c (x : y : Nil)  | c == cCons   = parens (hspace [pretty x, pretty cCons, pretty y])
prettyConstr c xs                            = pretty c :<>: lspace (hspace (prettyParensOpt <$> xs))

instance prettyExpr :: Pretty (E.Expr Boolean) where
   pretty E.Hole                    = hole
   pretty (E.Int α n)               = highlightIf α (text (show n))
   pretty (E.Float _ n)             = text (show n)
   pretty (E.Str _ str)             = text (show str)
   pretty (E.Var x)                 = text x
   pretty (E.Constr _ c es)         = prettyConstr c es
   pretty (E.Matrix _ _ _ _)        = error "todo"
   pretty (E.Op op)                 = parens (text op)
   pretty (E.Let (E.VarDef σ e) e') = atop (hspace [text str.let_, pretty σ, text str.equals, pretty e, text str.in_])
                                           (pretty e')
   pretty (E.LetRec δ e)            = atop (hspace [text str.let_, pretty δ, text str.in_]) (pretty e)
   pretty (E.Lambda σ)              = rspace (text str.fun) :<>: pretty σ
   pretty (E.App e e')              = pretty e :<>: lspace (pretty e')

instance prettyRecDefs :: Pretty (Bindings Elim Boolean) where
   pretty Empty               = error absurd -- non-empty
   pretty (Extend Empty fσ)   = pretty fσ
   pretty (Extend δ fσ)       = atop (pretty δ :<>: semi) (pretty fσ)

instance prettyRecDef :: Pretty (Binding Elim Boolean) where
   pretty (f ↦ σ) = hspace [text f, text str.equals, pretty σ]

instance prettyCont :: Pretty (Cont Boolean) where
   pretty ContHole      = hole
   pretty (ContExpr e)  = pretty e
   pretty (ContElim σ)  = pretty σ

instance prettyBranch :: Pretty (Ctr × Cont Boolean) where
   pretty (c × κ) = hspace [text (show c), text str.rArrow, pretty κ]

instance prettyElim :: Pretty (Elim Boolean) where
   pretty (ElimHole)       = hole
   pretty (ElimVar x κ)    = text x :<>: operator str.rArrow :<>: pretty κ
   pretty (ElimConstr κs)  = hcat ((\x -> pretty x :<>: comma) <$> toUnfoldable κs :: List _)

instance prettyVal :: Pretty (Val Boolean) where
   pretty V.Hole                       = hole
   pretty (V.Int α n)                  = highlightIf α (text (show n))
   pretty (V.Float _ n)                = text (show n)
   pretty (V.Str _ str)                = text (show str)
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
   pretty (S.Float _ n)                = text (show n)
   pretty (S.Str _ str)                = text (show str)
   pretty r@(S.Constr _ c es)          = prettyConstr c es
   pretty (S.Matrix α e (x × y) e')    =
      hspace [text str.arrayLBracket, pretty e, text str.bar, parens (text x :<>: comma :<>: text y),
              text (str.in_), pretty e', text str.arrayRBracket]
   pretty (S.Lambda bs)                = text str.fun :<>: vert semi (pretty <$> bs)
   pretty (S.App s s')                 = pretty s :<>: lspace (pretty s')
   pretty (S.BinaryApp s op s')        = parens (pretty s :<>: operator op :<>: pretty s')
   pretty (S.MatchAs s bs)             = atop (hspace [text str.match, pretty s, text str.as]) (vert semi (pretty <$> bs))
   pretty (S.IfElse s1 s2 s3)          =
      hspace [text str.if_, pretty s1, text str.then_, pretty s2, text str.else_, pretty s3]
   pretty (S.ListEmpty _)              = nil
   pretty (S.ListNonEmpty _ e l)       = comma :<>: hspace [pretty e, pretty l]
   pretty (S.ListEnum s s')            = brackets (hspace [pretty s, text str.ellipsis, pretty s'])
   pretty (S.ListComp _ s qs)          = brackets (hspace [pretty s, text str.bar, hcomma (pretty <$> qs)])
   pretty (S.Let ds s)                 = atop (hspace [text str.let_, vert semi (pretty <$> ds)])
                                              (hspace [text str.in_, pretty s])
   pretty (S.LetRec h s)               = atop (hspace [text str.let_, vert semi (pretty <$> h)])
                                              (rspace (text str.in_) :<>: pretty s)

instance prettyClause :: Pretty (String × (NonEmptyList S.Pattern × S.Expr Boolean)) where
   pretty (x × b) = text x :<>: lspace (pretty b)

instance prettySBranch :: Pretty (NonEmptyList S.Pattern × S.Expr Boolean) where
   pretty (ps × s) = hspace ((pretty <$> NEL.toList ps) <> (text str.equals : pretty s : Nil))

instance prettySVarDef :: Pretty (S.VarDef Boolean) where
   pretty (S.VarDef p s) = pretty p :<>: operator str.equals :<>: pretty s

instance prettyPatternExpr :: Pretty (S.Pattern × S.Expr Boolean) where
   pretty (p × s) = pretty p :<>: text str.lArrow :<>: pretty s

instance prettyQualifier :: Pretty (S.Qualifier Boolean) where
   pretty (S.Guard e)                     = pretty e
   pretty (S.Generator p e)               = pretty p :<>: operator str.lArrow :<>: pretty e
   pretty (S.Declaration (S.VarDef p e))  = hspace [text str.let_, pretty p, text str.equals, pretty e]

instance prettyEither :: (Pretty a, Pretty b) => Pretty (a + b) where
   pretty = pretty ||| pretty

instance prettyPattern :: Pretty S.Pattern where
   pretty (S.PVar x)             = text x
   pretty p@(S.PConstr c πs)
      | c == cNil || c == cCons  = prettyList (toList p)
      | c == cPair               = prettyPair (toPair p)
      | otherwise                = prettyConstr c πs
   pretty (S.PListEmpty)         = nil
   pretty (S.PListNonEmpty s l)  = text str.lBracket :<>: hspace [pretty s, pretty l]

instance toListPattern :: ToList S.Pattern  where
   toList (S.PConstr c (p : p' : Nil)) | c == cCons   = p : toList p'
   toList (S.PConstr c Nil)            | c == cNil    = Nil
   toList _                                           = error absurd

instance toPairPattern :: ToPair S.Pattern where
   toPair (S.PConstr c (p : p' : Nil)) | c == cPair   = p × p'
   toPair _                                           = error absurd

instance prettyListPatternRest :: Pretty S.ListRestPattern where
   pretty S.PEnd        = pretty str.rBracket
   pretty (S.PNext s l) = comma :<>: hspace [pretty s, pretty l]
