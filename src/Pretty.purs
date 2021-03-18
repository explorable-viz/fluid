module Pretty (class Pretty, pretty, prettyP, module P) where

import Prelude hiding (absurd, between)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.List (List(..), (:), fromFoldable)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty (toList) as NEL
import Data.Map (toUnfoldable)
import Data.String (Pattern(..), contains) as Data.String
import Text.Pretty (Doc, atop, beside, hcat, render, text, vcat)
import Text.Pretty (render) as P
import Bindings (Binding, Bindings, (↦))
import Bindings (toList) as B
import DataType (Ctr, cCons, cNil, cPair)
import Expr (Cont(..), Elim(..))
import Expr (Expr(..), VarDef(..)) as E
import SExpr (Expr(..), ListRest(..), ListRestPattern(..), Pattern(..), Qualifier(..), VarDef(..)) as S
import Parse (str)
import Util (Endo, type (×), (×), absurd, error, intersperse)
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

space :: Doc
space = text " "

lspace :: Endo Doc
lspace = (:<>:) space

rspace :: Endo Doc
rspace = flip (:<>:) space

lrspace :: Endo Doc
lrspace = between space space -- or: lspace >>> rspace

hspace :: forall f . Foldable f => f Doc -> Doc
hspace = hcat <<< intersperse space <<< fromFoldable

tab :: Doc
tab = text "   "

operator :: String -> Doc
operator = text >>> lrspace

parens :: Endo Doc
parens = between (text "(") (text ")")

hole :: Doc
hole = text "□"

class ToList a where
   toList :: a -> List a

instance toListExpr :: ToList (E.Expr Boolean)  where
   toList (E.Constr _ c (e : e' : Nil))   | c == cCons   = e : toList e'
   toList (E.Constr _ c Nil)              | c == cNil    = Nil
   toList e                                              = error ("Not a list: " <> render (pretty e))

instance toListVal :: ToList (Val Boolean)  where
   toList (V.Constr _ c (v : v' : Nil)) | c == cCons  = v : toList v'
   toList (V.Constr _ c Nil)            | c == cNil   = Nil
   toList v                                           = error ("Not a list: " <> render (pretty v))

class Pretty p where
   pretty :: p -> Doc

instance prettyBool :: Pretty Boolean where
   pretty = text <<< show

instance prettyBindings :: Pretty (Binding t a) => Pretty (Bindings t a) where
   pretty = B.toList >>> pretty

instance prettyList_ :: Pretty a => Pretty (List a) where
   pretty xs = vcat (pretty <$> xs)

-- Render a user-level list reflected as a PureScript list.
prettyList :: forall a. Pretty a => List a -> Doc
prettyList xs = brackets (hcat (intersperse comma (pretty <$> xs)))

instance prettyListRest :: Pretty (S.ListRest Boolean) where
   pretty = pretty <<< listRestToExprs

instance prettyListPatternRest :: Pretty S.ListRestPattern where
   pretty = pretty <<< listRestPatternToPatterns

instance prettyCtr :: Pretty Ctr where
   pretty = show >>> text

-- Cheap hack; revisit.
prettyParensOpt :: forall a . Pretty a => a -> Doc
prettyParensOpt x =
   let doc = pretty x in
   if Data.String.contains (Data.String.Pattern " ") (render doc)
   then parens doc
   else doc

prettyConstr :: forall a . Pretty a => Ctr -> List a -> Doc
prettyConstr c xs
   | c == cPair = case xs of
      x : y : Nil -> parens (pretty x :<>: comma :<>: pretty y)
      _           -> error absurd
   | c == cNil || c == cCons = pretty xs
   | otherwise = pretty c :<>: lspace (hspace (prettyParensOpt <$> xs))

instance prettyExpr :: Pretty (E.Expr Boolean) where
   pretty E.Hole                    = hole
   pretty (E.Int α n)               = highlightIf α (text (show n))
   pretty (E.Float _ n)             = text (show n)
   pretty (E.Str _ str)             = text (show str)
   pretty (E.Var x)                 = text x
   pretty r@(E.Constr _ c es)
      | c == cNil || c == cCons     = prettyList (toList r)
      | otherwise                   = prettyConstr c es
   pretty (E.Matrix _ _ _ _)        = error "todo"
   pretty (E.Op op)                 = parens (text op)
   pretty (E.Let (E.VarDef σ e) e') = atop (hspace [text str.let_, pretty σ, text str.equals, pretty e, text str.in_])
                                           (pretty e')
   pretty (E.LetRec δ e)            = atop (text str.let_ :<>: pretty δ :<>: lspace (text str.in_)) (pretty e)
   pretty (E.Lambda σ)              = rspace (text str.fun) :<>: pretty σ
   pretty (E.App e e')              = pretty e :<>: lspace (pretty e')

instance prettyBinding :: Pretty (t Boolean) => Pretty (Binding t Boolean) where
   pretty (f ↦ σ) = text f :<>: operator "=" :<>: pretty σ

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
      | c == cNil || c == cCons        = prettyList (toList u)
      | otherwise                      = prettyConstr c vs
   pretty (V.Matrix _ (vss × _ × _))   = hcat (pretty <$> fromFoldable (fromFoldable <$> vss))
   pretty (V.Closure ρ δ σ) =
    text "Closure" :<>: text "(" :<>:
    (atop (atop (text "env: " :<>: pretty ρ) (text "defs: " :<>: pretty δ)) (text "elim: " :<>: pretty σ)) :<>: text ")"
   pretty (V.Primitive φ _)            = parens (pretty φ)

instance prettyPrimOp :: Pretty PrimOp where
   pretty _ = text "<prim op>" -- TODO

-- Surface language

listRestToExprs :: forall a . S.ListRest a -> List (S.Expr a)
listRestToExprs (S.End _)        = Nil
listRestToExprs (S.Next _ e l)   = e : listRestToExprs l

listRestPatternToPatterns :: S.ListRestPattern -> List S.Pattern
listRestPatternToPatterns S.PEnd         = Nil
listRestPatternToPatterns (S.PNext π πs) = π : listRestPatternToPatterns πs

instance toListSExpr :: ToList (S.Expr Boolean)  where
   toList (S.Constr _ c (e : e' : Nil)) | c == cCons  = e : toList e'
   toList (S.Constr _ c Nil) | c == cNil              = Nil
   toList (S.ListEmpty _)                             = Nil
   toList (S.ListNonEmpty _ e l)                      = e : listRestToExprs l
   toList _                                           = error absurd

instance prettySExpr :: Pretty (S.Expr Boolean) where
   pretty (S.Var x)                    = text x
   pretty (S.Op op)                    = parens (text op)
   pretty (S.Int α n)                  = highlightIf α (text (show n))
   pretty (S.Float _ n)                = text (show n)
   pretty (S.Str _ str)                = text (show str)
   pretty r@(S.Constr _ c es)
      | c == cNil || c == cCons        = prettyList (toList r)
      | otherwise                      = prettyConstr c es
   pretty (S.Matrix α e (x × y) e')    =
      hspace [text str.arrayLBracket, pretty e, text str.bar, parens (text x :<>: comma :<>: text y),
              text (str.in_), pretty e', text str.arrayRBracket]
   pretty (S.Lambda bs)                = text str.fun :<>: pretty bs
   pretty (S.App s s')                 = pretty s :<>: lspace (pretty s')
   pretty (S.BinaryApp s op s')        = parens (pretty s :<>: operator op :<>: pretty s')
   pretty (S.MatchAs s bs)             = atop (hspace [text str.match, pretty s, text str.as]) (pretty bs)
   pretty (S.IfElse s1 s2 s3)          =
      hspace [text str.if_, pretty s1, text str.then_, pretty s2, text str.else_, pretty s3]
   pretty r@(S.ListEmpty _)            = prettyList (toList r)
   pretty r@(S.ListNonEmpty _ e l)     = prettyList (toList r)
   pretty (S.ListEnum s s')            = brackets (pretty s :<>: operator str.ellipsis :<>: pretty s')
   pretty (S.ListComp _ s qs)          = brackets (pretty s :<>: operator str.bar :<>: pretty qs)
   pretty (S.Let ds s)                 = atop (hspace [text str.let_, pretty ds]) (hspace [text str.in_, pretty s])
   pretty (S.LetRec h s)               = atop (text str.let_ :<>: lspace (pretty h)) (rspace (text str.in_) :<>: pretty s)

instance prettyNonEmptyList :: Pretty a => Pretty (NonEmptyList a) where
   pretty = pretty <<< NEL.toList

instance prettyClause :: Pretty (String × (NonEmptyList S.Pattern × S.Expr Boolean)) where
   pretty (x × b) = text x :<>: lspace (pretty b)

instance prettySBranch :: Pretty (NonEmptyList S.Pattern × S.Expr Boolean) where
   pretty (πs × e) = hspace (pretty <$> NEL.toList πs) :<>: operator str.equals :<>: pretty e

instance prettySVarDef :: Pretty (S.VarDef Boolean) where
   pretty (S.VarDef π e) = pretty π :<>: operator str.equals :<>: pretty e

instance prettyPatternExpr :: Pretty (S.Pattern × S.Expr Boolean) where
   pretty (π × e) = pretty π :<>: text str.lArrow :<>: pretty e

instance prettyQualifier :: Pretty (S.Qualifier Boolean) where
   pretty (S.Guard e)                     = pretty e
   pretty (S.Generator π e)               = pretty π :<>: operator str.lArrow :<>: pretty e
   pretty (S.Declaration (S.VarDef π e))  = hspace [text str.let_, pretty π, text str.equals, pretty e]

instance prettyEither :: (Pretty a, Pretty b) => Pretty (Either a b) where
   pretty (Left p)   = pretty p
   pretty (Right p)  = pretty p

instance prettyPattern :: Pretty S.Pattern where
   pretty (S.PVar x)               = text x
   pretty (S.PConstr ctr πs)       = pretty ctr :<>: lspace (pretty πs)
   pretty (S.PListEmpty)           = text (str.lBracket <> str.rBracket)
   pretty (S.PListNonEmpty π πs)   = pretty (π : listRestPatternToPatterns πs)
