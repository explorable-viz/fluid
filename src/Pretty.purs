module Pretty (class Pretty, pretty, module P) where

import Prelude hiding (absurd, between)
import Data.Either (Either(..))
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
import SExpr (Expr(..), ListRest(..), ListRestPattern(..), Pattern(..), Qualifier(..), VarDef(..))
import Parse (str)
import Util (Endo, type (×), (×), absurd, error, intersperse)
import Val (PrimOp, Val)
import Val (Val(..)) as V

infixl 5 beside as :<>:

between :: Doc -> Doc -> Endo Doc
between l r doc = l :<>: doc :<>: r

brackets :: Endo Doc
brackets = between (text str.lBracket) (text str.rBracket)

highlightIf :: Boolean -> Endo Doc
highlightIf false   = identity
highlightIf true    = between (text "_") (text "_")

comma :: Doc
comma = text "," :<>: space

space :: Doc
space = text " "

tab :: Doc
tab = text "   "

operator :: String -> Doc
operator = text >>> between space space

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

instance prettyListRest :: Pretty (ListRest Boolean) where
   pretty = pretty <<< listRestToExprs

instance prettyListPatternRest :: Pretty ListRestPattern where
   pretty = pretty <<< listRestPatternToPatterns

instance prettyCtr :: Pretty Ctr where
   pretty = show >>> text

-- Cheap hack to make progress on migrating some tests; need to revisit.
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
   | otherwise = pretty c :<>: space :<>: hcat (intersperse space $ map prettyParensOpt xs)

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
   pretty (E.Let (E.VarDef σ e) e') =
      atop (text str.let_ :<>: pretty σ :<>: operator str.equals :<>: pretty e :<>: space :<>: text str.in_) (pretty e')
   pretty (E.LetRec δ e)            =
      atop (text str.let_ :<>: pretty δ :<>: text str.in_) (pretty e)
   pretty (E.Lambda σ)              = text "fun " :<>: pretty σ
   pretty (E.App e e')              = pretty e :<>: space :<>: pretty e'

instance prettyBinding :: Pretty (t Boolean) => Pretty (Binding t Boolean) where
   pretty (f ↦ σ) = text f :<>: operator "=" :<>: pretty σ

instance prettyCont :: Pretty (Cont Boolean) where
   pretty ContHole      = hole
   pretty (ContExpr e)  = pretty e
   pretty (ContElim σ)  = pretty σ

instance prettyBranch :: Pretty (Ctr × Cont Boolean) where
   pretty (c × κ) = text (show c) :<>: operator "->" :<>: pretty κ

instance prettyElim :: Pretty (Elim Boolean) where
   pretty (ElimHole)       = hole
   pretty (ElimVar x κ)    = text x :<>: operator "->" :<>: pretty κ
   pretty (ElimConstr κs)  = hcat (map (\x -> pretty x :<>: comma) (toUnfoldable κs :: List _))

instance prettyVal :: Pretty (Val Boolean) where
   pretty V.Hole                       = hole
   pretty (V.Int α n)                  = highlightIf α (text (show n))
   pretty (V.Float _ n)                = text (show n)
   pretty (V.Str _ str)                = text (show str)
   pretty u@(V.Constr _ c vs)
      | c == cNil || c == cCons        = pretty (toList u)
      | otherwise                      = prettyConstr c vs
   pretty (V.Matrix _ (vss × _ × _))   = hcat (pretty <$> fromFoldable (fromFoldable <$> vss))
   pretty (V.Closure ρ δ σ) =
    text "Closure" :<>: text "(" :<>:
    (atop (atop (text "env: " :<>: pretty ρ) (text "defs: " :<>: pretty δ)) (text "elim: " :<>: pretty σ)) :<>: (text ")")
   pretty (V.Primitive φ _)            = parens (pretty φ)

instance prettyPrimOp :: Pretty PrimOp where
   pretty _ = text "<prim op>" -- TODO

-- Surface language

listRestToExprs :: forall a . ListRest a -> List (Expr a)
listRestToExprs (End _) = Nil
listRestToExprs (Next _ e l) = e : listRestToExprs l

listRestPatternToPatterns :: ListRestPattern -> List Pattern
listRestPatternToPatterns PEnd         = Nil
listRestPatternToPatterns (PNext π πs) = π : listRestPatternToPatterns πs

instance toListSExpr :: ToList (Expr Boolean)  where
   toList (Constr _ c (e : e' : Nil)) | c == cCons   = e : toList e'
   toList (Constr _ c Nil) | c == cNil               = Nil
   toList (ListEmpty _)                              = Nil
   toList (ListNonEmpty _ e l)                       = e : listRestToExprs l
   toList _                                          = error absurd

instance prettySExpr :: Pretty (Expr Boolean) where
   pretty (Var x)                   = text x
   pretty (Op op)                   = parens (text op)
   pretty (Int α n)                 = highlightIf α (text (show n))
   pretty (Float _ n)               = text (show n)
   pretty (Str _ str)               = text (show str)
   pretty r@(Constr _ c es)
      | c == cNil || c == cCons     = prettyList (toList r)
      | otherwise                   = prettyConstr c es
   pretty (Matrix α e (x × y) e')   =
      text str.arrayLBracket :<>: pretty e :<>: space :<>: text str.bar :<>: space :<>: pretty e' :<>: space :<>: text str.arrayRBracket
   pretty (Lambda bs)               = text str.fun :<>: pretty bs
   pretty (App s s')                = pretty s :<>: space :<>: pretty s'
   pretty (BinaryApp s op s')       = parens (pretty s :<>: operator op :<>: pretty s')
   pretty (MatchAs s bs)            =
      text str.match :<>: space :<>: pretty s :<>: space :<>: text str.as :<>: space :<>: pretty bs
   pretty (IfElse s1 s2 s3)         =
      text "if " :<>: pretty s1 :<>: text " then " :<>: pretty s2 :<>: text " else " :<>: pretty s3
   pretty r@(ListEmpty _)           = prettyList (toList r)
   pretty r@(ListNonEmpty _ e l)    = prettyList (toList r)
   pretty (ListEnum s s')           = brackets (pretty s :<>: text " .. " :<>: pretty s')
   pretty (ListComp _ s qs)         = brackets (pretty s :<>: text " | " :<>: pretty qs)
   pretty (Let ds s)                = atop (text str.let_ :<>: space :<>: pretty ds) (text "in " :<>: pretty s)
   pretty (LetRec h s)              = atop (text str.let_ :<>: space :<>: pretty h) (text "in " :<>: pretty s)

instance prettyNonEmptyList :: Pretty a => Pretty (NonEmptyList a) where
   pretty = pretty <<< NEL.toList

instance prettyClause :: Pretty (String × (NonEmptyList Pattern × Expr Boolean)) where
   pretty (x × b) = text x :<>: space :<>: pretty b

instance prettySBranch :: Pretty (NonEmptyList Pattern × Expr Boolean) where
   pretty (πs × e) = hcat (intersperse space (pretty <$> NEL.toList πs)) :<>: space :<>: text str.equals :<>: space :<>: pretty e

instance prettySVarDef :: Pretty (VarDef Boolean) where
   pretty (VarDef π e) = pretty π :<>: text " = " :<>: pretty e

instance prettyPatternExpr :: Pretty (Pattern × Expr Boolean) where
   pretty (π × e) = pretty π :<>: text " -> " :<>: pretty e

instance prettyQualifier :: Pretty (Qualifier Boolean) where
   pretty (Guard e)                    = pretty e
   pretty (Generator π e)              = pretty π :<>: text " <- " :<>: pretty e
   pretty (Declaration (VarDef π e))   = text str.let_ :<>: space :<>: pretty π :<>: text " = " :<>: pretty e

instance prettyPatt :: (Pretty a, Pretty b) => Pretty (Either a b) where
   pretty (Left p)   = pretty p
   pretty (Right p)  = pretty p

instance prettyPattern :: Pretty Pattern where
   pretty (PVar x)               = text x
   pretty (PConstr ctr πs)       = pretty ctr :<>: space :<>: pretty πs
   pretty (PListEmpty)           = text (str.lBracket <> str.rBracket)
   pretty (PListNonEmpty π πs)   = pretty (π : listRestPatternToPatterns πs)

prettyProgram :: E.Expr Boolean -> Doc
prettyProgram e = atop (pretty e) (text "")
