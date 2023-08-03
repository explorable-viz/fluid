module EvalGraph where

import Prelude

import Bindings (varAnon)
import Expr
import Graph
import Control.Monad.State
import Data.Either (Either(..), note)
import Data.List (List(..), (:), length, range, singleton, unzip, zip)
import Data.Set as S
import Data.Set (Set)
import Data.Traversable (traverse)
import DataType (Ctr, arity, consistentWith, dataTypeFor, showCtr)
import Dict (fromFoldable, empty, get, singleton, disjointUnion, lookup, keys, unzip) as D
import Util (MayFail, error, type (×), (×), with, report, check)
import Util.Pair (Pair, toTuple)
import Pretty
import Val (Fun(..), Val(..)) as V
import Val (Val, Env)

{-# Allocating addresses #-}

type Fresh a = State Int a

freshVertex :: Fresh Vertex
freshVertex = do
   s <- get
   put (s + 1)
   pure (Vertex (show s))

runAlloc :: forall a. Expr a -> (Expr Vertex) × Int
runAlloc e = runState (alloc e) 0

alloc :: forall a. Expr a -> Fresh (Expr Vertex)
alloc (Var x) = pure $ Var x
alloc (Op op) = pure $ Op op
alloc (Int _ n) = do
   α <- freshVertex
   pure $ Int α n
alloc (Float _ n) = do
   α <- freshVertex
   pure $ Float α n
alloc (Str _ str) = do
   α <- freshVertex
   pure $ Str α str
alloc (Record _ xes) = do
   α <- freshVertex
   Record α <$> traverse alloc xes
alloc (Dictionary _ ees) = do
   α <- freshVertex
   Dictionary α <$> traverse (traverse alloc) ees
alloc (Constr _ c es) = do
   α <- freshVertex
   Constr α c <$> traverse alloc es
alloc (Matrix _ e1 (x × y) e2) = do
   α <- freshVertex
   Matrix α <$> alloc e1 <*> pure (x × y) <*> alloc e2
alloc (Lambda σ) = Lambda <$> allocElim σ
alloc (Project e x) = Project <$> alloc e <*> pure x
alloc (App e e') = App <$> alloc e <*> alloc e'
alloc (Let (VarDef σ e) e') = Let <$> (VarDef <$> allocElim σ <*> alloc e) <*> alloc e'
alloc (LetRec ρ e) = LetRec <$> traverse allocElim ρ <*> alloc e
alloc (Sugar s e) = Sugar s <$> alloc e

allocElim :: forall a. Elim a -> Fresh (Elim Vertex)
allocElim (ElimVar x κ) = ElimVar x <$> allocCont κ
allocElim (ElimConstr m) = ElimConstr <$> traverse (allocCont) m
allocElim (ElimRecord xs κ) = ElimRecord xs <$> allocCont κ
allocElim (ElimSug sσ κ) = ElimSug sσ <$> allocElim κ

allocCont :: forall a. Cont a -> Fresh (Cont Vertex)
allocCont ContNone = pure ContNone
allocCont (ContExpr e) = ContExpr <$> alloc e
allocCont (ContElim σ) = ContElim <$> allocElim σ

{-# Matching #-}
patternMismatch :: String -> String -> String
patternMismatch s s' = "Pattern mismatch: found " <> s <> ", expected " <> s'

match :: Val Vertex -> Elim Vertex -> MayFail (Env Vertex × Cont Vertex × Set Vertex)
match v (ElimVar x κ)
   | x == varAnon = pure (D.empty × κ × S.empty)
   | otherwise = pure (D.singleton x v × κ × S.empty)
match (V.Constr α c vs) (ElimConstr m) = do
   with "Pattern mismatch" $ S.singleton c `consistentWith` D.keys m
   κ <- note ("Incomplete patterns: no branch for " <> showCtr c) (D.lookup c m)
   γ × κ' × αs <- matchMany vs κ
   pure (γ × κ' × (S.insert α αs))
match v (ElimConstr m) = do
   d <- dataTypeFor $ D.keys m
   report $ patternMismatch (prettyP v) (show d)
match (V.Record α xvs) (ElimRecord xs κ) = do
   check (S.subset xs (S.fromFoldable $ D.keys xvs))
      $ patternMismatch (show (D.keys xvs)) (show xs)
   let xs' = xs # S.toUnfoldable
   γ × κ' × αs <- matchMany (xs' <#> flip D.get xvs) κ
   pure $ γ × κ' × (S.insert α αs)
match v (ElimRecord xs _) = report (patternMismatch (prettyP v) (show xs))
match v (ElimSug _ κ) = match v κ

matchMany :: List (Val Vertex) -> Cont Vertex -> MayFail (Env Vertex × Cont Vertex × Set Vertex)
matchMany Nil κ = pure (D.empty × κ × S.empty)
matchMany (v : vs) (ContElim σ) = do
   γ × κ' × αs <- match v σ
   γ' × κ'' × βs <- matchMany vs κ'
   pure $ γ `D.disjointUnion` γ' × κ'' × (αs `S.union` βs)
matchMany (_ : vs) (ContExpr _) = report $
   show (length vs + 1) <> " extra argument(s) to constructor/record; did you forget parentheses in lambda pattern?"
matchMany _ _ = error "absurd"
