module EvalGraph where

import Prelude

import Expr
import Graph
import Util (error, type (×), (×))
import Control.Monad
import Control.Monad.State
import Data.Traversable
import Util.Pair (Pair, toTuple)

{-# Allocating addresses #-}

type Fresh a = State Int a

freshVertex :: Fresh Vertex
freshVertex = do
   s <- get
   put (s + 1)
   pure (Vertex (show s))

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
alloc (Let (VarDef σ e) e') = do
   Let <$> (VarDef <$> allocElim σ <*> alloc e) <*> alloc e'
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

-- assignAddr :: forall f a. Traversable f => f a -> State Int (f (a × Vertex))
-- assignAddr = traverse (\a -> freshVertex >>= \v -> pure $ a × v)

{-# Allocating addresses #-}