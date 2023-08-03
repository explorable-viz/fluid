module EvalGraph where

import Prelude

import Expr
import Graph
import Util (error, type (×), (×))
import Control.Monad
import Control.Monad.State
import Data.Traversable
import Util.Pair (Pair, toTuple)

{-
data Expr a
   = Var Var
   | Op Var
   | Int a Int
   | Float a Number
   | Str a String
   | Record a (Dict (Expr a))
   | Dictionary a (List (Pair (Expr a))) -- constructor name Dict borks (import of same name)
   | Constr a Ctr (List (Expr a))
   | Matrix a (Expr a) (Var × Var) (Expr a)
   | Lambda (Elim a)
   | Project (Expr a) Var
   | App (Expr a) (Expr a)
   | Let (VarDef a) (Expr a)
   | LetRec (RecDefs a) (Expr a)
   | Sugar (Sugar' Expr) (Expr a)
-}

type Fresh a = State Int a

freshVertex :: Fresh Vertex
freshVertex = do
   s <- get
   put (s + 1)
   pure (Vertex (show s))

-- assignAddr :: forall f a. Traversable f => f a -> State Int (f (a × Vertex))
-- assignAddr = traverse (\a -> freshVertex >>= \v -> pure $ a × v)

alloc :: forall a. Expr a -> Fresh (Expr (a × Vertex))
alloc (Var x) = pure $ Var x
alloc (Op op) = pure $ Op op
alloc (Int α n) = do
   v <- freshVertex
   pure $ Int (α × v) n
alloc (Float α n) = do
   v <- freshVertex
   pure $ Float (α × v) n
alloc (Str α str) = do
   v <- freshVertex
   pure $ Str (α × v) str
alloc (Record α xes) = do
   v <- freshVertex
   Record (α × v) <$> traverse alloc xes
alloc (Dictionary α ees) = do
   v <- freshVertex
   Dictionary (α × v) <$> traverse (traverse alloc) ees
alloc (Constr α c es) = do
   v <- freshVertex
   Constr (α × v) c <$> traverse alloc es
alloc (Matrix α e1 (x × y) e2) = do
   v <- freshVertex
   Matrix (α × v) <$> alloc e1 <*> pure (x × y) <*> alloc e2
alloc (Lambda σ) = Lambda <$> allocElim σ
alloc (Project e x) = Project <$> alloc e <*> pure x
alloc (App e e') = App <$> alloc e <*> alloc e'
alloc (Let (VarDef σ e) e') = do
   Let <$> (VarDef <$> allocElim σ <*> alloc e) <*> alloc e'
alloc (LetRec ρ e) = LetRec <$> allocRecDefs ρ <*> alloc e
alloc (Sugar s e) = Sugar s <$> alloc e

allocElim :: forall a. Elim a -> Fresh (Elim (a × Vertex))
allocElim _ = error "to do"

allocRecDefs :: forall a. RecDefs a -> Fresh (RecDefs (a × Vertex))
allocRecDefs _ = error "to do"