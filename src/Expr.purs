module Expr where

import Prelude hiding (absurd, top)
import Control.Apply (lift2)
import Data.List (List)
import Data.Map (Map)
import Bindings (Bindings, Var, (⪂))
import Bindings2 (Bindings2)
import DataType (Ctr)
import Lattice (
   class BoundedSlices, class Expandable, class JoinSemilattice, class Slices, (∨), definedJoin, expand, maybeJoin, neg
)
import Util (type (×), (×), type (+), (≞), (≜), (⪄), absurd, error, report)
import Util.SnocList (SnocList)

data Expr a =
   Hole a |
   Var Var |
   Op Var |
   Int a Int |
   Float a Number |
   Str a String |
   Record a (Bindings2 (Expr a)) |
   Constr a Ctr (List (Expr a)) |
   Matrix a (Expr a) (Var × Var) (Expr a) |
   Lambda (Elim a) |
   App (Expr a) (Expr a) |
   Let (VarDef a) (Expr a) |
   LetRec (RecDefs2 a) (Expr a)

-- eliminator in var def is always singleton, with an empty terminal continuation represented by hole
data VarDef a = VarDef (Elim a) (Expr a)
type RecDefs = Bindings Elim
type RecDefs2 a = Bindings2 (Elim a)

data Elim a =
   ElimHole a |
   ElimVar Var (Cont a) |
   ElimConstr (Map Ctr (Cont a)) |
   ElimRecord (SnocList Var) (Cont a)

-- Continuation of an eliminator branch.
data Cont a =
   ContHole a | -- arise in backward slicing, but also used to represent structured let
   ContExpr (Expr a) |
   ContElim (Elim a)

asElim :: forall a . Cont a -> Elim a
asElim (ContHole α)  = ElimHole α
asElim (ContElim σ)  = σ
asElim (ContExpr _)  = error "Eliminator expected"

asExpr :: forall a . Cont a -> Expr a
asExpr (ContHole α)  = Hole α
asExpr (ContElim _)  = error "Expression expected"
asExpr (ContExpr e)  = e

data Module a = Module (List (VarDef a + RecDefs a))

-- ======================
-- boilerplate
-- ======================
derive instance functorVarDef :: Functor VarDef
derive instance functorExpr :: Functor Expr
derive instance functorCont :: Functor Cont
derive instance functorElim :: Functor Elim

instance joinSemilatticeElim :: JoinSemilattice (Elim Boolean) where
   join = definedJoin
   neg = (<$>) neg

instance slicesElim :: Slices (Elim Boolean) where
   maybeJoin (ElimHole false) σ                    = pure σ
   maybeJoin (ElimHole true) σ                     = pure (ElimHole true)
   maybeJoin σ (ElimHole false)                    = pure σ
   maybeJoin σ (ElimHole true)                     = pure (ElimHole true)
   maybeJoin (ElimVar x κ) (ElimVar x' κ')         = ElimVar <$> (x ≞ x') <*> maybeJoin κ κ'
   maybeJoin (ElimConstr κs) (ElimConstr κs')      = ElimConstr <$> maybeJoin κs κs'
   maybeJoin (ElimRecord xs κ) (ElimRecord ys κ')  = ElimRecord <$> (xs ≞ ys) <*> maybeJoin κ κ'
   maybeJoin _ _                                   = report "Incompatible eliminators"

instance boundedSlicesElim :: BoundedSlices (Elim Boolean) where
   botOf = const (ElimHole false)

instance joinSemilatticeCont :: JoinSemilattice (Cont Boolean) where
   join = definedJoin
   neg = (<$>) neg

instance slicesCont :: Slices (Cont Boolean) where
   maybeJoin (ContHole false) κ           = pure κ
   maybeJoin (ContHole true) κ            = pure (ContHole true)
   maybeJoin κ (ContHole false)           = pure κ
   maybeJoin κ (ContHole true)            = pure (ContHole true)
   maybeJoin (ContExpr e) (ContExpr e')   = ContExpr <$> maybeJoin e e'
   maybeJoin (ContElim σ) (ContElim σ')   = ContElim <$> maybeJoin σ σ'
   maybeJoin _ _                          = report "Incompatible continuations"

instance boundedSlicesCont :: BoundedSlices (Cont Boolean) where
   botOf = const (ContHole false)

instance joinSemilatticeVarDef :: JoinSemilattice (VarDef Boolean) where
   join = definedJoin
   neg = (<$>) neg

instance slicesVarDef :: Slices (VarDef Boolean) where
   maybeJoin (VarDef σ e) (VarDef σ' e') = VarDef <$> maybeJoin σ σ' <*> maybeJoin e e'

instance boundedSlicesExpr :: BoundedSlices (Expr Boolean) where
   botOf = const (Hole false)

instance joinSemilatticeExpr :: JoinSemilattice (Expr Boolean) where
   join = definedJoin
   neg = (<$>) neg

instance slicesExpr :: Slices (Expr Boolean) where
   maybeJoin (Hole false) e                                    = pure e
   maybeJoin (Hole true) e                                     = pure (Hole true)
   maybeJoin e (Hole false)                                    = pure e
   maybeJoin e (Hole true)                                     = pure (Hole true)
   maybeJoin (Var x) (Var x')                                  = Var <$> (x ≞ x')
   maybeJoin (Op op) (Op op')                                  = Op <$> (op ≞ op')
   maybeJoin (Int α n) (Int α' n')                             = Int (α ∨ α') <$> (n ≞ n')
   maybeJoin (Str α str) (Str α' str')                         = Str (α ∨ α') <$> (str ≞ str')
   maybeJoin (Float α n) (Float α' n')                         = Float (α ∨ α') <$> (n ≞ n')
   maybeJoin (Record α xes) (Record α' xes')                   = Record (α ∨ α') <$> maybeJoin xes xes'
   maybeJoin (Constr α c es) (Constr α' c' es')                = Constr (α ∨ α') <$> (c ≞ c') <*> maybeJoin es es'
   maybeJoin (Matrix α e1 (x × y) e2) (Matrix α' e1' (x' × y') e2') =
      Matrix (α ∨ α') <$> maybeJoin e1 e1' <*> ((x ≞ x') `lift2 (×)` (y ≞ y')) <*> maybeJoin e2 e2'
   maybeJoin (App e1 e2) (App e1' e2')                         = App <$> maybeJoin e1 e1' <*> maybeJoin e2 e2'
   maybeJoin (Lambda σ) (Lambda σ')                            = Lambda <$> maybeJoin σ σ'
   maybeJoin (Let def e) (Let def' e')                         = Let <$> maybeJoin def def' <*> maybeJoin e e'
   maybeJoin (LetRec δ e) (LetRec δ' e')                       = LetRec <$> maybeJoin δ δ' <*> maybeJoin e e'
   maybeJoin _ _                                               = report "Incompatible expressions"

instance exprExpandable :: Expandable (Expr Boolean) where
   expand e (Hole false)                        = e
   expand (Hole _) e@(Var x)                    = e
   expand (Hole _) e@(Op op)                    = e
   expand (Hole α) e@(Int β n)                  = Int (α ⪄ β) n
   expand (Hole α) e@(Float β n)                = Float (α ⪄ β) n
   expand (Hole α) e@(Str β str)                = Str (α ⪄ β) str
   expand (Hole α) (Record β xes)               = Record (α ⪄ β) (expand (map (const (Hole α)) <$> xes) xes)
   expand (Hole α) (Constr β c es)              = Constr (α ⪄ β) c (expand (Hole α) <$> es)
   expand (Hole α) (Matrix β e1 (x × y) e2)     = Matrix (α ⪄ β) (expand (Hole α) e1) (x × y) (expand (Hole α) e2)
   expand (Hole α) (Lambda σ)                   = Lambda (expand (ElimHole α) σ)
   expand (Hole α) (App e1 e2)                  = App (expand (Hole α) e1) (expand (Hole α) e2)
   expand (Hole α) (Let (VarDef σ e1) e2) =
      Let (VarDef (expand (ElimHole α) σ) (expand (Hole α) e1)) (expand (Hole α) e2)
   expand (Hole α) (LetRec h e)                 = LetRec (expand (map (const (ElimHole α)) <$> h) h) (expand (Hole α) e)
   expand (Var x) (Var x')                      = Var (x ≜ x')
   expand (Op op) (Op op')                      = Op (op ≜ op')
   expand (Int α n) (Int β n')                  = Int (α ⪄ β) (n ≜ n')
   expand (Float α n) (Float β n')              = Float (α ⪄ β) (n ≜ n')
   expand (Str α str) (Str β str')              = Str (α ⪄ β) (str ≜ str')
   expand (Record α xes) (Record β xes')        = Record (α ⪄ β) (expand xes xes')
   expand (Constr α c es) (Constr β c' es')     = Constr (α ⪄ β) (c ≜ c') (expand es es')
   expand (Matrix α e1 (x1 × y1) e2) (Matrix β e1' (x2 × y2) e2') =
      Matrix (α ⪄ β) (expand e1 e1') ((x1 ≜ x2) × (y1 ≜ y2)) (expand e2 e2')
   expand (Lambda σ) (Lambda σ')                = Lambda (expand σ σ')
   expand (App e1 e2) (App e1' e2')             = App (expand e1 e1') (expand e2 e2')
   expand (Let (VarDef σ e1) e2)
          (Let (VarDef σ' e1') e2')             = Let (VarDef (expand σ σ') (expand e1 e1')) (expand e2 e2')
   expand (LetRec h e) (LetRec h' e')           = LetRec (expand h h') (expand e e')
   expand _ _                                   = error absurd

instance elimExpandable :: Expandable (Elim Boolean) where
   expand σ (ElimHole false)                    = σ
   expand (ElimHole α) (ElimVar x κ)            = ElimVar x (expand (ContHole α) κ)
   expand (ElimHole α) (ElimConstr m)           = ElimConstr (expand (ContHole α) <$> m)
   expand (ElimHole α) (ElimRecord xs κ)        = ElimRecord xs (expand (ContHole α) κ)
   expand (ElimVar x κ) (ElimVar x' κ')         = ElimVar (x ⪂ x') (expand κ κ')
   expand (ElimConstr m) (ElimConstr m')        = ElimConstr (expand m m')
   expand (ElimRecord xs κ) (ElimRecord ys κ')  = ElimRecord (xs ⪄ ys) (expand κ κ')
   expand _ _                                   = error absurd

instance contExpandable :: Expandable (Cont Boolean) where
   expand κ (ContHole false)           = κ
   expand (ContHole α) (ContExpr e)    = ContExpr (expand (Hole α) e)
   expand (ContHole α) (ContElim σ)    = ContElim (expand (ElimHole α) σ)
   expand (ContExpr e) (ContExpr e')   = ContExpr (expand e e')
   expand (ContElim σ) (ContElim σ')   = ContElim (expand σ σ')
   expand _ _                          = error absurd
