module Expr where

import Prelude hiding (top)
import Control.Apply (lift2)
import Data.List (List)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Bindings (Bindings)
import DataType (Ctr)
import Lattice (class BoundedSlices, class JoinSemilattice, class Slices, (∨), definedJoin, maybeJoin)
import Util (type (×), (×), type (+), (≟), error)

type Var = String

varAnon = "_" :: Var

data Expr a =
   Hole |
   Var Var |
   Op Var |
   Int a Int |
   Float a Number |
   Str a String |
   Constr a Ctr (List (Expr a)) |
   Matrix a (Expr a) (Var × Var) (Expr a) |
   Lambda (Elim a) |
   App (Expr a) (Expr a) |
   BinaryApp (Expr a) Var (Expr a) |
   Let (VarDef a) (Expr a) |
   LetRec (RecDefs a) (Expr a)

-- eliminator in var def is always singleton, with an empty terminal continuation represented by hole
data VarDef a = VarDef (Elim a) (Expr a)
type RecDefs = Bindings Elim

data Elim a =
   ElimHole |
   ElimVar Var (Cont a) |
   ElimConstr (Map Ctr (Cont a))

-- Continuation of an eliminator. None form only used in structured let.
data Cont a =
   ContHole | -- arise in backward slicing, but also used to represent structured let
   ContExpr (Expr a) |
   ContElim (Elim a)

asElim :: forall a . Cont a -> Elim a
asElim ContHole = ElimHole
asElim (ContElim σ) = σ
asElim (ContExpr _) = error "Eliminator expected"

asExpr :: forall a . Cont a -> Expr a
asExpr ContHole = Hole
asExpr (ContElim _) = error "Expression expected"
asExpr (ContExpr e) = e

data Module a = Module (List (VarDef a + RecDefs a))

-- ======================
-- boilerplate
-- ======================
derive instance functorVarDef :: Functor VarDef
derive instance functorExpr :: Functor Expr
derive instance functorCont :: Functor Cont
derive instance functorElim :: Functor Elim

instance joinSemilatticeElim :: JoinSemilattice a => JoinSemilattice (Elim a) where
   join = definedJoin

instance slicesElim :: JoinSemilattice a => Slices (Elim a) where
   maybeJoin ElimHole σ                         = pure σ
   maybeJoin σ ElimHole                         = pure σ
   maybeJoin (ElimVar x κ) (ElimVar x' κ')      = ElimVar <$> x ≟ x' <*> maybeJoin κ κ'
   maybeJoin (ElimConstr κs) (ElimConstr κs')   = ElimConstr <$> maybeJoin κs κs'
   maybeJoin _ _                                = Nothing

instance boundedSlicesElim :: JoinSemilattice a => BoundedSlices (Elim a) where
   botOf = const ElimHole

instance joinSemilatticeCont :: JoinSemilattice a => JoinSemilattice (Cont a) where
   join = definedJoin

instance slicesCont :: JoinSemilattice a => Slices (Cont a) where
   maybeJoin ContHole κ                   = pure κ
   maybeJoin κ ContHole                   = pure κ
   maybeJoin (ContExpr e) (ContExpr e')   = ContExpr <$> maybeJoin e e'
   maybeJoin (ContElim σ) (ContElim σ')   = ContElim <$> maybeJoin σ σ'
   maybeJoin _ _                          = Nothing

instance boundedSlicesCont :: JoinSemilattice a => BoundedSlices (Cont a) where
   botOf = const ContHole

instance joinSemilatticeVarDef :: JoinSemilattice a => JoinSemilattice (VarDef a) where
   join = definedJoin

instance slicesVarDef :: JoinSemilattice a => Slices (VarDef a) where
   maybeJoin (VarDef σ e) (VarDef σ' e') = VarDef <$> maybeJoin σ σ' <*> maybeJoin e e'

instance boundedSlicesExpr :: JoinSemilattice a => BoundedSlices (Expr a) where
   botOf = const Hole

instance joinSemilatticeExpr :: JoinSemilattice a => JoinSemilattice (Expr a) where
   join = definedJoin

instance slicesExpr :: JoinSemilattice a => Slices (Expr a) where
   maybeJoin Hole e                                            = pure e
   maybeJoin e Hole                                            = pure e
   maybeJoin (Var x) (Var x')                                  = Var <$> x ≟ x'
   maybeJoin (Op op) (Op op')                                  = Op <$> op ≟ op'
   maybeJoin (Int α n) (Int α' n')                             = Int (α ∨ α') <$> n ≟ n'
   maybeJoin (Str α str) (Str α' str')                         = Str (α ∨ α') <$> str ≟ str'
   maybeJoin (Float α n) (Float α' n')                         = Float (α ∨ α') <$> n ≟ n'
   maybeJoin (Constr α c es) (Constr α' c' es')                = Constr (α ∨ α') <$> c ≟ c' <*> maybeJoin es es'
   maybeJoin (Matrix α e1 (x × y) e2) (Matrix α' e1' (x' × y') e2') =
      Matrix (α ∨ α') <$> maybeJoin e1 e1' <*> ((x ≟ x') `lift2 (×)` (y ≟ y')) <*> maybeJoin e2 e2'
   maybeJoin (App e1 e2) (App e1' e2')                         = App <$> maybeJoin e1 e1' <*> maybeJoin e2 e2'
   maybeJoin (BinaryApp e1 op e2) (BinaryApp e1' op' e2')      =
      BinaryApp <$> maybeJoin e1 e1' <*> op ≟ op' <*> maybeJoin e2 e2'
   maybeJoin (Lambda σ) (Lambda σ')                            = Lambda <$> maybeJoin σ σ'
   maybeJoin (Let def e) (Let def' e')                         = Let <$> maybeJoin def def' <*> maybeJoin e e'
   maybeJoin (LetRec δ e) (LetRec δ' e')                       = LetRec <$> maybeJoin δ δ' <*> maybeJoin e e'
   maybeJoin _ _                                               = Nothing
