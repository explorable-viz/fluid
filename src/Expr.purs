module Expr where

import Prelude hiding (absurd, top)

import Bindings (Var)
import Control.Apply (lift2)
import Data.Foldable (class Foldable)
import Data.Either (Either(..))
import Data.List (List(..), (:), zipWith)
import Data.Set (Set, difference, empty, singleton, union, unions)
import Data.Set (fromFoldable) as S
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (snd)
import DataType (Ctr, consistentWith)
import Dict (Dict, keys, asSingletonMap)
import Dict (apply2) as D
import Lattice (class BoundedJoinSemilattice, class Expandable, class JoinSemilattice, Raw, (∨), definedJoin, expand, maybeJoin, neg)
import Util (type (+), type (×), both, error, report, (×), (≜), (≞))
import Util.Pair (Pair, toTuple)

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

-- eliminator here is a singleton with null terminal continuation
data VarDef a = VarDef (Elim a) (Expr a)
type RecDefs a = Dict (Elim a)
newtype RecDefs' a = RecDefs' (RecDefs a)

data Elim a
   = ElimVar Var (Cont a)
   | ElimConstr (Dict (Cont a))
   | ElimRecord (Set Var) (Cont a)

-- Continuation of an eliminator branch.
data Cont a
   = ContNone
   | -- null continuation, used in let bindings/module variable bindings
     ContExpr (Expr a)
   | ContElim (Elim a)

asElim :: forall a. Cont a -> Elim a
asElim (ContElim σ) = σ
asElim _ = error "Eliminator expected"

asExpr :: forall a. Cont a -> Expr a
asExpr (ContExpr e) = e
asExpr _ = error "Expression expected"

data Module a = Module (List (VarDef a + RecDefs a))
class FV a where
   fv :: a -> Set Var

instance FV (Expr a) where
   fv (Var x) = singleton x
   fv (Op op) = singleton op
   fv (Int _ _) = empty
   fv (Float _ _) = empty
   fv (Str _ _) = empty
   fv (Record _ xes) = unions (fv <$> xes)
   fv (Dictionary _ ees) = unions ((unions <<< (fv # both) <<< toTuple) <$> ees)
   fv (Constr _ _ es) = unions (fv <$> es)
   fv (Matrix _ e1 _ e2) = union (fv e1) (fv e2)
   fv (Lambda σ) = fv σ
   fv (Project e _) = fv e
   fv (App e1 e2) = fv e1 `union` fv e2
   fv (Let def e) = fv def `union` (fv e `difference` bv def)
   fv (LetRec ρ e) = unions (fv <$> ρ) `union` fv e

instance FV (Elim a) where
   fv (ElimVar x κ) = fv κ `difference` singleton x
   fv (ElimConstr m) = unions (fv <$> m)
   fv (ElimRecord _ κ) = fv κ

instance FV (Cont a) where
   fv ContNone = empty
   fv (ContElim σ) = fv σ
   fv (ContExpr e) = fv e

instance FV (VarDef a) where
   fv (VarDef _ e) = fv e

instance FV a => FV (Dict a) where
   fv ρ = (unions $ (fv <$> ρ)) `difference` (S.fromFoldable $ keys ρ)

class BV a where
   bv :: a -> Set Var

-- Bound variables, defined only for singleton eliminators.
instance BV (Elim a) where
   bv (ElimVar x κ) = singleton x `union` bv κ
   bv (ElimConstr m) = bv (snd (asSingletonMap m))
   bv (ElimRecord _ κ) = bv κ

instance BV (VarDef a) where
   bv (VarDef σ _) = bv σ

instance BV (Cont a) where
   bv ContNone = empty
   bv (ContElim σ) = bv σ
   bv (ContExpr _) = empty

-- ======================
-- boilerplate
-- ======================
derive instance Functor VarDef
derive instance Foldable VarDef
derive instance Traversable VarDef
derive instance Functor Cont
derive instance Foldable Cont
derive instance Traversable Cont
derive instance Functor Elim
derive instance Foldable Elim
derive instance Traversable Elim
derive instance Functor Expr
derive instance Foldable Expr
derive instance Traversable Expr
derive instance Functor Module

traverseModule :: forall a m b. Monad m => (a -> m b) -> Module a -> m (Module b)
traverseModule _ (Module Nil) = pure (Module Nil)
traverseModule f (Module (Left (VarDef σ e) : ds)) = do
   VarDef σ' e' <- traverse f (VarDef σ e)
   Module ds'   <- traverseModule f (Module ds)
   pure (Module (Left (VarDef σ' e') : ds'))
traverseModule f (Module (Right ρ : ds)) = do
   ρ' <- traverse (traverse f) ρ
   Module ds'   <- traverseModule f (Module ds)
   pure (Module (Right ρ' : ds'))

derive instance Eq a => Eq (Expr a)
derive instance Eq a => Eq (VarDef a)
derive instance Eq a => Eq (Elim a)
derive instance Eq a => Eq (Cont a)

instance Apply Expr where
   apply (Var x) (Var _) = Var x
   apply (Op op) (Op _) = Op op
   apply (Int fα n) (Int α _) = Int (fα α) n
   apply (Float fα n) (Float α _) = Float (fα α) n
   apply (Str fα s) (Str α _) = Str (fα α) s
   apply (Record fα fxvs) (Record α xvs) = Record (fα α) (D.apply2 fxvs xvs)
   apply (Dictionary fα fxvs) (Dictionary α xvs) = Dictionary (fα α) (zipWith (lift2 (<*>)) fxvs xvs)
   apply (Constr fα c fes) (Constr α _ es) = Constr (fα α) c (zipWith (<*>) fes es)
   apply (Matrix fα fe1 (x × y) fe2) (Matrix α e1 (_ × _) e2) = Matrix (fα α) (fe1 <*> e1) (x × y) (fe2 <*> e2)
   apply (Lambda fσ) (Lambda σ) = Lambda (fσ <*> σ)
   apply (Project fe x) (Project e _) = Project (fe <*> e) x
   apply (App fe1 fe2) (App e1 e2) = App (fe1 <*> e1) (fe2 <*> e2)
   apply (Let (VarDef fσ fe1) fe2) (Let (VarDef σ e1) e2) = Let (VarDef (fσ <*> σ) (fe1 <*> e1)) (fe2 <*> e2)
   apply (LetRec fρ fe) (LetRec ρ e) = LetRec (D.apply2 fρ ρ) (fe <*> e)
   apply _ _ = error "Apply Expr: shape mismatch"

instance Apply Elim where
   apply (ElimVar x fk) (ElimVar _ k) = ElimVar x (fk <*> k)
   apply (ElimConstr fk) (ElimConstr k) = ElimConstr (D.apply2 fk k)
   apply (ElimRecord xs fk) (ElimRecord _ k) = ElimRecord xs (fk <*> k)
   apply _ _ = error "Apply Elim: shape mismatch"

instance Apply Cont where
   apply ContNone ContNone = ContNone
   apply (ContExpr f) (ContExpr e) = ContExpr (f <*> e)
   apply (ContElim fσ) (ContElim σ) = ContElim (fσ <*> σ)
   apply _ _ = error "Apply Cont: shape mismatch"

instance JoinSemilattice a => JoinSemilattice (Elim a) where
   maybeJoin (ElimVar x κ) (ElimVar x' κ') = ElimVar <$> (x ≞ x') <*> maybeJoin κ κ'
   maybeJoin (ElimConstr cκs) (ElimConstr cκs') =
      ElimConstr <$> ((keys cκs `consistentWith` keys cκs') *> maybeJoin cκs cκs')
   maybeJoin (ElimRecord xs κ) (ElimRecord ys κ') = ElimRecord <$> (xs ≞ ys) <*> maybeJoin κ κ'
   maybeJoin _ _ = report "Incompatible eliminators"

   join σ = definedJoin σ
   neg = (<$>) neg

instance BoundedJoinSemilattice a => Expandable (Elim a) (Raw Elim) where
   expand (ElimVar x κ) (ElimVar x' κ') = ElimVar (x ≜ x') (expand κ κ')
   expand (ElimConstr cκs) (ElimConstr cκs') = ElimConstr (expand cκs cκs')
   expand (ElimRecord xs κ) (ElimRecord ys κ') = ElimRecord (xs ≜ ys) (expand κ κ')
   expand _ _ = error "Incompatible eliminators"

instance JoinSemilattice a => JoinSemilattice (Cont a) where
   maybeJoin ContNone ContNone = pure ContNone
   maybeJoin (ContExpr e) (ContExpr e') = ContExpr <$> maybeJoin e e'
   maybeJoin (ContElim σ) (ContElim σ') = ContElim <$> maybeJoin σ σ'
   maybeJoin _ _ = report "Incompatible continuations"

   join κ = definedJoin κ
   neg = (<$>) neg

instance BoundedJoinSemilattice a => Expandable (Cont a) (Raw Cont) where
   expand ContNone ContNone = ContNone
   expand (ContExpr e) (ContExpr e') = ContExpr (expand e e')
   expand (ContElim σ) (ContElim σ') = ContElim (expand σ σ')
   expand _ _ = error "Incompatible continuations"

instance JoinSemilattice a => JoinSemilattice (VarDef a) where
   join def = definedJoin def
   maybeJoin (VarDef σ e) (VarDef σ' e') = VarDef <$> maybeJoin σ σ' <*> maybeJoin e e'
   neg = (<$>) neg

instance BoundedJoinSemilattice a => Expandable (VarDef a) (Raw VarDef) where
   expand (VarDef σ e) (VarDef σ' e') = VarDef (expand σ σ') (expand e e')

instance JoinSemilattice a => JoinSemilattice (Expr a) where
   maybeJoin (Var x) (Var x') = Var <$> (x ≞ x')
   maybeJoin (Op op) (Op op') = Op <$> (op ≞ op')
   maybeJoin (Int α n) (Int α' n') = Int (α ∨ α') <$> (n ≞ n')
   maybeJoin (Str α str) (Str α' str') = Str (α ∨ α') <$> (str ≞ str')
   maybeJoin (Float α n) (Float α' n') = Float (α ∨ α') <$> (n ≞ n')
   maybeJoin (Record α xes) (Record α' xes') = Record (α ∨ α') <$> maybeJoin xes xes'
   maybeJoin (Dictionary α ees) (Dictionary α' ees') = Dictionary (α ∨ α') <$> maybeJoin ees ees'
   maybeJoin (Constr α c es) (Constr α' c' es') = Constr (α ∨ α') <$> (c ≞ c') <*> maybeJoin es es'
   maybeJoin (Matrix α e1 (x × y) e2) (Matrix α' e1' (x' × y') e2') =
      Matrix (α ∨ α') <$> maybeJoin e1 e1' <*> ((x ≞ x') `lift2 (×)` (y ≞ y')) <*> maybeJoin e2 e2'
   maybeJoin (Lambda σ) (Lambda σ') = Lambda <$> maybeJoin σ σ'
   maybeJoin (Project e x) (Project e' x') = Project <$> maybeJoin e e' <*> (x ≞ x')
   maybeJoin (App e1 e2) (App e1' e2') = App <$> maybeJoin e1 e1' <*> maybeJoin e2 e2'
   maybeJoin (Let def e) (Let def' e') = Let <$> maybeJoin def def' <*> maybeJoin e e'
   maybeJoin (LetRec ρ e) (LetRec ρ' e') = LetRec <$> maybeJoin ρ ρ' <*> maybeJoin e e'
   maybeJoin _ _ = report "Incompatible expressions"

   join e = definedJoin e
   neg = (<$>) neg

instance BoundedJoinSemilattice a => Expandable (Expr a) (Raw Expr) where
   expand (Var x) (Var x') = Var (x ≜ x')
   expand (Op op) (Op op') = Op (op ≜ op')
   expand (Int α n) (Int _ n') = Int α (n ≜ n')
   expand (Str α str) (Str _ str') = Str α (str ≜ str')
   expand (Float α n) (Float _ n') = Float α (n ≜ n')
   expand (Record α xes) (Record _ xes') = Record α (expand xes xes')
   expand (Dictionary α ees) (Dictionary _ ees') = Dictionary α (expand ees ees')
   expand (Constr α c es) (Constr _ c' es') = Constr α (c ≜ c') (expand es es')
   expand (Matrix α e1 (x × y) e2) (Matrix _ e1' (x' × y') e2') =
      Matrix α (expand e1 e1') ((x ≜ x') × (y ≜ y')) (expand e2 e2')
   expand (Lambda σ) (Lambda σ') = Lambda (expand σ σ')
   expand (Project e x) (Project e' x') = Project (expand e e') (x ≜ x')
   expand (App e1 e2) (App e1' e2') = App (expand e1 e1') (expand e2 e2')
   expand (Let def e) (Let def' e') = Let (expand def def') (expand e e')
   expand (LetRec ρ e) (LetRec ρ' e') = LetRec (expand ρ ρ') (expand e e')
   expand _ _ = error "Incompatible expressions"
