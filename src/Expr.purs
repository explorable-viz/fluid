module Expr where

import Prelude hiding (absurd, top)

import Bind (Var)
import Control.Apply (lift2)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldl, foldrDefault, foldMapDefaultL)
import Data.List (List(..), (:), zipWith)
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set, empty, unions)
import Data.Set (fromFoldable) as S
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Data.Tuple (snd)
import DataType (Ctr)
import Dict (Dict)
import Lattice (class BoundedJoinSemilattice, class Expandable, class JoinSemilattice, Raw, (∨), expand)
import Util (type (+), type (×), error, shapeMismatch, singleton, (×), (≜))
import Util.Pair (Pair(..))
import Util.Map (keys, asMaplet)
import Util.Set ((\\), (∪))

-- Deviate from POPL paper by having closures depend on originating lambda or letrec
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
   | Lambda a (Elim a)
   | Project (Expr a) Var
   | App (Expr a) (Expr a)
   | Let (VarDef a) (Expr a)
   | LetRec (RecDefs a) (Expr a)

-- eliminator here is a singleton with null terminal continuation
data VarDef a = VarDef (Elim a) (Expr a)
data RecDefs a = RecDefs a (Dict (Elim a))

data Elim a
   = ElimVar Var (Cont a)
   | ElimConstr (Dict (Cont a))
   | ElimRecord (Set Var) (Cont a)

-- Continuation of an eliminator branch.
data Cont a
   = ContExpr (Expr a)
   | ContElim (Elim a)

asElim :: forall a. Cont a -> Elim a
asElim (ContElim σ) = σ
asElim _ = error "Eliminator expected"

asExpr :: forall a. Cont a -> Expr a
asExpr (ContExpr e) = e
asExpr _ = error "Expression expected"

newtype Module a = Module (List (VarDef a + RecDefs a))

class FV a where
   fv :: a -> Set Var

instance FV (Expr a) where
   fv (Var x) = singleton x
   fv (Op op) = singleton op
   fv (Int _ _) = empty
   fv (Float _ _) = empty
   fv (Str _ _) = empty
   fv (Record _ xes) = unions (fv <$> xes)
   fv (Dictionary _ ees) = unions ((\(Pair e e') -> fv e ∪ fv e') <$> ees)
   fv (Constr _ _ es) = unions (fv <$> es)
   fv (Matrix _ e1 _ e2) = fv e1 ∪ fv e2
   fv (Lambda _ σ) = fv σ
   fv (Project e _) = fv e
   fv (App e1 e2) = fv e1 ∪ fv e2
   fv (Let def e) = fv def ∪ (fv e \\ bv def)
   fv (LetRec ρ e) = fv ρ ∪ fv e

instance FV (Elim a) where
   fv (ElimVar x κ) = fv κ \\ singleton x
   fv (ElimConstr m) = unions (fv <$> m)
   fv (ElimRecord _ κ) = fv κ

instance FV (Cont a) where
   fv (ContElim σ) = fv σ
   fv (ContExpr e) = fv e

instance FV (VarDef a) where
   fv (VarDef _ e) = fv e

instance FV (RecDefs a) where
   fv (RecDefs _ ρ) = fv ρ

instance FV a => FV (Dict a) where
   fv ρ = unions (fv <$> ρ) \\ S.fromFoldable (keys ρ)

instance (FV a, FV b) => FV (a × b) where
   fv (x × y) = fv x ∪ fv y

class BV a where
   bv :: a -> Set Var

-- Bound variables, defined only for singleton eliminators.
instance BV (Elim a) where
   bv (ElimVar x κ) = singleton x ∪ bv κ
   bv (ElimConstr m) = bv (snd (asMaplet m))
   bv (ElimRecord _ κ) = bv κ

instance BV (VarDef a) where
   bv (VarDef σ _) = bv σ

instance BV (Cont a) where
   bv (ContElim σ) = bv σ
   bv (ContExpr _) = empty

instance JoinSemilattice a => JoinSemilattice (Elim a) where
   join (ElimVar x κ) (ElimVar x' κ') = ElimVar (x ≜ x') (κ ∨ κ')
   join (ElimConstr cκs) (ElimConstr cκs') = ElimConstr (cκs ∨ cκs')
   join (ElimRecord xs κ) (ElimRecord ys κ') = ElimRecord (xs ≜ ys) (κ ∨ κ')
   join _ _ = shapeMismatch unit

instance BoundedJoinSemilattice a => Expandable (Elim a) (Raw Elim) where
   expand (ElimVar x κ) (ElimVar x' κ') = ElimVar (x ≜ x') (expand κ κ')
   expand (ElimConstr cκs) (ElimConstr cκs') = ElimConstr (expand cκs cκs')
   expand (ElimRecord xs κ) (ElimRecord ys κ') = ElimRecord (xs ≜ ys) (expand κ κ')
   expand _ _ = shapeMismatch unit

instance JoinSemilattice a => JoinSemilattice (Cont a) where
   join (ContExpr e) (ContExpr e') = ContExpr (e ∨ e')
   join (ContElim σ) (ContElim σ') = ContElim (σ ∨ σ')
   join _ _ = shapeMismatch unit

instance BoundedJoinSemilattice a => Expandable (Cont a) (Raw Cont) where
   expand (ContExpr e) (ContExpr e') = ContExpr (expand e e')
   expand (ContElim σ) (ContElim σ') = ContElim (expand σ σ')
   expand _ _ = shapeMismatch unit

instance JoinSemilattice a => JoinSemilattice (VarDef a) where
   join (VarDef σ e) (VarDef σ' e') = VarDef (σ ∨ σ') (e ∨ e')

instance BoundedJoinSemilattice a => Expandable (VarDef a) (Raw VarDef) where
   expand (VarDef σ e) (VarDef σ' e') = VarDef (expand σ σ') (expand e e')

instance JoinSemilattice a => JoinSemilattice (RecDefs a) where
   join (RecDefs α ρ) (RecDefs α' ρ') = RecDefs (α ∨ α') (ρ ∨ ρ')

instance BoundedJoinSemilattice a => Expandable (RecDefs a) (Raw RecDefs) where
   expand (RecDefs α ρ) (RecDefs _ ρ') = RecDefs α (expand ρ ρ')

instance JoinSemilattice a => JoinSemilattice (Expr a) where
   join (Var x) (Var x') = Var (x ≜ x')
   join (Op op) (Op op') = Op (op ≜ op')
   join (Int α n) (Int α' n') = Int (α ∨ α') (n ≜ n')
   join (Str α str) (Str α' str') = Str (α ∨ α') (str ≜ str')
   join (Float α n) (Float α' n') = Float (α ∨ α') (n ≜ n')
   join (Record α xes) (Record α' xes') = Record (α ∨ α') (xes ∨ xes')
   join (Dictionary α ees) (Dictionary α' ees') = Dictionary (α ∨ α') (ees ∨ ees')
   join (Constr α c es) (Constr α' c' es') = Constr (α ∨ α') (c ≜ c') (es ∨ es') -- TODO: assert consistentWith
   join (Matrix α e1 (x × y) e2) (Matrix α' e1' (x' × y') e2') =
      Matrix (α ∨ α') (e1 ∨ e1') ((x ≜ x') × (y ≜ y')) (e2 ∨ e2')
   join (Lambda α σ) (Lambda α' σ') = Lambda (α ∨ α') (σ ∨ σ')
   join (Project e x) (Project e' x') = Project (e ∨ e') (x ≜ x')
   join (App e1 e2) (App e1' e2') = App (e1 ∨ e1') (e2 ∨ e2')
   join (Let def e) (Let def' e') = Let (def ∨ def') (e ∨ e')
   join (LetRec ρ e) (LetRec ρ' e') = LetRec (ρ ∨ ρ') (e ∨ e')
   join _ _ = shapeMismatch unit

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
   expand (Lambda α σ) (Lambda _ σ') = Lambda α (expand σ σ')
   expand (Project e x) (Project e' x') = Project (expand e e') (x ≜ x')
   expand (App e1 e2) (App e1' e2') = App (expand e1 e1') (expand e2 e2')
   expand (Let def e) (Let def' e') = Let (expand def def') (expand e e')
   expand (LetRec ρ e) (LetRec ρ' e') = LetRec (expand ρ ρ') (expand e e')
   expand _ _ = shapeMismatch unit

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
derive instance Functor RecDefs
derive instance Foldable RecDefs
derive instance Traversable RecDefs
derive instance Newtype (Module a) _
derive instance Functor Module

-- For terms of a fixed shape.
instance Apply Expr where
   apply (Var x) (Var x') = Var (x ≜ x')
   apply (Op op) (Op _) = Op op
   apply (Int fα n) (Int α n') = Int (fα α) (n ≜ n')
   apply (Float fα n) (Float α n') = Float (fα α) (n ≜ n')
   apply (Str fα s) (Str α s') = Str (fα α) (s ≜ s')
   apply (Record fα fxes) (Record α xes) = Record (fα α) (((<*>) <$> fxes) <*> xes)
   apply (Dictionary fα fxes) (Dictionary α xes) = Dictionary (fα α) (zipWith (lift2 (<*>)) fxes xes)
   apply (Constr fα c fes) (Constr α c' es) = Constr (fα α) (c ≜ c') (zipWith (<*>) fes es)
   apply (Matrix fα fe1 (x × y) fe2) (Matrix α e1 (x' × y') e2) =
      Matrix (fα α) (fe1 <*> e1) ((x ≜ x') × (y ≜ y')) (fe2 <*> e2)
   apply (Lambda fα fσ) (Lambda α σ) = Lambda (fα α) (fσ <*> σ)
   apply (Project fe x) (Project e _) = Project (fe <*> e) x
   apply (App fe1 fe2) (App e1 e2) = App (fe1 <*> e1) (fe2 <*> e2)
   apply (Let (VarDef fσ fe1) fe2) (Let (VarDef σ e1) e2) = Let (VarDef (fσ <*> σ) (fe1 <*> e1)) (fe2 <*> e2)
   apply (LetRec fρ fe) (LetRec ρ e) = LetRec (fρ <*> ρ) (fe <*> e)
   apply _ _ = shapeMismatch unit

instance Apply Elim where
   apply (ElimVar x fk) (ElimVar _ k) = ElimVar x (fk <*> k)
   apply (ElimConstr fk) (ElimConstr k) = ElimConstr (((<*>) <$> fk) <*> k)
   apply (ElimRecord xs fk) (ElimRecord _ k) = ElimRecord xs (fk <*> k)
   apply _ _ = shapeMismatch unit

instance Apply Cont where
   apply (ContExpr f) (ContExpr e) = ContExpr (f <*> e)
   apply (ContElim fσ) (ContElim σ) = ContElim (fσ <*> σ)
   apply _ _ = shapeMismatch unit

instance Apply VarDef where
   apply (VarDef fσ fe) (VarDef σ e) = VarDef (fσ <*> σ) (fe <*> e)

instance Apply RecDefs where
   apply (RecDefs fα fρ) (RecDefs α ρ) = RecDefs (fα α) (((<*>) <$> fρ) <*> ρ)

-- Apply instance for Either no good here as doesn't assume fixed shape.
instance Apply Module where
   apply (Module Nil) (Module Nil) = Module Nil
   apply (Module (Left fdef : fdefs)) (Module (Left def : defs)) =
      Module (Left (fdef <*> def) : unwrap (apply (Module fdefs) (Module defs)))
   apply (Module (Right fdef : fdefs)) (Module (Right def : defs)) =
      Module (Right (fdef <*> def) : unwrap (apply (Module fdefs) (Module defs)))
   apply _ _ = shapeMismatch unit

-- Foldable instance for Either only considers Right case.
foldlModuleDef :: forall a b. (b -> a -> b) -> b -> VarDef a + RecDefs a -> b
foldlModuleDef f acc (Left def) = foldl f acc def
foldlModuleDef f acc (Right def) = foldl f acc def

instance Foldable Module where
   foldl _ acc (Module Nil) = acc
   foldl f acc (Module (Left def : defs)) =
      foldl (foldlModuleDef f) (foldl f acc def) defs
   foldl f acc (Module (Right def : defs)) =
      foldl (foldlModuleDef f) (foldl f acc def) defs

   foldr f = foldrDefault f
   foldMap f = foldMapDefaultL f

instance Traversable Module where
   traverse _ (Module Nil) = pure (Module Nil)
   traverse f (Module (Left def : ds)) =
      Module <$> ((Left <$> traverse f def) `lift2 (:)` (unwrap <$> traverse f (Module ds)))
   traverse f (Module (Right def : ds)) =
      Module <$> ((Right <$> traverse f def) `lift2 (:)` (unwrap <$> traverse f (Module ds)))

   sequence = sequenceDefault

derive instance Eq a => Eq (Expr a)
derive instance Eq a => Eq (Elim a)
derive instance Eq a => Eq (Cont a)
derive instance Eq a => Eq (VarDef a)
derive instance Eq a => Eq (RecDefs a)

derive instance Ord a => Ord (Expr a)
derive instance Ord a => Ord (Elim a)
derive instance Ord a => Ord (Cont a)
derive instance Ord a => Ord (VarDef a)
derive instance Ord a => Ord (RecDefs a)
