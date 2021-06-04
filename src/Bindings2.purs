module Bindings2 where

import Prelude
import Bindings (Bindings(..), (:+:))
import Bindings ((↦)) as B
import Lattice (
   class BoundedSlices, class Expandable, class JoinSemilattice, class Slices, botOf, definedJoin, expand, maybeJoin, neg
)
import Util (Endo, MayFail, (≜), (≞), report)
import Util.SnocList (SnocList(..), (:-))

type Var = String -- newtype?

varAnon = "_" :: Var

data Bind a = Bind Var a
type Bindings2 a = SnocList (Bind a)

derive instance functorBind :: Functor Bind

infix 7 Bind as ↦

instance expandableBind :: Expandable a => Expandable (Bind a) where
   expand (x ↦ v) (x' ↦ v') = (x ≜ x') ↦ expand v v'

instance joinSemilatticeBind :: Slices a => JoinSemilattice (Bind a) where
   join = definedJoin
   neg = (<$>) neg

instance slicesBind :: Slices a => Slices (Bind a) where
   maybeJoin (x ↦ v) (y ↦ v') = (↦) <$> (x ≞ y) <*> maybeJoin v v'

instance boundedSlicesBind :: BoundedSlices a => BoundedSlices (Bind a) where
   botOf = (<$>) botOf

-- Temporary conversion from new bindings to old.
asBindings :: forall t a . Bindings2 (t a) -> Bindings t a
asBindings Lin = Empty
asBindings (ρ :- x ↦ v) = asBindings ρ :+: x B.↦ v

asBindings2 :: forall t a . Bindings t a -> Bindings2 (t a)
asBindings2 Empty = Lin
asBindings2 (ρ :+: x B.↦ v) = asBindings2 ρ :- x ↦ v

-- Could simplify these now but not high priority.
find :: forall a . Var -> Bindings2 a -> MayFail a
find x Lin  = report ("variable " <> x <> " not found")
find x (ρ :- x' ↦ v)
   | x == x'   = pure v
   | otherwise = find x ρ

update :: forall a . Bindings2 a -> Bind a -> Bindings2 a
update Lin _ = Lin
update (ρ :- x ↦ v) (x' ↦ v')
   | x == x'    = ρ :- x' ↦ v'
   | otherwise  = update ρ (x' ↦ v') :- x ↦ v

-- Replace by SnocList fold?
foldBindings :: forall a b . (Bind a -> Endo b) -> b -> Bindings2 a -> b
foldBindings f z (ρ :- x)  = f x (foldBindings f z ρ)
foldBindings _ z Lin       = z
