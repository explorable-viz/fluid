module Bindings where

import Prelude
import Data.Foldable (class Foldable, foldMapDefaultL, foldrDefault)
import Data.Set (Set, empty, singleton, union)
import Data.Traversable (class Traversable, sequenceDefault)
import Lattice (class BoundedSlices, class JoinSemilattice, class Slices, botOf, definedJoin, maybeJoin, neg)
import Util (Endo, MayFail, (≞), definitely, error, report, whenever)
import Util.SnocList (SnocList(..), (:-))

type Var = String -- newtype?

varAnon = "_" :: Var

-- Discrete partial order for variables.
mustGeq :: Var -> Var -> Var
mustGeq x y = definitely "greater" (whenever (x == y) x)

data Bind a = Bind Var a
type Bindings a = SnocList (Bind a)

derive instance Functor Bind

key :: forall a . Bind a -> Var
key (x ↦ _) = x

val :: forall a . Bind a -> a
val (_ ↦ v) = v

dom :: forall a . Bindings a -> Set Var
dom Lin           = empty
dom (ρ :- x ↦ _)  = singleton x `union` dom ρ

instance Foldable Bind where
   foldl f b (_ ↦ v) = f b v
   foldr x = foldrDefault x
   foldMap = foldMapDefaultL

instance Traversable Bind where
   traverse f (x ↦ v) = (x ↦ _) <$> f v
   sequence = sequenceDefault

infix 7 Bind as ↦
infixl 4 mustGeq as ⪂

instance Slices a => JoinSemilattice (Bind a) where
   join = definedJoin
   neg = (<$>) neg

instance Slices a => Slices (Bind a) where
   maybeJoin (x ↦ v) (y ↦ v') = (↦) <$> (x ≞ y) <*> maybeJoin v v'

instance BoundedSlices a => BoundedSlices (Bind a) where
   botOf = (<$>) botOf

find :: forall a . Var -> Bindings a -> MayFail a
find x Lin  = report ("variable " <> x <> " not found")
find x (ρ :- x' ↦ v)
   | x == x'   = pure v
   | otherwise = find x ρ

-- Replace by SnocList fold?
foldBindings :: forall a b . (Bind a -> Endo b) -> b -> Bindings a -> b
foldBindings f z (ρ :- x)  = f x (foldBindings f z ρ)
foldBindings _ z Lin       = z

-- In recursive definitions (which in the new design will be the only use of bindings),
-- keys are always unique (there is no hiding, since the definitions are simultaneous).
update :: forall a . Bindings a -> Endo (Bindings a)
update ρ Lin = ρ
update Lin _ = error "Expected order-preserving subsequence."
update (ρ :- x ↦ v) (ρ' :- y ↦ u)
   | x == y    = update ρ ρ' :- x ↦ u
   | otherwise = update ρ (ρ' :- y ↦ u) :- x ↦ v
