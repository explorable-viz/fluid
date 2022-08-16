module Bindings2 where

import Prelude
import Data.Foldable (class Foldable, foldMapDefaultL, foldrDefault)
import Data.Set (Set, empty, singleton, union)
import Data.Traversable (class Traversable, sequenceDefault)
import Lattice2 (class BoundedSlices, class JoinSemilattice, class Slices, botOf, definedJoin, maybeJoin, neg)
import Util2 (Endo, MayFail, (≞), definitely, report, whenever)
import Util.SnocList2 (SnocList(..), (:-))

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
infixl 5 update as ◃
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

update :: forall a . Bindings a -> Bind a -> Bindings a
update Lin _ = Lin
update (ρ :- x ↦ v) (x' ↦ v')
   | x == x'    = ρ :- x' ↦ v'
   | otherwise  = update ρ (x' ↦ v') :- x ↦ v
