module Bindings where

import Prelude hiding (absurd)
import Data.List (List(..), (:), singleton)
import Lattice (
   class BoundedSlices, class Expandable, class JoinSemilattice, class Slices,
   botOf, definedJoin, expand, maybeJoin, neg
)
import Util (Endo, MayFail, type (×), (×), (≞), (≜), absurd, error, fromJust, report, whenever)
import Util.SnocList (SnocList(..), (:-))

type Var = String -- newtype?

varAnon = "_" :: Var

-- Discrete partial order for variables.
mustGeq :: Var -> Var -> Var
mustGeq x y = fromJust "Must be greater" (whenever (x == y) x)

infixl 4 mustGeq as ⪂

data Binding t a = Binding Var (t a)
data Bindings t a = Empty | Extend (Bindings t a) (Binding t a)

infix 6 Binding as ↦
infixl 5 Extend as :+:
infixl 5 update as ◃

find :: forall t a . Var -> Bindings t a -> MayFail (t a)
find x Empty  = report ("variable " <> x <> " not found")
find x (ρ :+: x' ↦ v)
   | x == x'   = pure v
   | otherwise = find x ρ

foldBindings :: forall t a b . (Binding t a -> Endo b) -> b -> Bindings t a -> b
foldBindings f z (ρ :+: x ↦ v)   = f (x ↦ v) (foldBindings f z ρ)
foldBindings _ z Empty           = z

update :: forall t a . Bindings t a -> Binding t a -> Bindings t a
update Empty _ = Empty
update (ρ :+: x ↦ v) (x' ↦ v')
   | x == x'    = ρ :+: x' ↦ v'
   | otherwise  = update ρ (x' ↦ v') :+: x ↦ v

splitAt :: forall t a . Int -> Bindings t a -> Bindings t a × Bindings t a
splitAt n ρ
  | n <= 0     = ρ × Empty
  | otherwise  = splitAt' n ρ
   where
   splitAt' :: Int -> Bindings t a -> Bindings t a × Bindings t a
   splitAt' _  Empty        = Empty × Empty
   splitAt' 1  (ρ0 :+: xv)  = ρ0 × Extend Empty xv
   splitAt' m  (ρ0 :+: xv)  = ρ' × (ρ'' :+: xv)
      where
      ρ' × ρ'' = splitAt' (m - 1) ρ0

length :: forall t a . Bindings t a -> Int
length Empty      = 0
length (ρ :+: _)  = 1 + length ρ

fromList :: forall t a . List (Binding t a) -> Bindings t a
fromList Nil      = Empty
fromList (xv : ρ) = fromList ρ :+: xv

-- Delete once we treat Bindings as snoc lists.
toList :: forall t a . Bindings t a -> List (Binding t a)
toList Empty      = Nil
toList (ρ :+: xv) = toList ρ <> singleton xv

-- Probably better to recast Bindings as a snoc list.
toSnocList :: forall t a . Bindings t a -> SnocList (Binding t a)
toSnocList Empty = Lin
toSnocList (ρ :+: xv) = toSnocList ρ :- xv

bindingsMap :: forall t a u b . (t a -> u b) -> Bindings t a -> Bindings u b
bindingsMap _ Empty = Empty
bindingsMap f (Extend ρ (x ↦ v)) = Extend (bindingsMap f ρ) (x ↦ f v)

-- ======================
-- boilerplate
-- ======================
derive instance functorBinding :: Functor t => Functor (Binding t)
derive instance functorBindings :: Functor t => Functor (Bindings t)

instance semigroupBindings :: Semigroup (Bindings t a) where
   append ρ Empty          = ρ
   append ρ (Extend ρ' xv) = Extend (append ρ ρ') xv

instance monoidBindings :: Monoid (Bindings t a) where
   mempty = Empty

instance joinSemilatticeBindings :: (Functor t, JoinSemilattice a, Slices (t a)) => JoinSemilattice (Bindings t a) where
   join = definedJoin
   neg = (<$>) neg

instance slicesBindings :: (Functor t, JoinSemilattice a, Slices (t a)) => Slices (Bindings t a) where
   maybeJoin Empty Empty                     = pure Empty
   maybeJoin (ρ :+: x ↦ v) (ρ' :+: y ↦ v')   = (:+:) <$> maybeJoin ρ ρ' <*> ((↦) <$> (x ≞ y) <*> maybeJoin v v')
   maybeJoin _ _                             = report "Bindings of different lengths"

instance boundedSlices :: (Functor t, BoundedSlices (t Boolean)) => BoundedSlices (Bindings t Boolean) where
   botOf Empty = Empty
   botOf (Extend ρ (x ↦ v)) = Extend (botOf ρ) (x ↦ botOf v)

instance expandableBindings :: Expandable (t a) => Expandable (Bindings t a) where
   expand Empty Empty                              = Empty
   expand (Extend ρ (x ↦ v)) (Extend ρ' (x' ↦ v')) = Extend (expand ρ ρ') ((x ≜ x') ↦ expand v v')
   expand _ _                                      = error absurd
