module Bindings where

import Prelude hiding (top)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Lattice (class Lattice, bot, top, (∧?), (∨?))
import Util ((≟))

type Var = String

data Bind a = Bind Var a

infix 6 Bind as ↦

data Bindings a = Empty | Extend (Bindings a) (Bind a)

infixl 5 Extend as :+:

infixl 5 update as ◃

ε :: ∀ a . Bindings a
ε = Empty

instance bindingsSemigroup :: Semigroup (Bindings a) where
   append m Empty           = m
   append m1 (Extend m2 kv) = Extend (append m1 m2) kv

instance bindingsMonoid :: Monoid (Bindings a) where
   mempty = ε

instance bindingsLattice :: Lattice a => Lattice (Bindings a) where
   maybeMeet (xs :+: x ↦ vx) (ys :+: y ↦ vy) = do
      z  <- x ≟ y
      vz <- vx ∧? vy
      zs <- xs ∧? ys
      pure (zs :+: z ↦ vz)
   maybeMeet Empty Empty    = pure Empty
   maybeMeet _     Empty    = Nothing
   maybeMeet Empty _        = Nothing
   maybeJoin (xs :+: x ↦ vx) (ys :+: y ↦ vy) = do
      z  <- x ≟ y
      vz <- vx ∨? vy
      zs <- xs ∨? ys
      pure (zs :+: z ↦ vz)
   maybeJoin Empty Empty    = pure Empty
   maybeJoin _     Empty    = Nothing
   maybeJoin Empty _        = Nothing
   top       (xs :+: x ↦ v) = top xs :+:  x ↦ top v
   top       Empty          = Empty
   bot       (xs :+: x ↦ v) = bot xs :+:  x ↦ bot v
   bot       Empty          = Empty

find :: forall a . Var -> Bindings a -> Either String a
find x' Empty          = Left $ "variable " <> x' <> " not found"
find x' (xs :+: x ↦ v) = if x == x' then Right v else find x' xs

update :: forall a . Lattice a => Var -> a -> Bindings a -> Bindings a
update x' v' (xs :+: x ↦ v)
   | x == x'    = xs :+: x' ↦ v'
   | otherwise  = (update x' v' xs) :+: x ↦ v
update x' v' Empty = Empty