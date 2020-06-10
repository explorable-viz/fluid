module Bindings where

import Prelude hiding (top)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Lattice (class Lattice, class Selectable, mapα, maybeZipWithα)
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

instance bindingsSelectable :: Selectable a => Selectable (Bindings a) where
   mapα _ Empty               = Empty
   mapα f (Extend m (x ↦ v))  = Extend (mapα f m) (x ↦ mapα f v)

   maybeZipWithα _ Empty Empty                              = pure Empty
   maybeZipWithα f (Extend m (x ↦ v)) (Extend m' (y ↦ v'))  =
      Extend <$> (maybeZipWithα f m m') <*> ((↦) <$> x ≟ y <*> maybeZipWithα f v v')
   maybeZipWithα _ _ _                                      = Nothing

find :: forall a . Var -> Bindings a -> Either String a
find x' Empty          = Left $ "variable " <> x' <> " not found"
find x' (xs :+: x ↦ v) = if x == x' then Right v else find x' xs

update :: forall a . Lattice a => Var -> a -> Bindings a -> Bindings a
update x' v' (xs :+: x ↦ v)
   | x == x'    = xs :+: x' ↦ v'
   | otherwise  = (update x' v' xs) :+: x ↦ v
update x' v' Empty = ε :+: x' ↦ v'
