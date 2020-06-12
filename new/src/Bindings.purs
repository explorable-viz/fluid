module Bindings where

import Prelude hiding (top)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Lattice (class Lattice, class Selectable, mapα, maybeZipWithα)
import Util ((≟))

type Var = String

data Bind a = Bind Var a
data Bindings a = Empty | Extend (Bindings a) (Bind a)

infix 6 Bind as ↦
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

tail :: forall a . Bindings a -> Either String (Bindings a)
tail Empty      = Left $ "can not take tail on empty environment"
tail (xs :+: x) = Right xs

find :: forall a . Var -> Bindings a -> Either String a
find x' Empty          = Left $ "variable " <> x' <> " not found"
find x' (xs :+: x ↦ v) = if x == x' then Right v else find x' xs

remove :: forall a . Var -> Bindings a -> Either String (Tuple a (Bindings a))
remove x' xss   = go xss Empty
   where go Empty          acc = Left $ "variable " <> x' <> " not found"
         go (xs :+: x ↦ v) acc = if x == x' then Right (Tuple v (append xs acc))
                                 else go xs (acc :+: x ↦ v)

update :: forall a . Lattice a => Var -> a -> Bindings a -> Bindings a
update _ _ Empty = Empty
update x' v' (xs :+: x ↦ v)
   | x == x'    = xs :+: x' ↦ v'
   | otherwise  = (update x' v' xs) :+: x ↦ v
