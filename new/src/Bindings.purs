module Bindings where

import Prelude hiding (top, absurd)
import Data.Either (Either(..), isRight)
import Data.Maybe (Maybe(..))
import Lattice (class Lattice, class Selectable, mapα, maybeZipWithα)
import Util (MayFail, type (×), (×), (≟), absurd, error, report)

type Var = String

data Bind a = Bind Var a
data Bindings a = Empty | Extend (Bindings a) (Bind a)

infix 6 Bind as ↦
infixl 5 Extend as :+:
infixl 5 append as :++:
infixl 5 update as ◃

instance bindingsSemigroup :: Semigroup (Bindings a) where
   append m Empty           = m
   append m1 (Extend m2 kv) = Extend (append m1 m2) kv

instance bindingsMonoid :: Monoid (Bindings a) where
   mempty = Empty

instance bindingsSelectable :: Selectable a => Selectable (Bindings a) where
   mapα _ Empty               = Empty
   mapα f (Extend m (x ↦ v))  = Extend (mapα f m) (x ↦ mapα f v)

   maybeZipWithα _ Empty Empty                              = pure Empty
   maybeZipWithα f (Extend m (x ↦ v)) (Extend m' (y ↦ v'))
      = Extend <$> (maybeZipWithα f m m') <*> ((↦) <$> x ≟ y <*> maybeZipWithα f v v')
   maybeZipWithα _ _ _                                      = Nothing

foldBind :: forall a b . (Bind b -> a -> a) -> a -> Bindings b -> a
foldBind f z (ρ :+: x ↦ v)   = f (x ↦ v) (foldBind f z ρ)
foldBind _ z Empty           = z

find :: forall a . Var -> Bindings a -> MayFail a
find x' Empty          = report $ "variable " <> x' <> " not found"
find x' (xs :+: x ↦ v) = if x == x' then pure v else find x' xs

remove :: forall a . Var -> Bindings a -> MayFail (a × Bindings a)
remove x' ρ = go ρ Empty
   where go Empty          acc = Left $ "variable " <> x' <> " not found"
         go (ρ' :+: x ↦ v) acc = if x == x' then pure $ v × (ρ' <> acc)
                                 else go ρ' (acc :+: x ↦ v)

update :: forall a . Lattice a => Bindings a -> Bind a -> Bindings a
update Empty _ = Empty
update (xs :+: x ↦ v) (x' ↦ v')
   | x == x'    = xs :+: x' ↦ v'
   | otherwise  = (update xs (x' ↦ v')) :+: x ↦ v

elem :: forall a . Var -> Bindings a -> Boolean
elem x ρ = isRight $ find x ρ

reverse :: forall a. Bindings a -> Bindings a
reverse = go Empty
  where
  go acc Empty = acc
  go acc (xs :+: x) = go (acc :+: x) xs

length :: forall a. Bindings a -> Int
length (ρ :+: x ↦ v) = 1 + length ρ
length Empty = 0

head :: forall a. Bindings a -> Bind a
head (xs :+: x ↦ v) = (x ↦ v)
head Empty = error absurd