module Bindings where

import Prelude hiding (top)
import Data.Maybe (Maybe(..))
import Lattice (class Lattice, class Selectable, mapα, maybeZipWithα)
import Util (MayFail, (≟), report)

type Var = String

data Bind a = Bind Var a
data Bindings a = Empty | Extend (Bindings a) (Bind a)

infix 6 Bind as ↦
infixl 5 Extend as :+:
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
   maybeZipWithα f (Extend m (x ↦ v)) (Extend m' (y ↦ v'))  =
      Extend <$> (maybeZipWithα f m m') <*> ((↦) <$> x ≟ y <*> maybeZipWithα f v v')
   maybeZipWithα _ _ _                                      = Nothing

foldBind :: forall a b . (Bind b -> a -> a) -> a -> Bindings b -> a
foldBind f z (ρ :+: x ↦ v)   = f (x ↦ v) (foldBind f z ρ)
foldBind _ z Empty           = z

find :: forall a . Var -> Bindings a -> MayFail a
find x' Empty          = report $ "variable " <> x' <> " not found"
find x' (xs :+: x ↦ v) = if x == x' then pure v else find x' xs

update :: forall a . Lattice a => Var -> a -> Bindings a -> Bindings a
update _ _ Empty = Empty
update x' v' (xs :+: x ↦ v)
   | x == x'    = xs :+: x' ↦ v'
   | otherwise  = (update x' v' xs) :+: x ↦ v
