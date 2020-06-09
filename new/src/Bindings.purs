module Bindings where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldl, foldMapDefaultL, foldrDefault)
import Util (MayFail)

type Var = String

data Bind a = Bind Var a

infix 6 Bind as ↦

data Bindings a =
   Empty | Extend (Bindings a) (Bind a)

infixl 5 Extend as :+:

ε :: ∀ a . Bindings a
ε = Empty

instance bindingsSemigroup :: Semigroup (Bindings a) where
   append m Empty = m
   append m1 (Extend m2 kv) = Extend (append m1 m2) kv

instance bindingsMonoid :: Monoid (Bindings a) where
   mempty = ε

instance bindingsFoldable :: Foldable Bindings where
   foldl _ z Empty = z
   foldl f z (m :+: k ↦ v) = f (foldl f z m) v

   foldr f = foldrDefault f
   foldMap = foldMapDefaultL

find :: ∀ a . Var -> Bindings a -> MayFail a
find x Empty = Left $ "variable " <> x <> " not found"
find x (m :+: k ↦ v) = if x == k then Right v else find x m
