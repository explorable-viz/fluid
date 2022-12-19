module Util.Pair where

import Prelude

import Data.Foldable (class Foldable, foldrDefault, foldMapDefaultL)
import Data.List (List)
import Data.List (unzip, zip) as L
import Data.Traversable (class Traversable, sequenceDefault)
import Util (type (×), (×))

-- a |-> a × a can't derive a functor instance, so use this
data Pair a = Pair a a

instance Functor Pair where
   map f (Pair x y) = Pair (f x) (f y)

instance Apply Pair where
   apply (Pair f g) (Pair x y) = Pair (f x) (g y)

instance Applicative Pair where
   pure x = Pair x x

instance Foldable Pair where
   foldl f z (Pair x y) = f (f z x) y
   foldr f = foldrDefault f
   foldMap f = foldMapDefaultL f

instance Traversable Pair where
   traverse f (Pair x y) = Pair <$> f x <*> f y
   sequence = sequenceDefault

toTuple :: forall a. Pair a -> a × a
toTuple (Pair x y) = x × y

fromTuple :: forall a. a × a -> Pair a
fromTuple (x × y) = Pair x y

zip :: forall a. List a -> List a -> List (Pair a)
zip xs ys = L.zip xs ys <#> fromTuple

unzip :: forall a. List (Pair a) -> List a × List a
unzip xys = xys <#> toTuple # L.unzip
