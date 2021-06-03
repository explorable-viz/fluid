module Util.SnocList where

import Prelude
import Control.Apply (lift2)
import Data.Foldable (class Foldable, foldl)
import Data.Traversable (class Traversable, traverse)
import Data.Function (on)
import Data.List (List(..), (:))
import Data.List (reverse) as L

-- Snoc lists. Could reformuate Bindings as SnocList Binding.
data SnocList a =
   SnocNil |
   Snoc (SnocList a) a

derive instance snocListFunctor :: Functor SnocList

infix 6 Snoc as :-

reverse :: SnocList ~> SnocList
reverse = toList >>> L.reverse >>> fromList

-- (Order-preserving) natural isomorphisms to List.
toList :: SnocList ~> List
toList SnocNil = Nil
toList (xs :- x) = x : toList xs

fromList :: List ~> SnocList
fromList Nil = SnocNil
fromList (x : xs) = fromList xs :- x

instance showSnocList :: Show a => Show (SnocList a) where
   show = toList >>> show

instance eqSnocList :: Eq a => Eq (SnocList a) where
   eq = eq `on` toList

instance ordSnocList :: Ord a => Ord (SnocList a) where
   compare = compare `on` toList

zipWith :: forall a b c. (a -> b -> c) -> SnocList a -> SnocList b -> SnocList c
zipWith f xs ys = reverse $ go xs ys SnocNil
   where
   go SnocNil _ acc = acc
   go _ SnocNil acc = acc
   go (as :- a) (bs :- b) acc = go as bs $ acc :- f a b

-- Adapted from PureScript prelude.
instance foldableSnocList :: Foldable SnocList where
   foldr f b = foldl (flip f) b <<< rev
      where
      rev = go SnocNil
         where
         go acc SnocNil = acc
         go acc (xs :- x) = go (acc :- x) xs
   foldl f = go
      where
      go b = case _ of
         SnocNil -> b
         as :- a -> go (f b a) as
   foldMap f = foldl (\acc -> append acc <<< f) mempty

-- Adapted from PureScript prelude.
instance traversableSnocList :: Traversable SnocList where
  traverse f = map (foldl (:-) SnocNil) <<< foldl (\acc -> lift2 (:-) acc <<< f) (pure SnocNil)
  sequence = traverse identity
