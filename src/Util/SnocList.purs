module Util.SnocList where

import Prelude
import Data.Function (on)
import Data.List (List(..), (:))

-- Snoc lists. Could reformuate Bindings as SnocList Binding.
data SnocList a =
   SnocNil |
   Snoc (SnocList a) a

derive instance snocListFunctor :: Functor SnocList

infix 6 Snoc as :-

reverse :: SnocList ~> SnocList
reverse = go SnocNil
  where
  go acc SnocNil = acc
  go acc (xs :- x) = go (acc :- x) xs

cons :: forall a. a -> SnocList a -> SnocList a
cons x xs = reverse (reverse xs :- x)

-- There are (at least) two isomorphisms to List, one order preserving, one order inverting.
toList :: forall a . SnocList a -> List a
toList = reverse >>> toListRev

fromList :: forall a . List a -> SnocList a
fromList = reverse <<< fromListRev

toListRev :: SnocList ~> List
toListRev SnocNil = Nil
toListRev (xs :- x) = x : toListRev xs

fromListRev :: List ~> SnocList
fromListRev Nil = SnocNil
fromListRev (x : xs) = fromListRev xs :- x

instance showSnocList :: Show a => Show (SnocList a) where
   show = toList >>> show

instance eqSnocList :: Eq a => Eq (SnocList a) where
   eq = eq `on` toList

instance ordSnocList :: Ord a => Ord (SnocList a) where
   compare = compare `on` toList
