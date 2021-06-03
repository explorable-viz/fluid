module Util.SnocList where

import Prelude
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
