module Util.SnocList where

import Prelude
import Data.Function (on)
import Data.List (List(..), snoc)

-- Snoc lists. Could reformuate Bindings as SnocList Binding.
data SnocList a =
   SnocNil |
   Snoc (SnocList a) a

derive instance snocListFunctor :: Functor SnocList

infix 6 Snoc as :-

toList :: forall a . SnocList a -> List a
toList SnocNil    = Nil
toList (xs :- x)  = snoc (toList xs) x

instance showSnocList :: Show a => Show (SnocList a) where
   show = toList >>> show

instance eqSnocList :: Eq a => Eq (SnocList a) where
   eq = eq `on` toList

instance ordSnocList :: Ord a => Ord (SnocList a) where
   compare = compare `on` toList
