module Util2.SnocList2 where

import Prelude
import Data.Foldable (class Foldable, foldl, foldMap, foldr)
import Data.Function (on)
import Data.List (List(..), (:), take, drop)
import Data.List (reverse, unzip, zip, zipWith) as L
import Data.Profunctor.Strong ((***))
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (curry, uncurry)
import Util2 (type (×), (×))

-- Snoc lists. Could reformuate Bindings as SnocList Binding.
data SnocList a =
   Lin |
   Snoc (SnocList a) a

derive instance snocListFunctor :: Functor SnocList

infixl 6 Snoc as :-

singleton :: forall a . a -> SnocList a
singleton x = Lin :- x

-- (Order-preserving) natural isomorphism to List.
toList :: SnocList ~> List
toList Lin = Nil
toList (xs :- x) = x : toList xs

fromList :: List ~> SnocList
fromList Nil = Lin
fromList (x : xs) = fromList xs :- x

reverse :: SnocList ~> SnocList
reverse = toList >>> L.reverse >>> fromList

instance showSnocList :: Show a => Show (SnocList a) where
   show = toList >>> show

instance eqSnocList :: Eq a => Eq (SnocList a) where
   eq = eq `on` toList

instance ordSnocList :: Ord a => Ord (SnocList a) where
   compare = compare `on` toList

zipWith :: forall a b c. (a -> b -> c) -> SnocList a -> SnocList b -> SnocList c
zipWith f = curry $ (toList *** toList) >>> uncurry (L.zipWith f) >>> fromList

instance foldableSnocList :: Foldable SnocList where
   foldr f b = toList >>> foldr f b
   foldl f b = toList >>> foldl f b
   foldMap f = toList >>> foldMap f

-- Adapted from PureScript prelude.
instance traversableSnocList :: Traversable SnocList where
  traverse f xs = fromList <$> traverse f (toList xs)
  sequence = traverse identity

unzip :: forall a b . SnocList (a × b) -> SnocList a × SnocList b
unzip = toList >>> L.unzip >>> (fromList *** fromList)

zip :: forall a b . SnocList a -> SnocList b -> SnocList (a × b)
zip = curry ((toList *** toList) >>> uncurry L.zip >>> fromList)

instance semigroupSnocList :: Semigroup (SnocList a) where
   -- The flip is crucial -- see https://stackoverflow.com/questions/13034856.
   append = curry $ (toList *** toList) >>> uncurry (flip append) >>> fromList

instance monoidSnocList :: Monoid (SnocList a) where
   mempty = Lin

splitAt :: forall a . Int -> SnocList a -> SnocList a × SnocList a
splitAt n xs =
   fromList (drop n (toList xs)) × fromList (take n (toList xs))
