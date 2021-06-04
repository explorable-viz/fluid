module Util.SnocList where

import Prelude
import Data.Foldable (class Foldable, foldl, foldMap, foldr)
import Data.Function (on)
import Data.List (List(..), (:))
import Data.List (reverse, unzip, zipWith) as L
import Data.Profunctor.Strong ((***))
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (curry, uncurry)
import Util (type (×))

-- Snoc lists. Could reformuate Bindings as SnocList Binding.
data SnocList a =
   SnocNil |
   Snoc (SnocList a) a

derive instance snocListFunctor :: Functor SnocList

infix 6 Snoc as :-

-- (Order-preserving) natural isomorphism to List.
toList :: SnocList ~> List
toList SnocNil = Nil
toList (xs :- x) = x : toList xs

fromList :: List ~> SnocList
fromList Nil = SnocNil
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

-- TODO: monad instance
