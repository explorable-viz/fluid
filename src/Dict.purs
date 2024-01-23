-- Better name and more consistent interface to Foreign.Object, plus some additional functions.
-- Maybe upgrade Dict into a full replacement of Foreign.Object; in particular Ord instance
-- seems broken (rather than isSubmap, compares on toAscArray).
module Dict
   ( module Foreign.Object
   , Dict(..)
   , asSingletonMap
   , toUnfoldable
   ) where

import Prelude hiding (apply)

import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndexDefaultL, foldlWithIndex, foldrWithIndexDefault)
import Data.List (head)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (class Foldable, class Traversable)
import Data.Unfoldable (class Unfoldable)
import Foreign.Object (Object, toAscUnfoldable) as O
import Foreign.Object (alter, delete, empty, filter, filterKeys, fromFoldable, insert, isEmpty, isSubmap, lookup, mapWithKey, member, singleton, toArrayWithKey, union, unionWith)
import Util (type (×), assert, definitely)
import Util.Map (class Map', class MapBlah, intersectionWith, keys, maplet, size, values)
import Util.Map as Map
import Util.Set (class Set', difference, (∈))

asSingletonMap :: forall a. Dict a -> String × a
asSingletonMap m =
   assert (size m == 1) (definitely "singleton map" (head (toUnfoldable m)))

toUnfoldable :: forall a f. Unfoldable f => Dict a -> f (String × a)
toUnfoldable = unwrap >>> O.toAscUnfoldable

newtype Dict a = Dict (O.Object a)

derive instance Newtype (Dict a) _

-- Equivalent to Foreign.Object Eq instance but explicate for clarity.
instance (Eq a) => Eq (Dict a) where
   eq (Dict d) (Dict d') = isSubmap d d' && isSubmap d' d

-- More sensible than the Foreign.Object Ord instance.
instance Ord a => Ord (Dict a) where
   compare (Dict d) (Dict d') =
      if isSubmap d d' then
         if isSubmap d' d then EQ
         else LT
      else GT

instance Apply Dict where
   apply (Dict f) (Dict x) = Dict (intersectionWith ($) f x)

derive instance Functor Dict
derive instance Foldable Dict
derive instance Traversable Dict

instance FoldableWithIndex String Dict where
   foldlWithIndex f z (Dict d) = foldlWithIndex f z d
   foldrWithIndex f = foldrWithIndexDefault f
   foldMapWithIndex f = foldMapWithIndexDefaultL f

instance Set' (Dict a) String where
   empty = Dict empty
   isEmpty (Dict d) = isEmpty d
   member x (Dict d) = x ∈ d
   difference (Dict d) (Dict d') = Dict (difference d d')
   union (Dict d) (Dict d') = Dict (union d d')

instance Map' (Dict a) String a where
   maplet k v = Dict (maplet k v)
   keys (Dict d) = keys d
   values (Dict d) = values d
   size (Dict d) = size d
   filterKeys p (Dict d) = Dict (filterKeys p d)
   unionWith f (Dict d) (Dict d') = Dict (unionWith f d d')
   lookup k (Dict d) = lookup k d
   delete k (Dict d) = Dict (delete k d)
   insert k v (Dict d) = Dict (insert k v d)

instance MapBlah Dict String where
   intersectionWith f (Dict d) (Dict d') = Dict (intersectionWith f d d')
   difference (Dict d) (Dict d') = Dict (Map.difference d d')
   mapWithKey f (Dict d) = Dict (mapWithKey f d)
