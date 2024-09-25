-- Better name and more consistent interface to Foreign.Object, plus some additional functions.
-- Newtype wrapper so we can fix Ord instance be consistent with Eq (i.e. to use isSubmap vs. toAscArray).
module Dict
   ( module Foreign.Object
   , Dict(..)
   ) where

import Prelude hiding (apply)

import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndexDefaultL, foldlWithIndex, foldrWithIndexDefault)
import Data.Newtype (class Newtype)
import Data.Traversable (class Foldable, class Traversable)
import Foreign.Object (Object) as O
import Foreign.Object (alter, delete, empty, filter, filterKeys, fromFoldable, insert, isEmpty, isSubmap, lookup, mapWithKey, member, singleton, toArrayWithKey, union, unionWith)
import Util (class IsEmpty)
import Util.Map (class Map, class MapF, intersectionWith, keys, maplet, toUnfoldable, values)
import Util.Map as Map
import Util.Set (class Set, difference, size, (∈))

-- Think we can generalise to keys of any type coercible to String.
newtype Dict a = Dict (O.Object a)

derive instance Newtype (Dict a) _
derive newtype instance Eq a => Eq (Dict a)

-- More sensible than Foreign.Object Ord instance.
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

instance IsEmpty (Dict a) where
   isEmpty (Dict d) = isEmpty d

instance Set (Dict a) String where
   empty = Dict empty
   filter p (Dict d) = Dict (filterKeys p d)
   size (Dict d) = size d
   member x (Dict d) = x ∈ d
   difference (Dict d) (Dict d') = Dict (difference d d')
   union (Dict d) (Dict d') = Dict (union d d')

instance Map (Dict a) String a where
   maplet k v = Dict (maplet k v)
   keys (Dict d) = keys d
   values (Dict d) = values d
   filterKeys p (Dict d) = Dict (filterKeys p d)
   unionWith f (Dict d) (Dict d') = Dict (unionWith f d d')
   lookup k (Dict d) = lookup k d
   delete k (Dict d) = Dict (delete k d)
   insert k v (Dict d) = Dict (insert k v d)
   toUnfoldable (Dict d) = toUnfoldable d

instance MapF Dict String where
   intersectionWith f (Dict d) (Dict d') = Dict (intersectionWith f d d')
   difference (Dict d) (Dict d') = Dict (Map.difference d d')
   mapWithKey f (Dict d) = Dict (mapWithKey f d)
