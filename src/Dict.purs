-- Better name and more consistent interface to Foreign.Object, plus some additional functions.
module Dict
   ( module Foreign.Object
   , Dict
   , (\\)
   , asSingletonMap
   , difference
   , disjointUnion
   , disjointUnion_inv
   , fromFoldable
   , get
   , intersection
   , intersectionWith
   , keys
   , toUnfoldable
   , unzip
   ) where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Data.List (head)
import Data.Set (Set, member)
import Data.Set (fromFoldable) as S
import Data.Tuple (fst, snd)
import Data.Unfoldable (class Unfoldable)
import Foreign.Object (Object, fromFoldable, keys, toAscUnfoldable) as O
import Foreign.Object (delete, empty, filterKeys, insert, isEmpty, lookup, singleton, size, union, unionWith, update)
import Util (Endo, type (×), (×), assert, definitely, definitely', error)

type Dict a = O.Object a

-- Unfortunately Foreign.Object doesn't define this; could implement using Foreign.Object.ST instead.
foreign import intersectionWith :: forall a b c. (a -> b -> c) -> Dict a -> Dict b -> Dict c

intersection :: forall a b. Dict a -> Dict b -> Dict a
intersection = intersectionWith const

difference :: forall a b. Dict a -> Dict b -> Dict a
difference m1 m2 = foldl (flip delete) m1 (O.keys m2)

infix 5 difference as \\

keys :: forall a. Dict a -> Set String
keys = O.keys >>> S.fromFoldable

asSingletonMap :: forall a. Dict a -> String × a
asSingletonMap m = assert (size m == 1) (definitely "singleton map" (head (toUnfoldable m)))

get :: forall a. String -> Dict a -> a
get k = definitely' <<< lookup k

disjointUnion :: forall a. Dict a -> Endo (Dict a)
disjointUnion = unionWith (\_ _ -> error "not disjoint")

disjointUnion_inv :: forall a. Set String -> Dict a -> Dict a × Dict a
disjointUnion_inv ks m = filterKeys (_ `member` ks) m × filterKeys (_ `not <<< member` ks) m

toUnfoldable :: forall a f. Unfoldable f => Dict a -> f (String × a)
toUnfoldable = O.toAscUnfoldable

fromFoldable :: forall a f. Foldable f => f (String × a) -> Dict a
fromFoldable = O.fromFoldable

unzip :: forall a b. Dict (a × b) -> Dict a × Dict b
unzip kvs = (kvs <#> fst) × (kvs <#> snd)
