module Util.Set where

import Prelude hiding (append)

import Data.Foldable (foldl)
import Data.Set (Set)
import Data.Set as Set
import Foreign.Object (Object)
import Foreign.Object as Object
import Util (Endo)

-- Generalises Set but also supports a fixed element type. Doesn't support transforming element type.
class Set' a b | a -> b where
   empty :: a
   isEmpty :: a -> Boolean
   difference :: a -> Endo a
   member :: b -> a -> Boolean
   union :: a -> Endo a

infix 5 difference as \\
infix 5 member as ∈
infixr 6 union as ∪

instance Ord a => Set' (Set a) a where
   empty = Set.empty
   isEmpty = Set.isEmpty
   difference = Set.difference
   member = Set.member
   union = Set.union

instance Set' (Object a) String where
   empty = Object.empty
   isEmpty = Object.isEmpty
   difference x y = foldl (flip Object.delete) x (Object.keys y)
   member = Object.member
   union = Object.union
