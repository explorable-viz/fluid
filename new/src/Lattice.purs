module Lattice where

import Prelude hiding (absurd, join, top)
import Control.Apply (lift2)
import Data.List (List, length, zipWith)
import Data.Map (Map, fromFoldable, size, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple)
import Util ((×), (≟), fromJust)

class JoinSemilattice a where
   maybeJoin :: a -> a -> Maybe a

class JoinSemilattice a <= BoundedJoinSemilattice a where
   bot2 :: a -> a

-- Give ∧ and ∨ same associativity and precedence as * and +
infixl 7 meet2 as ∧   -- don't need a meet semilattice typeclass yet
infixl 6 join2 as ∨

type 𝔹 = Boolean

instance joinSemilatticeBoolean :: JoinSemilattice Boolean where
   maybeJoin x y = pure $ x || y

join2 :: forall a . JoinSemilattice a => a -> a -> a
join2 x y = fromJust "Join undefined" $ maybeJoin x y

meet2 :: Boolean -> Boolean -> Boolean
meet2 = (&&)

instance joinSemilatticeTuple :: (Eq k, JoinSemilattice t) => JoinSemilattice (Tuple k t) where
   maybeJoin (k × v) (k' × v') = (k ≟ k') `lift2 (×)` maybeJoin v v'

instance joinSemilatticeList :: JoinSemilattice t => JoinSemilattice (List t) where
   maybeJoin xs ys
      | length xs == length ys   = sequence $ zipWith maybeJoin xs ys
      | otherwise                = Nothing

instance joinSemilatticeMap :: (Ord k, JoinSemilattice t) => JoinSemilattice (Map k t) where
   maybeJoin κs κs'
      | size κs == size κs' =
         fromFoldable <$> (sequence $ zipWith maybeJoin (toUnfoldable κs) (toUnfoldable κs'))
      | otherwise = Nothing
