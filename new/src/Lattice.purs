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
   bot :: a -> a

-- Give ∧ and ∨ same associativity and precedence as * and +
infixl 7 meet as ∧
infixl 6 join as ∨

type 𝔹 = Boolean

instance joinSemilatticeBoolean :: JoinSemilattice Boolean where
   maybeJoin x y = pure $ x || y

join :: forall a . JoinSemilattice a => a -> a -> a
join x y = fromJust "Join undefined" $ maybeJoin x y

-- don't need a meet semilattice typeclass just yet
meet :: Boolean -> Boolean -> Boolean
meet = (&&)

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
