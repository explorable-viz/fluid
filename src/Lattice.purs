module Lattice where

import Prelude hiding (absurd, join, top)
import Control.Apply (lift2)
import Data.Array (zipWith) as A
import Data.Foldable (length, foldM)
import Data.List (List, zipWith)
import Data.Map (Map, insert, lookup, toUnfoldable, update)
import Data.Map.Internal (keys)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong (second)
import Data.Traversable (sequence)
import Data.Tuple (Tuple)
import Util (Endo, MayFail, type (×), (×), (≞), report, successfulWith)
import Util.SnocList (SnocList)
import Util.SnocList (zipWith) as S

class JoinSemilattice a where
   join :: a -> a -> a
   neg :: Endo a

class JoinSemilattice a <= BoundedJoinSemilattice a where
   bot :: a

instance JoinSemilattice Boolean where
   join = (||)
   neg = not

instance BoundedJoinSemilattice Boolean where
   bot = false

instance JoinSemilattice Unit where
   join _ = identity
   neg = identity

instance BoundedJoinSemilattice Unit where
   bot = unit

-- Sometimes convenient to assume join defined even if it may not be.
class JoinSemilattice a <= Slices a where
   maybeJoin :: a -> a -> MayFail a

definedJoin :: forall a . Slices a => a -> a -> a
definedJoin x = successfulWith "Join undefined" <<< maybeJoin x

class Slices a <= BoundedSlices a where
   botOf :: Endo a

topOf :: forall a . BoundedSlices a => Endo a
topOf = botOf >>> neg

-- Give ∧ and ∨ same associativity and precedence as * and +
infixl 7 meet as ∧
infixl 6 join as ∨

type 𝔹 = Boolean

-- don't need a meet semilattice typeclass just yet
meet :: Boolean -> Boolean -> Boolean
meet = (&&)

instance (Eq k, Show k, Slices t) => JoinSemilattice (Tuple k t) where
   join = definedJoin
   neg = second neg

instance (Eq k, Show k, Slices t) => Slices (Tuple k t) where
   maybeJoin (k × v) (k' × v') = (k ≞ k') `lift2 (×)` maybeJoin v v'

instance Slices t => JoinSemilattice (List t) where
   join = definedJoin
   neg = (<$>) neg

instance Slices t => JoinSemilattice (SnocList t) where
   join = definedJoin
   neg = (<$>) neg

instance Slices t => Slices (List t) where
   maybeJoin xs ys
      | (length xs :: Int) == length ys   = sequence (zipWith maybeJoin xs ys)
      | otherwise                         = report "Mismatched lengths"

instance Slices t => Slices (SnocList t) where
   maybeJoin xs ys
      | (length xs :: Int) == length ys   = sequence (S.zipWith maybeJoin xs ys)
      | otherwise                         = report "Mismatched lengths"

instance BoundedSlices t => BoundedSlices (List t) where
   botOf = (<$>) botOf

instance BoundedSlices t => BoundedSlices (SnocList t) where
   botOf = (<$>) botOf

instance (Key k, Slices t) => JoinSemilattice (Map k t) where
   join = definedJoin
   neg = (<$>) neg

class Ord k <= Key k where
   checkConsistent :: String -> k -> List k -> MayFail Unit

instance Key String where
   checkConsistent _ _ _ = pure unit

-- This is more general than we technically need for slicing, in that we can join maps with distinct keys as long as
-- join is defined for any overlapping keys. This is harmless, and it allows us to reuse the join operator
-- here for merging branches of piecewise function definitions.
instance (Key k, Slices t) => Slices (Map k t) where
   maybeJoin m m' =
      foldM mayFailUpdate m (toUnfoldable m' :: List (k × t))

mayFailUpdate :: forall k t . Key k => Slices t => Map k t -> k × t -> MayFail (Map k t)
mayFailUpdate m (k × v) =
   case lookup k m of
      Nothing -> do
         checkConsistent "Inconsistent keys: " k (keys m)
         pure (insert k v m)
      Just v' ->
         update <$> (const <$> Just <$> maybeJoin v' v) <@> k <@> m

instance Slices a => JoinSemilattice (Array a) where
   join = definedJoin
   neg = (<$>) neg

instance Slices a => Slices (Array a) where
   maybeJoin xs ys
      | length xs == (length ys :: Int)   = sequence (A.zipWith maybeJoin xs ys)
      | otherwise                         = report "Mismatched lengths"
