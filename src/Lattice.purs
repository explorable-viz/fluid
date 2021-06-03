module Lattice where

import Prelude hiding (absurd, join, top)
import Control.Apply (lift2)
import Data.Array (length, zipWith) as A
import Data.Foldable (foldM, length)
import Data.List (List, zipWith)
import Data.Map (Map, fromFoldable, insert, lookup, toUnfoldable, update)
import Data.Map.Internal (keys)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong (second)
import Data.Traversable (sequence)
import Data.Tuple (Tuple)
import Util (MayFail, type (×), (×), (≞), absurd, error, report, successfulWith)
import Util.SnocList (SnocList(..))
import Util.SnocList (zipWith) as S

class JoinSemilattice a where
   join :: a -> a -> a
   neg :: a -> a

class JoinSemilattice a <= BoundedJoinSemilattice a where
   bot :: a

instance joinSemilatticeBoolean :: JoinSemilattice Boolean where
   join = (||)
   neg = not

instance boundedJoinSemilatticeBoolean :: BoundedJoinSemilattice Boolean where
   bot = false

instance joinSemilatticeUnit :: JoinSemilattice Unit where
   join _ = identity
   neg = identity

instance boundedJoinSemilatticeUnit :: BoundedJoinSemilattice Unit where
   bot = unit

-- Sometimes convenient to assume join defined even if it may not be.
class JoinSemilattice a <= Slices a where
   maybeJoin :: a -> a -> MayFail a

definedJoin :: forall a . Slices a => a -> a -> a
definedJoin x = successfulWith "Join undefined" <<< maybeJoin x

class Slices a <= BoundedSlices a where
   botOf :: a -> a

-- Give ∧ and ∨ same associativity and precedence as * and +
infixl 7 meet as ∧
infixl 6 join as ∨

type 𝔹 = Boolean

-- don't need a meet semilattice typeclass just yet
meet :: Boolean -> Boolean -> Boolean
meet = (&&)

instance joinSemilatticeTuple :: (Eq k, Show k, Slices t) => JoinSemilattice (Tuple k t) where
   join = definedJoin
   neg = second neg

instance slicesTuple :: (Eq k, Show k, Slices t) => Slices (Tuple k t) where
   maybeJoin (k × v) (k' × v') = (k ≞ k') `lift2 (×)` maybeJoin v v'

instance joinSemilatticeList :: Slices t => JoinSemilattice (List t) where
   join = definedJoin
   neg = (<$>) neg

instance slicesList :: Slices t => Slices (List t) where
   maybeJoin xs ys
      | (length xs :: Int) == length ys   = sequence (zipWith maybeJoin xs ys)
      | otherwise                         = report "Lists of different lengths"

instance boundedSlicesList :: BoundedSlices t => BoundedSlices (List t) where
   botOf = (<$>) botOf

instance joinSemilatticeMap :: (Key k, Slices t) => JoinSemilattice (Map k t) where
   join = definedJoin
   neg = (<$>) neg

class Ord k <= Key k where
   checkConsistent :: String -> k -> List k -> MayFail Unit

-- This is more general than we technically need for slicing, in that one can merge maps with distinct keys, as long as
-- the values are mergable for overlapping keys. I think this is harmless, and it allows use to reuse the join operator
-- here for merging branches of function definitions.
instance slicesMap :: (Key k, Slices t) => Slices (Map k t) where
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

instance joinSemilatticeArray :: Slices a => JoinSemilattice (Array a) where
   join = definedJoin
   neg = (<$>) neg

instance slicesArray :: Slices a => Slices (Array a) where
   maybeJoin xs ys
      | A.length xs == A.length ys  = sequence (A.zipWith maybeJoin xs ys)
      | otherwise                   = report "Arrays of different lengths"

class Expandable a where
   -- Partial function defined iff x is above x', which expands in x any subtree prefixes which are expanded in x'
   -- Negative holes are used in x' to represent unexpand subtrees; positive holes will never occur.
   expand :: a -> a -> a

instance expandableArray :: Expandable t => Expandable (Array t) where
   expand xs ys
      | A.length xs == A.length ys  = A.zipWith expand xs ys
      | otherwise                   = error absurd

instance expandableList :: Expandable t => Expandable (List t) where
   expand xs ys
      | (length xs :: Int) == length ys   = zipWith expand xs ys
      | otherwise                         = error absurd

instance expandableSnocList :: Expandable t => Expandable (SnocList t) where
   expand xs ys
      | (length xs :: Int) == length ys   = S.zipWith expand xs ys
      | otherwise                         = error absurd

instance expandableMap :: (Ord k, Expandable (t a)) => Expandable (Map k (t a)) where
   expand m m'
      | keys m == keys m'  = fromFoldable (zipWith expandValue (toUnfoldable m) (toUnfoldable m'))
      where
      expandValue (k × x) (_ × x') = k × expand x x'
      | otherwise          = error absurd
