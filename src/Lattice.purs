module Lattice where

import Prelude hiding (absurd, join, top)
import Control.Apply (lift2)
import Data.Array (zipWith) as A
import Data.Foldable (length, foldM)
import Data.List (List, zipWith)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty (zipWith) as NEL
import Data.Map (Map, difference, insert, keys, lookup, toUnfoldable, union, update)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong (second)
import Data.Set (Set, subset)
import Data.Traversable (sequence)
import Data.Tuple (Tuple)
import Util (Endo, MayFail, type (×), (×), (≞), assert, report, successfulWith)

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

botOf :: forall t a . Functor t => BoundedJoinSemilattice a => Endo (t a)
botOf = (<$>) (const bot)

topOf :: forall t a . Functor t => BoundedJoinSemilattice a => Endo (t a)
topOf = (<$>) (const bot >>> neg)

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

instance Slices t => JoinSemilattice (NonEmptyList t) where
   join = definedJoin
   neg = (<$>) neg

instance Slices t => Slices (List t) where
   maybeJoin xs ys
      | (length xs :: Int) == length ys   = sequence (zipWith maybeJoin xs ys)
      | otherwise                         = report "Mismatched lengths"

instance Slices t => Slices (NonEmptyList t) where
   maybeJoin xs ys
      | (length xs :: Int) == length ys   = sequence (NEL.zipWith maybeJoin xs ys)
      | otherwise                         = report "Mismatched lengths"

instance (Key k, Slices t) => JoinSemilattice (Map k t) where
   join = definedJoin
   neg = (<$>) neg

class Ord k <= Key k where
   checkConsistent :: String -> k -> Set k -> MayFail Unit

instance Key String where
   checkConsistent _ _ _ = pure unit

instance (Key k, Slices t) => Slices (Map k t) where
   maybeJoin m m' = foldM mayFailUpdate m (toUnfoldable m' :: List (k × t))

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

class Expandable a where
   expand :: a -> a -> a

instance (Key k, Functor t, BoundedJoinSemilattice a, Expandable (t a)) => Expandable (Map k (t a)) where
   expand kvs kvs' =
      assert (keys kvs `subset` keys kvs') $
      kvs `union` ((kvs' `difference` kvs) <#> botOf)

instance Expandable a => Expandable (List a) where
   expand xs ys = zipWith expand xs ys

instance Expandable a => Expandable (Array a) where
   expand xs ys = A.zipWith expand xs ys
