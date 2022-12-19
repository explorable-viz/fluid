module Lattice where

import Prelude hiding (absurd, join)
import Control.Apply (lift2)
import Data.Array (zipWith) as A
import Data.Foldable (length, foldM)
import Data.List (List, zipWith)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong (second)
import Data.Set (subset)
import Data.Traversable (sequence)
import Data.Tuple (Tuple)
import Dict (Dict, difference, intersectionWith, lookup, insert, keys, toUnfoldable, union, unionWith, update)
import Bindings (Var)
import Util (Endo, MayFail, type (×), (×), (≞), assert, report, successfulWith)

-- TODO: move 'neg' out of here.
-- Join here is actually more general "weak join" operation of the formalism, which operates on maps using unionWith.
class JoinSemilattice a where
   join :: a -> a -> a
   neg :: Endo a

class MeetSemilattice a where
   meet :: a -> a -> a

class JoinSemilattice a <= BoundedJoinSemilattice a where
   bot :: a

class MeetSemilattice a <= BoundedMeetSemilattice a where
   top :: a

instance JoinSemilattice Boolean where
   join = (||)
   neg = not

instance MeetSemilattice Boolean where
   meet = (&&)

instance BoundedJoinSemilattice Boolean where
   bot = false

instance BoundedMeetSemilattice Boolean where
   top = true

instance JoinSemilattice Unit where
   join _ = identity
   neg = identity

instance BoundedJoinSemilattice Unit where
   bot = unit

class (BoundedJoinSemilattice a, BoundedMeetSemilattice a) <= BoundedLattice a

instance BoundedLattice Boolean

-- Need "soft failure" for joining incompatible eliminators so we can use it to desugar function clauses.
-- TODO: rename to PartialJoinSemilattice
class JoinSemilattice a <= Slices a where
   maybeJoin :: a -> a -> MayFail a

definedJoin :: forall a. Slices a => a -> a -> a
definedJoin x = successfulWith "Join undefined" <<< maybeJoin x

botOf :: forall t a. Functor t => BoundedJoinSemilattice a => Endo (t a)
botOf = (<$>) (const bot)

topOf :: forall t a. Functor t => BoundedJoinSemilattice a => Endo (t a)
topOf = (<$>) (const bot >>> neg)

-- Give ∧ and ∨ same associativity and precedence as * and +
infixl 7 meet as ∧
infixl 6 join as ∨

type 𝔹 = Boolean

instance (Eq k, Show k, Slices a) => JoinSemilattice (Tuple k a) where
   join = definedJoin
   neg = second neg

instance (Eq k, Show k, Slices a) => Slices (Tuple k a) where
   maybeJoin (k × v) (k' × v') = (k ≞ k') `lift2 (×)` maybeJoin v v'

instance Slices a => JoinSemilattice (List a) where
   join = definedJoin
   neg = (<$>) neg

instance Slices a => Slices (List a) where
   maybeJoin xs ys
      | (length xs :: Int) == length ys = sequence (zipWith maybeJoin xs ys)
      | otherwise = report "Mismatched lengths"

instance Slices a => JoinSemilattice (Dict a) where
   join = unionWith (∨) -- faster than definedJoin
   neg = (<$>) neg

instance Slices a => Slices (Dict a) where
   maybeJoin m m' = foldM mayFailUpdate m (toUnfoldable m' :: List (Var × a))

mayFailUpdate :: forall a. Slices a => Dict a -> Var × a -> MayFail (Dict a)
mayFailUpdate m (k × v) =
   case lookup k m of
      Nothing -> pure (insert k v m)
      Just v' -> update <$> (const <$> Just <$> maybeJoin v' v) <@> k <@> m

instance Slices a => JoinSemilattice (Array a) where
   join = definedJoin
   neg = (<$>) neg

instance Slices a => Slices (Array a) where
   maybeJoin xs ys
      | length xs == (length ys :: Int) = sequence (A.zipWith maybeJoin xs ys)
      | otherwise = report "Mismatched lengths"

class Expandable a where
   expand :: a -> a -> a

instance (Functor t, BoundedJoinSemilattice a, Expandable (t a)) => Expandable (Dict (t a)) where
   expand kvs kvs' =
      assert (keys kvs `subset` keys kvs') $
         (kvs `intersectionWith expand` kvs') `union` ((kvs' `difference` kvs) <#> botOf)

instance Expandable a => Expandable (List a) where
   expand xs ys = zipWith expand xs ys

instance Expandable a => Expandable (Array a) where
   expand xs ys = A.zipWith expand xs ys
