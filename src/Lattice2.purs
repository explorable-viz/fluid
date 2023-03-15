module Lattice2 where

import Prelude hiding (absurd, join)
import Control.Apply (lift2)
import Data.Array (zipWith) as A
import Data.Foldable (length, foldM)
import Data.List (List, zipWith)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong ((***))
import Data.Set (subset)
import Data.Traversable (sequence)
import Dict (Dict, difference, intersectionWith, lookup, insert, keys, toUnfoldable, union, unionWith, update)
import Bindings (Var)
import Util (Endo, MayFail, type (×), (×), assert, report, successfulWith)
import Util.Pair (Pair(..))

-- join here is actually more general "weak join" operation of the formalism, which operates on maps using unionWith.
class JoinSemilattice a where
   join :: a -> a -> a
   -- soft failure for joining incompatible eliminators, used to desugar function clauses
   maybeJoin :: a -> a -> MayFail a
   -- TODO: extract new typeclass for neg
   neg :: Endo a

class MeetSemilattice a where
   meet :: a -> a -> a

class JoinSemilattice a <= BoundedJoinSemilattice a where
   bot :: a

class MeetSemilattice a <= BoundedMeetSemilattice a where
   top :: a

instance JoinSemilattice Boolean where
   join = (||)
   maybeJoin x y = pure (join x y)
   neg = not

instance MeetSemilattice Boolean where
   meet = (&&)

instance BoundedJoinSemilattice Boolean where
   bot = false

instance BoundedMeetSemilattice Boolean where
   top = true

instance JoinSemilattice Unit where
   join _ = identity
   maybeJoin x y = pure (join x y)
   neg = identity

instance MeetSemilattice Unit where
   meet _ = identity

instance BoundedJoinSemilattice Unit where
   bot = unit

instance BoundedMeetSemilattice Unit where
   top = unit

class (BoundedJoinSemilattice a, BoundedMeetSemilattice a) <= BoundedLattice a

instance BoundedLattice Boolean
instance BoundedLattice Unit

definedJoin :: forall a. JoinSemilattice a => a -> a -> a
definedJoin x = successfulWith "Join undefined" <<< maybeJoin x

class BotOf t u | t -> u where
   botOf :: t -> u

class TopOf t u | t -> u where
   topOf :: t -> u

instance (Functor t, BoundedJoinSemilattice a) => BotOf (Unit × Raw t) (a × t a) where
   botOf = const bot *** botOf
else instance (Functor t, BoundedJoinSemilattice a, BoundedJoinSemilattice a') => BotOf (t a) (t a') where
   botOf = (<$>) (const bot)

instance (Functor t, BoundedJoinSemilattice a, BoundedJoinSemilattice a') => TopOf (t a) (t a') where
   topOf = (<$>) (const bot >>> neg)

-- Specialises botOf and topOf but omits the lattice constraint.
erase :: forall t a. Functor t => t a -> Raw t
erase = (<$>) (const unit)

-- Give ∧ and ∨ same associativity and precedence as * and +
infixl 7 meet as ∧
infixl 6 join as ∨

type 𝔹 = Boolean
type Raw (c :: Type -> Type) = c Unit

instance (JoinSemilattice a, JoinSemilattice b) => JoinSemilattice (a × b) where
   join ab = definedJoin ab
   maybeJoin (a × a') (b × b') = maybeJoin a b `lift2 (×)` maybeJoin a' b'
   neg = (<$>) neg

instance JoinSemilattice a => JoinSemilattice (Pair a) where
   join ab = definedJoin ab
   maybeJoin (Pair a1 a1') (Pair a2 a2') = Pair <$> maybeJoin a1 a2 <*> maybeJoin a1' a2'
   neg = (<$>) neg

instance JoinSemilattice a => JoinSemilattice (List a) where
   join xs = definedJoin xs
   maybeJoin xs ys
      | (length xs :: Int) == length ys = sequence (zipWith maybeJoin xs ys)
      | otherwise = report "Mismatched list lengths"
   neg = (<$>) neg

instance JoinSemilattice a => JoinSemilattice (Dict a) where
   join = unionWith (∨) -- faster than definedJoin
   maybeJoin m m' = foldM mayFailUpdate m (toUnfoldable m' :: List (Var × a))
   neg = (<$>) neg

mayFailUpdate :: forall a. JoinSemilattice a => Dict a -> Var × a -> MayFail (Dict a)
mayFailUpdate m (k × v) =
   case lookup k m of
      Nothing -> pure (insert k v m)
      Just v' -> update <$> (const <$> Just <$> maybeJoin v' v) <@> k <@> m

instance JoinSemilattice a => JoinSemilattice (Array a) where
   join xs = definedJoin xs
   maybeJoin xs ys
      | length xs == (length ys :: Int) = sequence (A.zipWith maybeJoin xs ys)
      | otherwise = report "Mismatched array lengths"
   neg = (<$>) neg

-- To express as Expandable (t :: Type -> Type) requires functor composition..
class Expandable t u | t -> u where
   expand :: t -> u -> t

instance Expandable (t a) (Raw t) => Expandable (a × t a) (Unit × Raw t) where
   expand (α × a) (_ × a') = α × expand a a'

instance Expandable t u => Expandable (Pair t) (Pair u) where
   expand (Pair x x') (Pair y y') = Pair (expand x y) (expand x' y')

instance (BotOf u t, Expandable t u) => Expandable (Dict t) (Dict u) where
   expand kvs kvs' =
      assert (keys kvs `subset` keys kvs') $
         (kvs `intersectionWith expand` kvs') `union` ((kvs' `difference` kvs) <#> botOf)

instance Expandable t u => Expandable (List t) (List u) where
   expand xs ys = zipWith expand xs ys

instance Expandable t u => Expandable (Array t) (Array u) where
   expand xs ys = A.zipWith expand xs ys
