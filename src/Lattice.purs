module Lattice where

import Prelude hiding (absurd, join, top)

import Bindings (Var)
import Control.Apply (lift2)
import Control.Monad.Error.Class (class MonadError)
import Data.Array (zipWith) as A
import Data.Foldable (length, foldM)
import Data.List (List, zipWith)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong ((***))
import Data.Set (subset)
import Data.Traversable (sequence)
import Dict (Dict, lookup, insert, keys, toUnfoldable, update)
import Dict ((\\), (∪), intersectionWith, unionWith) as D
import Effect.Exception (Error)
import Util (type (×), Endo, assert, successfulWith, throw, (×))
import Util.Pair (Pair(..))

-- join here is actually more general "weak join" operation of the formalism, which operates on maps using unionWith.
class JoinSemilattice a where
   join :: a -> a -> a
   -- soft failure for joining incompatible eliminators, used to desugar function clauses; see #776
   maybeJoin :: forall m. MonadError Error m => a -> a -> m a

class MeetSemilattice a where
   meet :: a -> a -> a

class JoinSemilattice a <= BoundedJoinSemilattice a where
   bot :: a

class MeetSemilattice a <= BoundedMeetSemilattice a where
   top :: a

class (BoundedJoinSemilattice a, BoundedMeetSemilattice a) <= BoundedLattice a

class Neg a where
   neg :: Endo a

class (BoundedLattice a, Neg a) <= BooleanLattice a

class BotOf t u | t -> u where
   botOf :: t -> u

class TopOf t u | t -> u where
   topOf :: t -> u

instance JoinSemilattice Boolean where
   join = (||)
   maybeJoin x y = pure (join x y)

instance MeetSemilattice Boolean where
   meet = (&&)

instance BoundedJoinSemilattice Boolean where
   bot = false

instance BoundedMeetSemilattice Boolean where
   top = true

instance BooleanLattice Boolean

instance Neg Boolean where
   neg = not

instance JoinSemilattice Unit where
   join _ = identity
   maybeJoin x y = pure (join x y)

instance MeetSemilattice Unit where
   meet _ = identity

instance BoundedJoinSemilattice Unit where
   bot = unit

instance BoundedMeetSemilattice Unit where
   top = unit

instance Neg Unit where
   neg = identity

definedJoin :: forall a. JoinSemilattice a => a -> a -> a
definedJoin x y = successfulWith "Join undefined" (maybeJoin x y)

instance (Functor t, BoundedJoinSemilattice a) => BotOf (Unit × Raw t) (a × t a) where
   botOf = const bot *** botOf
else instance (Functor t, BoundedJoinSemilattice a') => BotOf (t a) (t a') where
   botOf = (<$>) (const bot)

instance (Functor t, BooleanLattice a') => TopOf (t a) (t a') where
   topOf = (<$>) (const bot >>> neg)

-- Specialises botOf and topOf but omits the lattice constraint.
erase :: forall t a. Functor t => t a -> Raw t
erase = (<$>) (const unit)

-- Same associativity and precedence as * and +
infixl 7 meet as ∧
infixl 6 join as ∨

type 𝔹 = Boolean
type Raw (f :: Type -> Type) = f Unit

instance (JoinSemilattice a, JoinSemilattice b) => JoinSemilattice (a × b) where
   join ab = definedJoin ab
   maybeJoin (a × a') (b × b') = maybeJoin a b `lift2 (×)` maybeJoin a' b'

instance (MeetSemilattice a, MeetSemilattice b) => MeetSemilattice (a × b) where
   meet (a × a') (b × b') = meet a b × meet a' b'

instance (BoundedJoinSemilattice a, BoundedJoinSemilattice b) => BoundedJoinSemilattice (a × b) where
   bot = bot × bot

instance (BoundedMeetSemilattice a, BoundedMeetSemilattice b) => BoundedMeetSemilattice (a × b) where
   top = top × top

instance JoinSemilattice a => JoinSemilattice (Pair a) where
   join ab = definedJoin ab
   maybeJoin (Pair a a') (Pair b b') = Pair <$> maybeJoin a b <*> maybeJoin a' b'

instance JoinSemilattice a => JoinSemilattice (List a) where
   join xs = definedJoin xs
   maybeJoin xs ys
      | (length xs :: Int) == length ys = sequence (zipWith maybeJoin xs ys)
      | otherwise = throw "Mismatched list lengths"

instance JoinSemilattice a => JoinSemilattice (Dict a) where
   join = D.unionWith (∨) -- faster than definedJoin
   maybeJoin m m' = foldM mayFailUpdate m (toUnfoldable m' :: List (Var × a))

instance (Functor f, Neg a) => Neg (f a) where
   neg = (<$>) neg

mayFailUpdate :: forall a m. MonadError Error m => JoinSemilattice a => Dict a -> Var × a -> m (Dict a)
mayFailUpdate m (k × v) =
   case lookup k m of
      Nothing -> pure (insert k v m)
      Just v' -> update <$> (const <$> Just <$> maybeJoin v' v) <@> k <@> m

instance JoinSemilattice a => JoinSemilattice (Array a) where
   join xs = definedJoin xs
   maybeJoin xs ys
      | length xs == (length ys :: Int) = sequence (A.zipWith maybeJoin xs ys)
      | otherwise = throw "Mismatched array lengths"

instance (BoundedJoinSemilattice a, BoundedMeetSemilattice a) => BoundedLattice a

-- Expandable (t :: Type -> Type) requires functor composition..
class Expandable t u | t -> u where
   expand :: t -> u -> t

instance Expandable (t a) (Raw t) => Expandable (a × t a) (Unit × Raw t) where
   expand (α × a) (_ × a') = α × expand a a'

instance Expandable t u => Expandable (Pair t) (Pair u) where
   expand (Pair x x') (Pair y y') = Pair (expand x y) (expand x' y')

instance (BotOf u t, Expandable t u) => Expandable (Dict t) (Dict u) where
   expand kvs kvs' =
      assert (keys kvs `subset` keys kvs') $
         (kvs `D.intersectionWith expand` kvs') D.∪ ((kvs' D.\\ kvs) <#> botOf)

instance Expandable t u => Expandable (List t) (List u) where
   expand xs ys = zipWith expand xs ys

instance Expandable t u => Expandable (Array t) (Array u) where
   expand xs ys = A.zipWith expand xs ys
