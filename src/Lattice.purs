module Lattice where

import Prelude hiding (absurd, (-), join, top)

import Control.Apply (lift2)
import Data.Array (zipWith) as A
import Data.Bifunctor (bimap)
import Data.Foldable (length)
import Data.List (List, zipWith)
import Data.Profunctor.Strong ((***))
import Data.Set (subset)
import Dict (Dict)
import Util (type (×), Endo, assert, shapeMismatch, (×))
import Util.Map (intersectionWith, keys, unionWith)
import Util.Map as Map
import Util.Pair (Pair(..))
import Util.Set ((∪))

-- join here is actually more general "weak join" operation of the formalism, which operates on maps using unionWith.
class JoinSemilattice a where
   join :: a -> a -> a

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

relativeComplement :: forall a. Neg a => MeetSemilattice a => a -> a -> a
relativeComplement a = neg >>> (_ ∧ a)

instance JoinSemilattice Boolean where
   join = (||)

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

instance MeetSemilattice Unit where
   meet _ = identity

instance BoundedJoinSemilattice Unit where
   bot = unit

instance BoundedMeetSemilattice Unit where
   top = unit

instance Neg Unit where
   neg = identity

instance (Functor f, BoundedJoinSemilattice a) => BotOf (Unit × Raw f) (a × f a) where
   botOf = const bot *** botOf -- for dictionary selections
else instance (Functor g, BotOf (f a) (f a')) => BotOf (g (f a)) (g (f a')) where
   botOf = (<$>) botOf
else instance (Functor f, BoundedJoinSemilattice a') => BotOf (f a) (f a') where
   botOf = (<$>) (const bot)
else instance (BotOf a b, BotOf c d) => BotOf (a × c) (b × d) where
   botOf = botOf *** botOf

instance (Functor f, BoundedMeetSemilattice a) => TopOf (Unit × Raw f) (a × f a) where
   topOf = const top *** ((<$>) (const top)) -- for dictionary selections
else instance (Functor g, TopOf (f a) (f a')) => TopOf (g (f a)) (g (f a')) where
   topOf = (<$>) topOf
else instance (Functor f, BoundedMeetSemilattice a') => TopOf (f a) (f a') where
   topOf = (<$>) (const top)
else instance (TopOf a b, TopOf c d) => TopOf (a × c) (b × d) where
   topOf = topOf *** topOf

-- Specialises botOf and topOf but omits the lattice constraint.
erase :: forall f a. Functor f => f a -> Raw f
erase = (<$>) (const unit)

-- Same associativity and precedence as * and +
infixl 7 meet as ∧
infixl 6 join as ∨
infixl 6 relativeComplement as -

symmetricDiff :: forall a. Neg a => MeetSemilattice a => a -> a -> a × a
symmetricDiff x y = (x - y) × (y - x)

type 𝔹 = Boolean
type Raw (f :: Type -> Type) = f Unit

-- Don't lift to arbitrary functor because we relax join to allow different (but compatible) shapes
instance (JoinSemilattice a, JoinSemilattice b) => JoinSemilattice (a × b) where
   join (a × a') (b × b') = (a ∨ b) × (a' ∨ b')

instance (MeetSemilattice a, MeetSemilattice b) => MeetSemilattice (a × b) where
   meet (a × a') (b × b') = meet a b × meet a' b'
else instance MeetSemilattice a => MeetSemilattice (Dict a) where
   meet = unionWith (∧) -- intersectionWith? in fact shouldn't we require equal domains?
else instance (Functor f, Apply f, MeetSemilattice a) => MeetSemilattice (f a) where
   meet = lift2 (∧)

instance (BoundedJoinSemilattice a, BoundedJoinSemilattice b) => BoundedJoinSemilattice (a × b) where
   bot = bot × bot

instance (BoundedMeetSemilattice a, BoundedMeetSemilattice b) => BoundedMeetSemilattice (a × b) where
   top = top × top

instance (Neg a, Neg b) => Neg (a × b) where
   neg x = bimap neg neg x
else instance (Functor f, Neg a) => Neg (f a) where
   neg x = neg <$> x

instance (BooleanLattice a, BooleanLattice b) => BooleanLattice (a × b)
else instance (BoundedLattice (f a), Neg (f a)) => BooleanLattice (f a)

instance JoinSemilattice a => JoinSemilattice (Pair a) where
   join (Pair a a') (Pair b b') = Pair (a ∨ b) (a' ∨ b')

instance JoinSemilattice a => JoinSemilattice (List a) where
   join xs ys
      | (length xs :: Int) == length ys = zipWith (∨) xs ys
      | otherwise = shapeMismatch unit

instance JoinSemilattice a => JoinSemilattice (Dict a) where
   join = unionWith (∨)

instance JoinSemilattice a => JoinSemilattice (Array a) where
   join xs ys
      | length xs == (length ys :: Int) = A.zipWith (∨) xs ys
      | otherwise = shapeMismatch unit

instance (BoundedJoinSemilattice a, BoundedMeetSemilattice a) => BoundedLattice a

-- Expandable (t :: Type -> Type) requires functor composition.
class Expandable t u | t -> u where
   expand :: t -> u -> t

instance Expandable (t a) (Raw t) => Expandable (a × t a) (Unit × Raw t) where
   expand (α × a) (_ × a') = α × expand a a'

instance Expandable t u => Expandable (Pair t) (Pair u) where
   expand (Pair x x') (Pair y y') = Pair (expand x y) (expand x' y')

instance (BotOf u t, Expandable t u) => Expandable (Dict t) (Dict u) where
   expand kvs kvs' =
      assert (keys kvs `subset` keys kvs') $
         (kvs `intersectionWith expand` kvs') ∪ ((kvs' Map.\\ kvs) <#> botOf)

instance Expandable t u => Expandable (List t) (List u) where
   expand xs = zipWith expand xs

instance Expandable t u => Expandable (Array t) (Array u) where
   expand xs = A.zipWith expand xs
