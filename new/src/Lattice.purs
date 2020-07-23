module Lattice where

import Prelude hiding (absurd, join, top)
import Control.Apply (lift2)
import Data.List (List(..), (:), length, zipWith)
import Data.Map (Map, fromFoldable, size, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple)
import Util ((×), (≟), fromJust)

class Lattice a where
   join   :: a -> a -> a
   meet   :: a -> a -> a
   top    :: a -> a
   bot    :: a -> a

class JoinSemilattice a where
   maybeJoin :: a -> a -> Maybe a

class JoinSemilattice a <= BoundedJoinSemilattice a where
   bot2 :: a -> a

-- Give ∧ and ∨ same associativity and precedence as * and +
infixl 7 meet as ∧
infixl 6 join2 as ∨

type 𝔹 = Boolean

class Functor t <= MaybeZippable t where
   maybeZipWith  :: forall a b c . (a -> b -> c) -> t a -> t b -> Maybe (t c)

instance maybeZippableList :: MaybeZippable List where
   maybeZipWith f Nil Nil           = pure Nil
   maybeZipWith f (x : xs) (y : ys) = (pure $ f x y) `lift2 (:)` maybeZipWith f xs ys
   maybeZipWith _ _ _               = Nothing

instance latticeBoolean :: Lattice Boolean where
   join  = (||)
   meet  = (&&)
   top   = const true
   bot   = const false

instance joinSemilatticeBoolean :: JoinSemilattice Boolean where
   maybeJoin x y = pure $ x || y

instance latticeMaybeZippable :: (Lattice a, MaybeZippable t) => Lattice (t a) where
   join x y = fromJust "Join undefined" $ maybeZipWith join x y
   meet x y = fromJust "Meet undefined" $ maybeZipWith meet x y
   top   = map top
   bot   = map bot

join2 :: forall a . JoinSemilattice a => a -> a -> a
join2 x y = fromJust "Join undefined" $ maybeJoin x y

-- Not sure how to do these with instances (need composable type constructors).
maybeZipWithTuple :: forall a b c k t . Eq k => MaybeZippable t =>
   (a -> b -> c) -> Tuple k (t a) -> Tuple k (t b) -> Maybe (Tuple k (t c))
maybeZipWithTuple f (k × v) (k' × v') = (k ≟ k') `lift2 (×)` maybeZipWith f v v'

maybeZipWithMap :: forall a b c k t . Ord k => MaybeZippable t =>
   (a -> b -> c) -> Map k (t a) -> Map k (t b) -> Maybe (Map k (t c))
maybeZipWithMap f κs κs'
   | size κs == size κs' =
      fromFoldable <$> (sequence $ zipWith (maybeZipWithTuple f) (toUnfoldable κs) (toUnfoldable κs'))
   | otherwise = Nothing

maybeZipWithList :: forall a b c t . MaybeZippable t =>
   (a -> b -> c) -> List (t a) -> List (t b) -> Maybe (List (t c))
maybeZipWithList f xs ys
   | length xs == length ys   = sequence $ zipWith (maybeZipWith f) xs ys
   | otherwise                = Nothing

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
