module Lattice where

import Prelude hiding (absurd, join)
import Control.Apply (lift2)
import Data.List (List, length, zipWith)
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

-- Give ∧ and ∨ same associativity and precedence as * and +
infixl 7 meet as ∧
infixl 6 join as ∨

type Selected = Boolean

class Functor t <= Selectable2 t where
   maybeZipWith  :: forall a b c . (a -> b -> c) -> t a -> t b -> Maybe (t c)

instance selectable2Lattice :: Selectable2 t => Lattice (t Boolean) where
   join x y = fromJust "Join undefined" $ maybeZipWith (||) x y
   meet x y = fromJust "Meet undefined" $ maybeZipWith (&&) x y
   top   = map $ const true
   bot   = map $ const false

-- Not sure how to do these with instances (need curried type constructors)
maybeZipWithTuple :: forall a b c k t . Eq k => Selectable2 t =>
   (a -> b -> c) -> Tuple k (t a) -> Tuple k (t b) -> Maybe (Tuple k (t c))
maybeZipWithTuple f (k × v) (k' × v') = (k ≟ k') `lift2 (×)` maybeZipWith f v v'

maybeZipWithMap :: forall a b c k t . Ord k => Selectable2 t =>
   (a -> b -> c) -> Map k (t a) -> Map k (t b) -> Maybe (Map k (t c))
maybeZipWithMap f κs κs'
   | size κs == size κs' =
      fromFoldable <$> (sequence $ zipWith (maybeZipWithTuple f) (toUnfoldable κs) (toUnfoldable κs'))
   | otherwise = Nothing

maybeZipWithList :: forall a b c t . Selectable2 t =>
   (a -> b -> c) -> List (t a) -> List (t b) -> Maybe (List (t c))
maybeZipWithList f xs ys
   | length xs == length ys   = sequence $ zipWith (maybeZipWith f) xs ys
   | otherwise                = Nothing
