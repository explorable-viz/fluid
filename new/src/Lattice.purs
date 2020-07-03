module Lattice where

import Prelude hiding (absurd, join)
import Control.Apply (lift2)
import Data.Function (on)
import Data.List (List(..), (:), length, zipWith)
import Data.Map (Map, fromFoldable, size, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple)
import Util (type (×), (×), (≟), error, fromJust)

class Lattice a where
   join   :: a -> a -> a
   meet   :: a -> a -> a
   top    :: a -> a
   bot    :: a -> a

-- Give ∧ and ∨ same associativity and precedence as * and +
infixl 7 meet as ∧
infixl 6 join as ∨

type Selected = Boolean

class Selectable a where
   mapα           :: (Selected -> Selected) -> a -> a
   maybeZipWithα  :: (Selected -> Selected -> Selected) -> a -> a -> Maybe a

class Selectable2 t where
   maybeZipWith  :: forall a b c . (a -> b -> c) -> t a -> t b -> Maybe (t c)

instance selectableLattice :: Selectable a => Lattice a where
   join x y = fromJust "Join undefined" $ maybeZipWithα (||) x y
   meet x y = fromJust "Meet undefined" $ maybeZipWithα (&&) x y
   top   = mapα $ const true
   bot   = mapα $ const false

instance selectableBoolean :: Selectable Boolean where
   mapα                    = identity
   maybeZipWithα op α α'   = pure $ α `op` α'

instance selectableUnit :: Selectable Unit where
   mapα _               = identity
   maybeZipWithα _ _ _  = pure unit

instance selectableTuple :: (Eq k, Selectable v) => Selectable (k × v) where
   mapα f (k × v)                    = k × mapα f v
   maybeZipWithα f (k × v) (k' × v') = (k ≟ k') `lift2 (×)` maybeZipWithα f v v'

instance selectable2Tuple :: (Eq k) => Selectable2 (Tuple k) where
   maybeZipWith f (k × v) (k' × v') = (k ≟ k') `lift2 (×)` (error "todo") -- maybeZipWith ?_ v v'

instance selectableMap :: (Ord k, Selectable v) => Selectable (Map k v) where
   mapα f = map (mapα f)

   maybeZipWithα f κs κs'
      | (eq `on` size) κs κs' =
         fromFoldable <$> sequence (zipWith (maybeZipWithα f) (toUnfoldable κs) (toUnfoldable κs'))
      | otherwise = Nothing

instance selectable2Map :: (Ord k) => Selectable2 (Map k) where
   maybeZipWith f κs κs'
      | size κs == size κs' =
         fromFoldable <$> sequence (zipWith (maybeZipWith f) (toUnfoldable κs) (toUnfoldable κs'))
      | otherwise = Nothing

instance selectableList :: Selectable a => Selectable (List a) where
   mapα f = map (mapα f)

   maybeZipWithα f xs ys
      | (eq `on` length) xs ys   = sequence (zipWith (maybeZipWithα f) xs ys)
      | otherwise                = Nothing

instance selectable2List :: Selectable2 List where
   maybeZipWith f xs ys
      | length xs == length ys   = pure $ zipWith f xs ys
      | otherwise                = Nothing

maybeZipWithList :: forall a b c t . (a -> b -> c) -> List (t a) -> List (t b) -> Maybe (List (t c))
maybeZipWithList _ Nil Nil             = pure Nil
maybeZipWithList f (x : xs) (y : ys)   = f x y : error "todo" -- maybeZipWith (?_) vs vs'
maybeZipWithList _ _ _                 = Nothing
