module Lattice where

import Prelude hiding (absurd, join)
import Control.Apply (lift2)
import Data.Function (on)
import Data.List (List, length, zipWith)
import Data.Map (Map, fromFoldable, size, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Util (type (×), (×), (≟), fromJust)

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

instance selectableMap :: (Ord k, Selectable v) => Selectable (Map k v) where
   mapα f = map (mapα f)

   maybeZipWithα f κs κs'
      | (eq `on` size) κs κs' =
         fromFoldable <$> sequence (zipWith (maybeZipWithα f) (toUnfoldable κs) (toUnfoldable κs'))
      | otherwise = Nothing

instance selectableList :: Selectable a => Selectable (List a) where
   mapα f = map (mapα f)

   maybeZipWithα f xs ys
      | (eq `on` length) xs ys   = sequence (zipWith (maybeZipWithα f) xs ys)
      | otherwise                = Nothing
