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
   maybeMeet   :: a -> a -> Maybe a
   maybeJoin   :: a -> a -> Maybe a
   top         :: a -> a
   bot         :: a -> a

join :: forall a . Lattice a => a -> a -> a
join p q = fromJust "Join undefined" $ p ∨? q

meet :: forall a . Lattice a => a -> a -> a
meet p q = fromJust "Meet undefined" $ p ∧? q

-- Give ∧ and ∨ same associativity and precedence as * and +
infixl 7 meet as ∧
infixl 6 join as ∨
infix 7 maybeMeet as ∧?
infix 6 maybeJoin as ∨?

type Selected = Boolean

class Selectable a where
   mapα           :: (Selected -> Selected) -> a -> a
   maybeZipWithα  :: (Selected -> Selected -> Selected) -> a -> a -> Maybe a

instance selectableLattice :: Selectable a => Lattice a where
   maybeJoin   = maybeZipWithα ((||))
   maybeMeet   = maybeZipWithα ((&&))
   top         = mapα $ const true
   bot         = mapα $ const false

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
