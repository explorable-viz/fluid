module Lattice where

import Prelude hiding (absurd, join)
import Data.Either (Either(..))
import Data.List (List(..)) as L
import Data.List (List, (:), zipWith)
import Data.Map (Map, fromFoldable, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Util (type (×), (≟), fromJust)

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
   mapα f (Tuple k v)                        = Tuple k (mapα f v)
   maybeZipWithα f (Tuple k v) (Tuple k' v') = Tuple <$> k ≟ k' <*> maybeZipWithα f v v'

instance selectableMap :: (Ord k, Selectable v) => Selectable (Map k v) where
   mapα f = map (mapα f)
   maybeZipWithα f κs κs' =
      -- should require the maps to have the same cardinality
      fromFoldable <$> sequence (zipWith (maybeZipWithα f) (toUnfoldable κs) (toUnfoldable κs'))

instance listSelectable :: Selectable a => Selectable (List a) where
   mapα f (x:xs) = (mapα f x) : (mapα f xs)
   mapα f L.Nil  = L.Nil
   maybeZipWithα f (x:xs) (y:ys) =
      L.Cons <$> maybeZipWithα f x y <*> (maybeZipWithα f xs ys)
   maybeZipWithα f L.Nil L.Nil = Just L.Nil
   maybeZipWithα f _   _       = Nothing

