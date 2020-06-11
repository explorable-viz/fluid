module Lattice where

import Prelude hiding (absurd, join)
import Data.Either (Either(..))
import Data.Map (Map(..))
import Data.Maybe (Maybe(..))
import Util (fromJust)

class Lattice a where
   maybeMeet   :: a -> a -> Maybe a
   maybeJoin   :: a -> a -> Maybe a
   top         :: a -> a
   bot         :: a -> a

join :: forall a . Lattice a => a -> a -> a
join p q = fromJust $ p ∨? q

meet :: forall a . Lattice a => a -> a -> a
meet p q = fromJust $ p ∧? q

-- Give ∧ and ∨ same associativity and precedence as * and +
infixl 7 meet as ∧
infixl 6 join as ∨
infix 7 maybeMeet as ∧?
infix 6 maybeJoin as ∨?

type Selected = Boolean

class Selectable a where
   mapα :: (Selected -> Selected) -> a -> a
   maybeZipWithα :: (Selected -> Selected -> Selected) -> a -> a -> Maybe a

instance selectableLattice :: Selectable a => Lattice a where
   maybeJoin = maybeZipWithα ((||))
   maybeMeet = maybeZipWithα ((&&))
   top = mapα $ const true
   bot = mapα $ const false

instance selectableEither :: (Selectable a, Selectable b) => Selectable (Either a b) where
   mapα f (Left e) = Left $ mapα f e
   mapα f (Right σ) = Right $ mapα f σ

   maybeZipWithα f (Left e) (Left e') = Left <$> maybeZipWithα f e e'
   maybeZipWithα f (Right σ) (Right σ') = Right <$> maybeZipWithα f σ σ'
   maybeZipWithα _ _ _ = Nothing

instance selectableBoolean :: Selectable Boolean where
   mapα = identity
   maybeZipWithα op α α' = pure $ α `op` α'

instance selectableUnit :: Selectable Unit where
   mapα _ = identity
   maybeZipWithα _ _ _ = pure unit

instance selectableMap :: Selectable b => Selectable (Map a b) where
   mapα f = map (mapα f)
   maybeZipWithα f m m' = ?_
