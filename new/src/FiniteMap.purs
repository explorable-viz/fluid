module FiniteMap where
-- PureScript's map doesn't give us access to its constructors.

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Lattice (class Selectable, mapα, maybeZipWithα)
import Util ((≟))

data Tree a =
   Empty |
   NonEmpty (Tree a) a (Tree a)

derive instance functorTree :: Functor Tree

type FiniteMap k v = Tree (Tuple k v)

lookup :: forall k v . Ord k => k -> FiniteMap k v -> Maybe v
lookup k Empty = Nothing
lookup k (NonEmpty t (Tuple k' v) t') =
   if k < k'
   then lookup k t
   else
      if k > k'
      then lookup k t'
      else Just v

instance treeSelected :: (Eq k, Selectable v) => Selectable (Tree (Tuple k v)) where
   mapα f Empty                        = Empty
   mapα f (NonEmpty t (Tuple k v) t')  = NonEmpty (mapα f t) (Tuple k $ mapα f v) (mapα f t')

   maybeZipWithα f Empty Empty                                                   = pure Empty
   maybeZipWithα f (NonEmpty t1 (Tuple k v) t2) (NonEmpty t1' (Tuple k' v') t2') =
      NonEmpty <$> maybeZipWithα f t1 t1'
               <*> (Tuple <$> k ≟ k' <*> maybeZipWithα f v v')
               <*> maybeZipWithα f t2 t2'
   maybeZipWithα _ _ _                                                           = Nothing
