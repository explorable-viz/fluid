module FiniteMap where
-- PureScript's map doesn't give us access to its constructors.

import Prelude
import Data.Either (Either(..))
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Lattice (class Selectable, mapα, maybeZipWithα)
import Util (MayFail, (≟), error)

data Tree a =
   Empty |
   NonEmpty (Tree a) a (Tree a)

derive instance functorTree :: Functor Tree

values :: forall a . Tree a -> List a
values Empty               = Nil
values (NonEmpty t a t')   = values t <> pure a <> values t'

type FiniteMap k v = Tree (Tuple k v)

lookup :: forall k v . Show k => Ord k => k -> FiniteMap k v -> MayFail (Tuple v (FiniteMap k v))
lookup k Empty = Left $ show k <> " not found"
lookup k (NonEmpty t (Tuple k' v) t') =
   if k < k'
   then lookup k t
   else
      if k > k'
      then lookup k t'
      else error "TODO"

insert :: forall k v . Ord k => k -> v -> FiniteMap k v -> FiniteMap k v
insert k v Empty = NonEmpty Empty (Tuple k v) Empty
insert k v (NonEmpty t kv@(Tuple k' v') t') =
   if k <= k'
   then
      if (k' <= k)
      then NonEmpty t (Tuple k v) t'
      else NonEmpty (insert k v t) kv t'
   else NonEmpty t kv t'

-- union :: forall k v . Ord k => FiniteMap k v -> FiniteMap k v -> FiniteMap k v
-- union Empty Empty = Empty

instance treeSelected :: (Eq k, Selectable v) => Selectable (Tree (Tuple k v)) where
   mapα f Empty                        = Empty
   mapα f (NonEmpty t (Tuple k v) t')  = NonEmpty (mapα f t) (Tuple k $ mapα f v) (mapα f t')

   maybeZipWithα f Empty Empty                                                   = pure Empty
   maybeZipWithα f (NonEmpty t1 (Tuple k v) t2) (NonEmpty t1' (Tuple k' v') t2') =
      NonEmpty <$> maybeZipWithα f t1 t1'
               <*> (Tuple <$> k ≟ k' <*> maybeZipWithα f v v')
               <*> maybeZipWithα f t2 t2'
   maybeZipWithα _ _ _                                                           = Nothing
