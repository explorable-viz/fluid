module Set where

import Prelude
import Data.Foldable (class Foldable)
import Data.Set as S
import Data.Unfoldable (class Unfoldable)

class (Ord a, Ord (s a), Foldable s) <= Set s a where
   delete :: a -> s a -> s a
   union :: s a -> s a -> s a
   insert :: a -> s a -> s a
   member :: a -> s a -> Boolean
   subset :: s a -> s a -> Boolean
   singleton :: a -> s a
   empty :: s a
   map :: forall b. Ord b => (a -> b) -> s a -> s b
   fromFoldable :: forall f. Foldable f => f a -> s a
   toUnfoldable :: forall f. Unfoldable f => s a -> f a

instance Ord a => Set S.Set a where
   delete = S.delete
   union = S.union
   insert = S.insert
   member = S.member
   singleton = S.singleton
   subset = S.subset
   empty = S.empty
   map = S.map
   fromFoldable = S.fromFoldable
   toUnfoldable = S.toUnfoldable
