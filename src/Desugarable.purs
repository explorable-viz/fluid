module Desugarable where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Lattice (Raw, class JoinSemilattice, class BoundedJoinSemilattice)

class (Functor s, Functor e) <= Desugarable s e | s -> e where
   desug :: forall a m. MonadError String m => JoinSemilattice a => s a -> m (e a)
   desugBwd :: forall a. BoundedJoinSemilattice a => e a -> Raw s -> s a
