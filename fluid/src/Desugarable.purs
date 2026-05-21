module Desugarable where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Effect.Exception (Error)
import Lattice (class BoundedLattice)

class (Functor s, Functor e) <= Desugarable s e | s -> e where
   desug :: forall a m. MonadError Error m => BoundedLattice a => s a -> m (e a)
