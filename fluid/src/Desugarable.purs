module Desugarable where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import DefiniteAssignment (TyResult)
import Effect.Exception (Error)

class (Functor s, Functor e) <= Desugarable s e | s -> e where
   desug :: forall m. MonadError Error m => s TyResult -> m (e TyResult)
