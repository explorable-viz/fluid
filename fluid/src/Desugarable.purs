module Desugarable where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import DefiniteAssignment (class HasCxt, VarCxt, TyResult)
import Effect.Exception (Error)

class (Functor s, Functor e) <= Desugarable s e | s -> e where
   desug :: forall m. HasCxt m => MonadError Error m => s (TyResult VarCxt) -> m (e (TyResult VarCxt))
