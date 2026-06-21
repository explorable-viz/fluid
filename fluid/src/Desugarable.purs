module Desugarable where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import DefiniteAssignment (class HasClassCtx, Ctx, TyResult)
import Effect.Exception (Error)

class (Functor s, Functor e) <= Desugarable s e | s -> e where
   desug :: forall m. HasClassCtx m => MonadError Error m => s (TyResult Ctx) -> m (e (TyResult Ctx))
