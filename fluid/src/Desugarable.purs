module Desugarable where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import DataType (class HasClasses)
import DefiniteAssignment (VarCxt, WfResult)
import Effect.Exception (Error)

class (Functor s, Functor e) <= Desugarable s e | s -> e where
   desug :: forall m. HasClasses m => MonadError Error m => s (WfResult VarCxt) -> m (e (WfResult VarCxt))
