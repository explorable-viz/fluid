module Desugarable2 where

import Prelude

import Ann (Raw)
import BoolAlg (BoolAlg)
import Control.Monad.Error.Class (class MonadError)
import Effect.Exception (Error)

class (Functor s, Functor e) <= Desugarable s e | s -> e where
   desug :: forall a m. MonadError Error m => BoolAlg a -> s a -> m (e a)
   desugBwd :: forall a. BoolAlg a -> e a -> Raw s -> s a
