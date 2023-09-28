module Desugarable2 where

import Prelude

import Ann (Raw)
import BoolAlg (BoolAlg)
import Control.Monad.Error.Class (class MonadError)
import Effect.Exception (Error)

class (Functor s, Functor e) <= Desugarable s e | s -> e where
   -- Dropped maybeJoin and just use join to desugar eliminators, so actually I don't think this computation
   -- ever actually fails via m. Revisit
   desug :: forall a m. MonadError Error m => BoolAlg a -> s a -> m (e a)
   desugBwd :: forall a. BoolAlg a -> e a -> Raw s -> s a
