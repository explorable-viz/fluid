module Test.Util2 where

import Prelude hiding (absurd)

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Effect.Exception (Error, error)

shouldSatisfy :: forall m t. MonadThrow Error m => Show t => String -> t -> (t -> Boolean) -> m Unit
shouldSatisfy msg v pred =
   unless (pred v)
      $ fail
      $ show v <> " doesn't satisfy predicate: " <> msg
   where
   fail :: forall m'. MonadThrow Error m' => String -> m' Unit
   fail = throwError <<< error
