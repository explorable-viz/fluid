module Util where

import Prelude
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)


error :: ∀ a . String -> a
error = unsafePerformEffect <<< throw

__todo :: ∀ a.a
__todo = error "todo"

absurd :: ∀ a.a
absurd = error "Impossible"
