module Util where

import Prelude
import Debug.Trace (trace)
import Effect.Exception (throw)
import Unsafe.Coerce (unsafeCoerce)


error :: ∀ a . String -> a
error msg =
   trace msg \_ ->
   unsafeCoerce $ throw msg

__todo :: ∀ a.a
__todo = error "todo"

absurd :: ∀ a.a
absurd = error "Impossible"
