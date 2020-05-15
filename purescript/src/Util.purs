module Util where

import Prelude
import Debug.Trace (trace)
import Effect.Exception (throw)
import Unsafe.Coerce (unsafeCoerce)


error :: String -> ∀ a.a
error msg =
   trace ("Error: " <> msg) \_ ->
   unsafeCoerce $ throw msg

__todo :: ∀ a.a
__todo = error "todo"

absurd :: ∀ a.a
absurd = error "Impossible"
