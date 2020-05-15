module Util where

import Prelude
import Debug.Trace (trace)
import Effect.Exception (error) as X
import Effect.Exception (throwException)
import Unsafe.Coerce (unsafeCoerce)


error :: String -> ∀ a.a
error msg =
   trace ("Error: " <> msg) \_ ->
   unsafeCoerce $ throwException $ X.error msg

__todo :: ∀ a.a
__todo = error "todo"

absurd :: ∀ a.a
absurd = error "Impossible"
