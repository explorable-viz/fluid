module Util where

import Prelude
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)


error :: ∀ a . String -> a
error = unsafePerformEffect <<< throw

todo :: String
todo = "todo"

absurd :: String
absurd = "Absurd"
