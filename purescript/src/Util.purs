module Util where

import Unsafe.Coerce (unsafeCoerce)

__todo :: ∀ a.a
__todo = unsafeCoerce "todo"

error :: String -> ∀ a.a
error msg = unsafeCoerce msg

absurd :: ∀ a.a
absurd = error "Impossible"
