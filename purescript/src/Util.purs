module Util where

import Unsafe.Coerce (unsafeCoerce)

__todo :: forall a.a
__todo = unsafeCoerce "todo"

error :: String -> forall a.a
error msg = unsafeCoerce msg
