module Temp.Util.UnsafeDebug where

import Prelude

foreign import writeFileUnsafe :: String -> String -> Unit
foreign import exitUnsafe :: Unit -> Unit
foreign import logUnsafe :: String -> Unit
foreign import logErrorUnsafe :: String -> Unit
