module Env2 where

import Data.List (List)
import Bindings2New (Bindings2)
import Val2 (Val)

type Env2 a = Bindings2 (List (Val a))
