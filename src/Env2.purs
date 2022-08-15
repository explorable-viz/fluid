module Env2 where

import Data.List (List)
import Bindings2New (Bindings2)
import Util2 (error, unimplemented)
import Val2 (Val)

type Env2 a = Bindings2 (List (Val a))
-- a "singleton" env is one where every variable is mapped to a singleton list

-- second arg is a singleton
update :: forall a . Bindings2 a -> Bindings2 a -> Bindings2 a
update _ _ = error unimplemented
