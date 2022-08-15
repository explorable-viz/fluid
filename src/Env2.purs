module Env2 where

import Prelude
import Data.List (List)
import Data.Map (toUnfoldable)
import Data.Tuple (uncurry)
import Bindings2 (Bind(..), Bindings)
import Bindings2New (Bindings2)
import Util2 (error, unimplemented)
import Util.SnocList2 (SnocList)
import Val2 (Val)

type Env2 a = Bindings2 (List (Val a))

-- A "singleton" env is one where every variable is mapped to a singleton list.
-- Second arg here is a singleton.
update :: forall a . Env2 a -> Env2 a -> Env2 a
update γ γ' = go (uncurry Bind <$> toUnfoldable γ :: SnocList (Bind (List (Val a)))) γ'
   where
   go :: Bindings (List (Val a)) -> Env2 a -> Env2 a
   go = error unimplemented
