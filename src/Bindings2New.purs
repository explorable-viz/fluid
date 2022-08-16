module Bindings2New where

import Data.Map (Map, filterKeys, keys)
import Data.Set (Set, member)
import Bindings2 (Var)

dom :: forall a . Map Var a -> Set Var
dom = keys

restrict :: forall a . Set Var -> Map Var a -> Map Var a
restrict xs = filterKeys (_ `member` xs)
