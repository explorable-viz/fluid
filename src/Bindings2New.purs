module Bindings2New where

import Data.Map (Map, filterKeys, keys)
import Data.Set (Set, member)

type Var = String -- newtype?

type Bindings2 a = Map Var a

dom :: forall a . Bindings2 a -> Set Var
dom = keys

restrict :: forall a . Set Var -> Bindings2 a -> Bindings2 a
restrict xs = filterKeys (_ `member` xs)
