module Bindings2New where

import Data.Map (Map, filterKeys, keys, unionWith)
import Data.Set (Set, member)
import Bindings2 (Var)
import Util2 (Endo, error)

dom :: forall a . Map Var a -> Set Var
dom = keys

restrict :: forall a . Set Var -> Endo (Map Var a)
restrict xs = filterKeys (_ `member` xs)

disjUnion :: forall a . Map Var a -> Endo (Map Var a)
disjUnion = unionWith (\_ _ -> error "not disjoint")
