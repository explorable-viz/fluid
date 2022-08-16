module Bindings2New where

import Data.Map (Map, keys, unionWith)
import Data.Set (Set)
import Bindings2 (Var)
import Util2 (Endo, error)

dom :: forall a . Map Var a -> Set Var
dom = keys

disjUnion :: forall a . Map Var a -> Endo (Map Var a)
disjUnion = unionWith (\_ _ -> error "not disjoint")
