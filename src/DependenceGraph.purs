module DependenceGraph where

import Prelude

import Data.Bifunctor (lmap)
import Data.Foldable (class Foldable, foldl, foldr, foldMap)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..), fst)
import Foreign.Object (Object) as O

newtype Graph v = Graph (O.Object (Tuple v (Array String)))

instance Functor Graph where
    map f (Graph o) = Graph (map (lmap f) o)

class DepGraph g where
    merge :: g -> g -> Maybe g
    allocate :: forall a. a -> g -> a
