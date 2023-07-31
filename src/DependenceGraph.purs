module DependenceGraph where

import Prelude

import Data.Foldable (class Foldable)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object (Object) as O

newtype Graph v = Graph (O.Object (Tuple v (Array String)))

entry :: Effect Unit
entry = do 
          log "hi"

class DepGraph g where
    merge :: g -> g -> Maybe g
    allocate :: forall a. a -> g -> a
