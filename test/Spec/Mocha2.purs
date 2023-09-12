module Test.Spec.Mocha2
   ( runMocha
   , MOCHA()
   ) where

import Prelude


import Data.Either (either)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, Error, runAff_)

foreign import data MOCHA :: Type

foreign import itAsync
   :: Boolean
   -> String
   -> ( Effect Unit
        -> (Error -> Effect Unit)
        -> Effect Unit
      )
   -> Effect Unit

foreign import itPending
   :: String
   -> Effect Unit

foreign import describe
   :: Boolean
   -> String
   -> Effect Unit
   -> Effect Unit

registerGroup
   :: forall a. Aff a -> Effect Unit
registerGroup example = itAsync true "It works!" cb
   where
   cb :: Effect Unit -> (Error -> Effect Unit) -> Effect Unit
   cb onSuccess onError =
      runAff_ (either onError (const onSuccess)) example

runMocha :: forall a. Array (Aff a) -> Effect Unit
runMocha = traverse_ registerGroup

