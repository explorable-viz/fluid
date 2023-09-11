module Test.Benchmark where

import Prelude hiding (add)

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Test.Spec.Mocha2 (runMocha)
import Test.Util2 (shouldSatisfy)

main :: Effect Unit
main = do
   runMocha [ minimalPass, minimalFail ]

minimalPass :: Aff Unit
minimalPass = liftEffect (log "it worked!")

minimalFail :: Aff Unit
minimalFail = shouldSatisfy "bool was false!" false (\x -> x)