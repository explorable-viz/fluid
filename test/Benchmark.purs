module Test.Benchmark where

import Prelude hiding (add)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Test.Util2 (run)

main :: Effect Unit
main = do
   run (liftEffect (log "it worked!"))