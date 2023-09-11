module Test.Benchmark where

import Prelude hiding (add)

import Benchmark.Util (BenchRow)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Module (File(..), open, openDefaultImports)
import Test.Spec.Mocha2 (runMocha)
import Test.Util2 (shouldSatisfy, testWithSetup)

main :: Effect Unit
main = do
   runMocha [ minimalPass, minimalFail ]
   runMocha [ minimalEval ]

minimalPass :: Aff Unit
minimalPass = liftEffect (log "it worked!")

minimalFail :: Aff Unit
minimalFail = shouldSatisfy "bool was false!" false (\x -> x)

minimalEval :: Aff BenchRow
minimalEval = do
   default <- openDefaultImports
   config <- open (File "filter")
   testWithSetup true config default { δv: identity, fwd_expect: "42", bwd_expect: mempty }