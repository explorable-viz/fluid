module Test.Fluid.Fluid where

import Prelude

import Control.Promise (fromAff)
import Data.Foldable (sequence_)
import Effect (Effect)
import Effect.Aff (Aff)
import EvalGraph (graphEval)
import Lattice (erase)
import Module.Node (File(..), loadProgCxt, prepConfig)
import Pretty (prettyP)
import Test.Util (testCondition)

main :: Effect Unit
main = do
   _promise <- fromAff $ sequence_ [ testFluid ]
   pure unit

testFluid :: Aff Unit
testFluid = do
   progCxt <- loadProgCxt [] []
   { e, gconfig } <- prepConfig (File "length") progCxt
   { outα } <- graphEval gconfig e
   testCondition "Fluid" ((prettyP (erase outα)) == "2") (prettyP outα)
   pure unit