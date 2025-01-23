module Test.Fluid where

import Prelude

import Control.Promise (fromAff)
import Data.Foldable (sequence_)
import Effect (Effect)
import Effect.Aff (Aff)
import Fluid (Program(..), evaluate)
import Lattice (erase)
import Pretty (prettyP)
import Test.Util (testCondition)

main :: Effect Unit
main = do
   void $ fromAff $ sequence_ [ testFluid ]

testFluid :: Aff Unit
testFluid = do
   outα <- evaluate $ Program
      { imports: []
      , datasets: []
      , fileName: "example/length"
      }
   testCondition "length" ((prettyP $ erase outα) == "2") (prettyP outα)
