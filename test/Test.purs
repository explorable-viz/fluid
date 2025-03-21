module Test.Test where

import Prelude hiding (add)

import App.Util.Selector (fst, matrixElement, snd)
import Bind ((↦))
import Data.Array (concat)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong (second)
import Effect (Effect)
import Graph (DVertex'(..))
import Lattice (neg)
import Module.Web (File(..), Folder(..), loadFile)
import Test.Specs.Bwd (bwd_cases)
import Test.Specs.Desugar (desugar_cases)
import Test.Specs.Graphics (graphics_cases)
import Test.Specs.LinkedInputs (linkedInputs_cases)
import Test.Specs.LinkedOutputs (linkedOutputs_cases)
import Test.Specs.Misc (misc_cases)
import Test.Util (TestSuite)
import Test.Util.Mocha (run)
import Test.Util.Suite (BenchSuite, bwdSuite, linkedInputsSuite, linkedOutputsSuite, suite, withDatasetSuite)
import Util ((×))
import Val (BaseVal(..), MatrixDim(..), MatrixRep(..), Val(..), asVal)

main :: Effect Unit
main = run tests

-- main = run scratchpad

scratchpad :: TestSuite
scratchpad = linkedOutputsSuite
   [ { spec:
          { fluidSrcPaths: [ Folder "fluid", Folder "test/fluid" ]
          , datasets: [ "data" ↦ "linked-outputs/convolution-data" ]
          , imports: [ "lib/convolution" ]
          , file: File "linked-outputs/convolution"
          , inputs: [ "data" ]
          , query:
               Just $ asVal >=> case _ of
                  v@(Val α (Matrix (MatrixRep (_ × MatrixDim (3 × _) × MatrixDim (3 × _))))) -> Just $ DVertex (α × v)
                  _ -> Nothing
          }
     , δ_out: fst (matrixElement 2 2 neg)
     , out_expect:
          fst
             ( matrixElement 2 1 neg
                  >>> matrixElement 2 2 neg
                  >>> matrixElement 2 3 neg
                  >>> matrixElement 2 4 neg
                  >>> matrixElement 2 5 neg
             )
             >>> snd
                ( matrixElement 1 1 neg
                     >>> matrixElement 1 2 neg
                     >>> matrixElement 1 3 neg
                     >>> matrixElement 2 1 neg
                     >>> matrixElement 2 2 neg
                     >>> matrixElement 2 3 neg
                     >>> matrixElement 3 1 neg
                     >>> matrixElement 3 2 neg
                     >>> matrixElement 3 3 neg
                )
     }
   ]

asTestSuite :: BenchSuite -> TestSuite
asTestSuite suite = second void <$> suite (1 × false)

tests :: TestSuite
tests = concat (benchmarks <#> asTestSuite)
   <> linkedOutputsSuite linkedOutputs_cases
   <> linkedInputsSuite linkedInputs_cases

benchmarks :: Array BenchSuite
benchmarks =
   [ suite loadFile desugar_cases
   , suite loadFile misc_cases
   , bwdSuite loadFile bwd_cases
   , withDatasetSuite loadFile graphics_cases
   ]
