module Test.Test where

import Prelude hiding (add)

import Control.Monad.Error.Class (class MonadError)
import Data.Array (concat)
import Data.Profunctor.Strong (second)
import Data.Traversable (sequence)
import Data.Tuple (snd)
import Effect (Effect)
import Effect.Aff (Aff, Error)
import Effect.Aff.Class (class MonadAff)
import File (class LoadFile)
import Module.Web (runWebT)
import Test.Specs.Bwd (bwd_cases)
import Test.Specs.Comments (comments_cases)
import Test.Specs.Desugar (desugar_cases)
import Test.Specs.Graphics (graphics_cases)
import Test.Specs.LinkedInputs (linkedInputs_cases)
import Test.Specs.LinkedOutputs (linkedOutputs_cases)
import Test.Specs.Misc (misc_cases)
import Test.Util (TestSuite, TestSuite2, TestSuite3)
import Test.Util.Mocha (run)
import Test.Util.Suite (BenchSuite, BenchSuite3, bwdSuite, linkedInputsSuite, linkedOutputsSuite, suite, withDatasetSuite)
import Util (type (×), (×))

main :: Effect Unit
main = run [] --tests

-- main = run scratchpad

{-
scratchpad :: TestSuite
scratchpad = asTestSuite $ suite
   [ { file: "comments/projection"
     , imports: []
     , fwd_expect: "\"\"\" Test \"\"\" 1"
     }
   ]
-}
asTestSuite :: BenchSuite -> TestSuite
asTestSuite suite = second void <$> suite (1 × false)

asTestSuite2 :: forall m. MonadAff m => MonadError Error m => LoadFile m => BenchSuite3 m -> TestSuite3 m
asTestSuite2 suite = second void <$> suite (1 × false)

nib :: (forall m. LoadFile m => String × m Unit) -> String × Aff Unit
nib quib = second runWebT quib

nib2 :: (forall m. LoadFile m => m Unit) -> Aff Unit
nib2 quib = runWebT quib

blah :: (forall m. TestSuite2 m) -> Array (Aff (String × Unit))
blah suite = runWebT <$> sequence <$> suite

bibble :: (forall m. TestSuite2 m) -> forall m. MonadAff m => MonadError Error m => LoadFile m => Array (m Unit)
bibble suite = suite <#> snd

blah2 :: (forall m. MonadAff m => MonadError Error m => LoadFile m => Array (m Unit)) -> Array (Aff Unit)
blah2 suite = suite <#> runWebT

tests2 :: forall m. MonadAff m => MonadError Error m => LoadFile m => TestSuite3 m
tests2 = concat (benchmarks' <#> asTestSuite2)
   <> linkedOutputsSuite linkedOutputs_cases
   <> linkedInputsSuite linkedInputs_cases

benchmarks' :: forall m. MonadAff m => MonadError Error m => LoadFile m => Array (BenchSuite3 m)
benchmarks' =
   [ suite desugar_cases
   , suite misc_cases
   , suite comments_cases
   , bwdSuite bwd_cases
   , withDatasetSuite graphics_cases
   ]
