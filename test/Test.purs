module Test.Test where

import Prelude hiding (add)

import App.Util.Selector (barChart, barSegment, multiViewEntry, select)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader (class MonadReader)
import Data.Array (concat)
import Data.Profunctor.Strong (second)
import Effect (Effect)
import Effect.Aff (Error)
import Effect.Aff.Class (class MonadAff)
import File (class LoadFile, FileCxt(..))
import Module.Web (runWebT)
import Test.Specs.Bwd (bwd_cases)
import Test.Specs.Comments (comments_cases)
import Test.Specs.Desugar (desugar_cases)
import Test.Specs.Graphics (graphics_cases)
import Test.Specs.LinkedInputs (linkedInputs_cases)
import Test.Specs.LinkedOutputs (linkedOutputs_cases)
import Test.Specs.Misc (misc_cases)
import Test.Util (TestSuite, fluidSrcPaths)
import Test.Util.Mocha (run)
import Test.Util.Suite (BenchSuite, bwdSuite, linkedInputsSuite, linkedOutputsSuite, suite, withDatasetSuite)
import Util ((×))

main :: Effect Unit
--main = run (second (runWebT (FileCxt { fluidSrcPaths })) <$> tests)

main = run (second (runWebT (FileCxt { fluidSrcPaths })) <$> scratchpad)

scratchpad :: forall m. MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => TestSuite m
scratchpad = asTestSuite $ bwdSuite
   [ { file: "linked-outputs/bar-chart-line-chart.fld"
     , bwd_expect_file: "linked-outputs/bar-chart-line-chart.expect.fld"
     , δv: multiViewEntry "barChart" (barChart (barSegment 1 0 select))
     , fwd_expect: "MultiView {[\"barChart\"] : BarChart {[\"caption\"] : \"Total output by country\", [\"size\"] : {[\"height\"] : 185, [\"width\"] : 275}, [\"stackedBars\"] : ({[\"segments\"] : ({[\"y\"] : \"output\", [\"z\"] : 295.3} : []), [\"x\"] : \"China\"} : ({[\"segments\"] : ({[\"y\"] : \"output\", [\"z\"] : ⸨196.7⸩} : []), [\"x\"] : \"USA\"} : ({[\"segments\"] : ({[\"y\"] : \"output\", [\"z\"] : 97.69999999999999} : []), [\"x\"] : \"Germany\"} : [])))}, [\"lineChart\"] : LineChart {[\"caption\"] : \"Output of USA relative to China\", [\"plots\"] : (LinePlot {[\"name\"] : \"Bio\", [\"points\"] : ({[\"x\"] : 2013, [\"y\"] : 2.5483870967741935} : ({[\"x\"] : 2014, [\"y\"] : 1.61} : ({[\"x\"] : 2015, [\"y\"] : 1.6213592233009706} : ({[\"x\"] : 2016, [\"y\"] : 1.4000000000000001} : ({[\"x\"] : 2017, [\"y\"] : 1.1208053691275166} : ({[\"x\"] : 2018, [\"y\"] : 0.9101123595505617} : []))))))} : (LinePlot {[\"name\"] : \"Hydro\", [\"points\"] : ({[\"x\"] : 2013, [\"y\"] : 0.3} : ({[\"x\"] : 2014, [\"y\"] : 0.28214285714285714} : ({[\"x\"] : 2015, [\"y\"] : 0.8333333333333334} : ({[\"x\"] : 2016, [\"y\"] : 0.26229508196721313} : ({[\"x\"] : 2017, [\"y\"] : 0.25559105431309903} : ({[\"x\"] : 2018, [\"y\"] : 0.2484472049689441} : []))))))} : (LinePlot {[\"name\"] : \"Solar\", [\"points\"] : ({[\"x\"] : 2013, [\"y\"] : 0.6080402010050252} : ({[\"x\"] : 2014, [\"y\"] : 0.6428571428571429} : ({[\"x\"] : 2015, [\"y\"] : 0.5909090909090909} : ({[\"x\"] : 2016, [\"y\"] : 0.5324675324675324} : ({[\"x\"] : 2017, [\"y\"] : 0.3893129770992366} : ({[\"x\"] : 2018, [\"y\"] : 0.3522727272727273} : []))))))} : (LinePlot {[\"name\"] : \"Wind\", [\"points\"] : ({[\"x\"] : 2013, [\"y\"] : 0.6703296703296703} : ({[\"x\"] : 2014, [\"y\"] : 0.5739130434782609} : ({[\"x\"] : 2015, [\"y\"] : 0.5103448275862069} : ({[\"x\"] : 2016, [\"y\"] : 0.48520710059171596} : ({[\"x\"] : 2017, [\"y\"] : 0.4734042553191489} : ({[\"x\"] : 2018, [\"y\"] : 0.45714285714285713} : []))))))} : [])))), [\"size\"] : {[\"height\"] : 285, [\"width\"] : 330}, [\"tickLabels\"] : {[\"x\"] : Default, [\"y\"] : Default}}}"
     , datasets: []
     }
   ]

-- scratchpad :: forall m. MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => TestSuite m
-- scratchpad = asTestSuite $ bwdSuite bwd_cases

asTestSuite :: forall m. MonadAff m => MonadError Error m => LoadFile m => BenchSuite m -> TestSuite m
asTestSuite suite = second void <$> suite (1 × false)

tests :: forall m. MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => TestSuite m
tests = concat (benchmarks <#> asTestSuite)
   <> linkedOutputsSuite linkedOutputs_cases
   <> linkedInputsSuite linkedInputs_cases

benchmarks :: forall m. MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => Array (BenchSuite m)
benchmarks =
   [ suite desugar_cases
   , suite misc_cases
   , suite comments_cases
   , bwdSuite bwd_cases
   , withDatasetSuite graphics_cases
   ]
