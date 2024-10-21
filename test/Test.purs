module Test.Test where

import Prelude hiding (add)

import App.Util.Selector (barChart, barSegment, multiViewEntry)
import Bind ((↦))
import Data.Array (concat)
import Data.Profunctor.Strong (second)
import Effect (Effect)
import Lattice (neg)
import Test.Benchmark (benchmarks)
import Test.Specs.LinkedInputs (linkedInputs_cases)
import Test.Specs.LinkedOutputs (linkedOutputs_cases)
import Test.Util (TestSuite)
import Test.Util.Mocha (run)
import Test.Util.Suite (BenchSuite, bwdSuite, linkedInputsSuite, linkedOutputsSuite)
import Util ((×))

main :: Effect Unit
main = run tests

--main = run scratchpad

scratchpad :: TestSuite
scratchpad = asTestSuite $ bwdSuite
   [ { file: "linked-outputs/stacked-bar-scatter-plot"
     , imports: []
     , bwd_expect_file: "linked-outputs/stacked-bar-scatter-plot.expect"
     , δv: multiViewEntry "stacked-bar-chart" (barChart (barSegment 3 2 neg >>> barSegment 4 1 neg >>> barSegment 4 3 neg))
     , fwd_expect: "MultiView {|[\"scatter-plot\"] : ScatterPlot {|[\"caption\"] : \"Clean energy efficiency vs proportion of renewable energy capacity\", [\"points\"] : ({|[\"x\"] : 0.8723185510332055, [\"y\"] : 0.4180741155728385|} : ({|[\"x\"] : 0.383891020964826, [\"y\"] : 0.3306374135311273|} : ({|[\"x\"] : 0.5685559399722339, [\"y\"] : 0.2651713517303818|} : ({|[\"x\"] : 0.39179907463864283, [\"y\"] : 0.5311676111397315|} : ({|[\"x\"] : 0.0886691179578209, [\"y\"] : 0.4125357483317445|} : ({|[\"x\"] : 0.3167847396421975, [\"y\"] : 0.2767379556904734|} : ({|[\"x\"] : 0.3129857171819161, [\"y\"] : 0.20426921772653447|} : ({|[\"x\"] : 0.29687029792356306, [\"y\"] : 0.3462200657379872|} : ({|[\"x\"] : 0.16239390265026848, [\"y\"] : 0.4128|} : ({|[\"x\"] : 0.2115752867627615, [\"y\"] : 0.5086651868096602|} : [])))))))))), [\"xlabel\"] : \"Renewables/TotalEnergyCap\", [\"ylabel\"] : \"Clean Capacity Factor\"|}, [\"stacked-bar-chart\"] : BarChart {|[\"caption\"] : \"Non-renewables by country\", [\"size\"] : {|[\"height\"] : 185, [\"width\"] : 275|}, [\"stackedBars\"] : ({|[\"bars\"] : ({|[\"y\"] : \"BRA\", [\"z\"] : 151.05|} : ({|[\"y\"] : \"EGY\", [\"z\"] : 159.93|} : ({|[\"y\"] : \"IND\", [\"z\"] : 1060.1799999999998|} : ({|[\"y\"] : \"JPN\", [\"z\"] : 928.82|} : [])))), [\"x\"] : \"2014\"|} : ({|[\"bars\"] : ({|[\"y\"] : \"BRA\", [\"z\"] : 142.76|} : ({|[\"y\"] : \"EGY\", [\"z\"] : 170.68|} : ({|[\"y\"] : \"IND\", [\"z\"] : 1118.8899999999999|} : ({|[\"y\"] : \"JPN\", [\"z\"] : 876.0999999999999|} : [])))), [\"x\"] : \"2015\"|} : ({|[\"bars\"] : ({|[\"y\"] : \"BRA\", [\"z\"] : 108.03|} : ({|[\"y\"] : \"EGY\", [\"z\"] : 174.07999999999998|} : ({|[\"y\"] : \"IND\", [\"z\"] : 1193.53|} : ({|[\"y\"] : \"JPN\", [\"z\"] : 883.3299999999999|} : [])))), [\"x\"] : \"2016\"|} : ({|[\"bars\"] : ({|[\"y\"] : \"BRA\", [\"z\"] : 116.76|} : ({|[\"y\"] : \"EGY\", [\"z\"] : 181.31|} : ({|[\"y\"] : \"IND\", [\"z\"] : ⸨1236.43⸩|} : ({|[\"y\"] : \"JPN\", [\"z\"] : 875.32|} : [])))), [\"x\"] : \"2017\"|} : ({|[\"bars\"] : ({|[\"y\"] : \"BRA\", [\"z\"] : 101.48|} : ({|[\"y\"] : \"EGY\", [\"z\"] : ⸨182.31⸩|} : ({|[\"y\"] : \"IND\", [\"z\"] : 1315.57|} : ({|[\"y\"] : \"JPN\", [\"z\"] : ⸨873.39⸩|} : [])))), [\"x\"] : \"2018\"|} : [])))))|}|}"
     , datasets:
          [ "renewables" ↦ "dataset/renewables-new"
          , "nonRenewables" ↦ "dataset/non-renewables"
          ]
     }
   ]

asTestSuite :: BenchSuite -> TestSuite
asTestSuite suite = second void <$> suite (1 × false)

tests :: TestSuite
tests = concat (benchmarks <#> asTestSuite)
   <> linkedOutputsSuite linkedOutputs_cases
   <> linkedInputsSuite linkedInputs_cases
