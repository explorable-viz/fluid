module Test.Main
   ( main
   , test_bwd
   , test_desugaring
   , test_graphics
   --  , test_linking
   , test_misc
   --  , test_scratchpad
   ) where

import Prelude hiding (add)

-- import App.Util (as𝔹Selector, selectNthCell)
import Data.Array (concat)
import Effect (Effect)
import Effect.Aff (Aff)
import Test.Spec.Specs (bwd_cases, desugar_cases, graphics_cases, misc_cases)
import Test.TestRunners (run, testWithDatasetMany, testMany, testBwdMany)
import Util (type (×))

main :: Effect Unit
main = do
   run tests

tests :: Array (String × Aff Unit)
tests = concat
   [ test_desugaring
   , test_misc
   , test_bwd
   , test_graphics
   --  , test_linking
   ]

{-
tests = [ test_scratchpad ]
-}

-- test_scratchpad :: Boolean -> Test Unit
-- test_scratchpad = testBwdMany
--    [ { file: "filter"
--      , file_expect: "filter.expect"
--      , δv: selectNthCell 0 # as𝔹Selector
--      , fwd_expect: "(_8_ _:_ (7 : []))"
--      }
--    ]

test_desugaring :: Array (String × Aff Unit)
test_desugaring = testMany desugar_cases

test_misc :: Array (String × Aff Unit)
test_misc = testMany misc_cases

test_bwd :: Array (String × Aff Unit)
test_bwd = testBwdMany bwd_cases

test_graphics :: Array (String × Aff Unit)
test_graphics = testWithDatasetMany graphics_cases

-- test_linking :: Test Unit
-- test_linking = testLinkMany
--    [ { divId: ""
--      , file1: File "pairs-1"
--      , file2: File "pairs-2"
--      , dataFile: File "pairs-data"
--      , x: "data"
--      }
--         ×
--            ( selectPair (const false) botOf
--                 ( selectPair (const false) botOf
--                      (selectPair (const false) (const $ Int true 3) botOf)
--                 )
--            )
--         × "(3, (_5_, _7_))"
--    , { divId: ""
--      , file1: File "convolution-1"
--      , file2: File "convolution-2"
--      , dataFile: File "convolution-data"
--      , x: "data"
--      }
--         × (selectMatrixElement 2 2 # as𝔹Selector)
--         ×
--            "_18_, _12_, _13_, 9, 19,\n\
--            \_20_, _11_, _24_, 9, 14,\n\
--            \_15_, _13_, _20_, 11, 14,\n\
--            \7, 15, 15, 8, 20,\n\
--            \3, 10, 12, 3, 11"
--    , { divId: ""
--      , file1: File "bar-chart"
--      , file2: File "line-chart"
--      , dataFile: File "renewables"
--      , x: "data"
--      }
--         × (botOf >>> selectBarChart_data (selectNth 1 (select_y topOf)))
--         ×
--            "LineChart ({\
--            \caption: \"Output of USA relative to China\", \
--            \plots: \
--            \(LinePlot ({\
--            \data: \
--            \({x: 2013, y: 2.5483870967741935} : \
--            \({x: 2014, y: 1.61} : \
--            \({x: 2015, y: _1.6213592233009706_} : \
--            \({x: 2016, y: 1.4000000000000001} : \
--            \({x: 2017, y: 1.1208053691275166} : \
--            \({x: 2018, y: 0.9101123595505617} : [])))))), \
--            \name: \"Bio\"\
--            \}) : \
--            \(LinePlot ({\
--            \data: \
--            \({x: 2013, y: 0.3} : \
--            \({x: 2014, y: 0.28214285714285714} : \
--            \({x: 2015, y: _0.8333333333333334_} : \
--            \({x: 2016, y: 0.26229508196721313} : \
--            \({x: 2017, y: 0.25559105431309903} : \
--            \({x: 2018, y: 0.2484472049689441} : [])))))), \
--            \name: \"Hydro\"\
--            \}) : \
--            \(LinePlot ({\
--            \data: \
--            \({x: 2013, y: 0.6080402010050252} : \
--            \({x: 2014, y: 0.6428571428571429} : \
--            \({x: 2015, y: _0.5909090909090909_} : \
--            \({x: 2016, y: 0.5324675324675324} : \
--            \({x: 2017, y: 0.3893129770992366} : \
--            \({x: 2018, y: 0.3522727272727273} : [])))))), \
--            \name: \"Solar\"\
--            \}) : \
--            \(LinePlot ({\
--            \data: ({x: 2013, y: 0.6703296703296703} : \
--            \({x: 2014, y: 0.5739130434782609} : \
--            \({x: 2015, y: _0.5103448275862069_} : \
--            \({x: 2016, y: 0.48520710059171596} : \
--            \({x: 2017, y: 0.4734042553191489} : \
--            \({x: 2018, y: 0.45714285714285713} : [])))))), \
--            \name: \"Wind\"\
--            \}) : []))))\
--            \})"
--    ]
