module Test.Main where

import Prelude hiding (add)

import App.Util.Select (constr, constrArg, field, matrixElement, listElement, listCell)
import Bindings ((↦))
import Data.Traversable (traverse_)
import DataType (cBarChart, cPair, cSome, f_data, f_y)
import Dict (fromFoldable) as D
import Effect (Effect)
import Lattice (neg)
import Module (File(..))
import Test.Util (Test, run, testWithDatasetMany, testLinkMany, testMany, testBwdMany)
import Util ((×))
import Val (DictRep(..), Val(..))

main :: Effect Unit
main = do
   traverse_ run $ tests false

tests :: Boolean -> Array (Test Unit)
tests is_bench =
   [ test_desugaring is_bench
   , test_misc is_bench
   , test_bwd is_bench
   , test_graphics is_bench
   , test_linking
   ]

{-
tests = [ test_scratchpad ]
-}

test_scratchpad :: Boolean -> Test Unit
test_scratchpad = testBwdMany
   [ { file: "filter"
     , file_expect: "filter.expect"
     , δv: listCell 0 neg
     , fwd_expect: "(_8_ _:_ (7 : []))"
     }
   ]

test_desugaring :: Boolean -> Test Unit
test_desugaring = testMany
   [ { file: "desugar/list-comp-1"
     , fwd_expect: "(14 : (12 : (10 : (13 : (11 : (9 : (12 : (10 : (8 : [])))))))))"
     }
   , { file: "desugar/list-comp-2"
     , fwd_expect:
          "(14 : (14 : (14 : (12 : (12 : (12 : (10 : (10 : (10 : (13 : (13 : (13 : (11 : (11 : (11 : (9 : \
          \(9 : (9 : (12 : (12 : (12 : (10 : (10 : (10 : (8 : (8 : (8 : [])))))))))))))))))))))))))))"
     }
   , { file: "desugar/list-comp-3", fwd_expect: "(9 : (8 : []))" }
   , { file: "desugar/list-comp-4", fwd_expect: "(5 : (4 : (3 : [])))" }
   , { file: "desugar/list-comp-5", fwd_expect: "(5 : (4 : (3 : [])))" }
   , { file: "desugar/list-comp-6", fwd_expect: "(5 : [])" }
   , { file: "desugar/list-comp-7", fwd_expect: "([] : [])" }
   , { file: "desugar/list-enum", fwd_expect: "(3 : (4 : (5 : (6 : (7 : [])))))" }
   ]

test_misc :: Boolean -> Test Unit
test_misc = testMany
   [ { file: "arithmetic", fwd_expect: "42" }
   , { file: "array", fwd_expect: "(1, (3, 3))" }
   , { file: "compose", fwd_expect: "5" }
   , { file: "dicts"
     , fwd_expect:
          "{d: {||}, e: {|\"a\":= 5, \"ab\":= 6|}, e_ab: 6, f: {|\"a\":= 6, \"ab\":= 7|}, g: {|\"a\":= 5|}, h: {|\"fst\":= 4, \"snd\":= (6 : (7 : []))|}}"
     }
   , { file: "div-mod-quot-rem"
     , fwd_expect:
          "((1 : (-1 : (-2 : (2 : [])))) : \
          \((2 : (2 : (1 : (1 : [])))) : \
          \((1 : (-1 : (-1 : (1 : [])))) : \
          \((2 : (2 : (-2 : (-2 : [])))) : []))))"
     }
   , { file: "factorial", fwd_expect: "40320" }
   , { file: "filter", fwd_expect: "(8 : (7 : []))" }
   , { file: "first-class-constr", fwd_expect: "((10 : []) : ((12 : []) : ((20 : []) : [])))" }
   , { file: "flatten"
     , fwd_expect: "((3, \"simon\") : ((4, \"john\") : ((6, \"sarah\") : ((7, \"claire\") : []))))"
     }
   , { file: "foldr_sumSquares", fwd_expect: "661" }
   , { file: "lexicalScoping", fwd_expect: "\"6\"" }
   , { file: "length", fwd_expect: "2" }
   , { file: "lookup", fwd_expect: "Some \"sarah\"" }
   , { file: "map", fwd_expect: "(5 : (7 : (13 : (15 : (4 : (3 : (-3 : [])))))))" }
   , { file: "mergeSort", fwd_expect: "(1 : (2 : (3 : [])))" }
   , { file: "normalise", fwd_expect: "(33, 66)" }
   , { file: "pattern-match", fwd_expect: "4" }
   , { file: "range", fwd_expect: "((0, 0) : ((0, 1) : ((1, 0) : ((1, 1) : []))))" }
   , { file: "records", fwd_expect: "{a: 2, b: 6, c: 7, d: (5 : []), e: 7}" }
   , { file: "reverse", fwd_expect: "(2 : (1 : []))" }
   ]

test_bwd :: Boolean -> Test Unit
test_bwd = testBwdMany
   [ { file: "add", file_expect: "add.expect", δv: neg, fwd_expect: "_8_" }
   , { file: "array/lookup", file_expect: "array/lookup.expect", δv: neg, fwd_expect: "_14_" }
   , { file: "array/dims", file_expect: "array/dims.expect", δv: neg, fwd_expect: "_(_3_, _3_)_" }
   , { file: "convolution/edgeDetect"
     , file_expect: "convolution/edgeDetect.expect"
     , δv: matrixElement 1 1 neg
     , fwd_expect:
          "_0_, -1, 2, 0, -1,\n\
          \0, 3, -2, 3, -2,\n\
          \-1, 1, -5, 0, 4,\n\
          \1, -1, 4, 0, -4,\n\
          \1, 0, -3, 2, 0"
     }
   , { file: "convolution/emboss"
     , file_expect: "convolution/emboss.expect"
     , δv: matrixElement 1 1 neg
     , fwd_expect:
          "_5_, 4, 2, 5, 2,\n\
          \3, 1, 2, -1, -2,\n\
          \3, 0, 1, 0, -1,\n\
          \2, 1, -2, 0, 0,\n\
          \1, 0, -1, -1, -2"
     }
   , { file: "convolution/gaussian"
     , file_expect: "convolution/gaussian.expect"
     , δv: matrixElement 1 1 neg
     , fwd_expect:
          "_38_, 37, 28, 30, 38,\n\
          \38, 36, 46, 31, 34,\n\
          \37, 41, 54, 34, 20,\n\
          \21, 35, 31, 31, 42,\n\
          \13, 32, 35, 19, 26"
     }
   , { file: "dict/create"
     , file_expect: "dict/create.expect"
     , δv: const $ Dictionary false $ DictRep $ D.fromFoldable
          [ "a" ↦ (false × Int false 5)
          , "ab" ↦ (true × Int false 6)
          ]
     , fwd_expect: "{|\"a\":= 5, _\"ab\"_:= 6|}"
     }
   , { file: "dict/difference"
     , file_expect: "dict/difference.expect"
     , δv: const $ Dictionary true $ DictRep $ D.fromFoldable
          [ "a" ↦ (false × Int false 5)
          ]
     , fwd_expect: "_{|\"a\":= 5|}_"
     }
   , { file: "dict/disjointUnion"
     , file_expect: "dict/disjointUnion.expect"
     , δv: const $ Dictionary false $ DictRep $ D.fromFoldable
          [ "a" ↦ (true × Int false 5)
          , "b" ↦ (false × Int false 6)
          , "c" ↦ (false × Int true 7)
          ]
     , fwd_expect:
          "{|_\"a\"_:= 5, \"b\":= 6, \"c\":= _7_|}"
     }
   , { file: "dict/foldl", file_expect: "dict/foldl.expect", δv: neg, fwd_expect: "_0_" }
   , { file: "dict/intersectionWith"
     , file_expect: "dict/intersectionWith.expect"
     , δv: const $ Dictionary false $ DictRep $ D.fromFoldable
          [ "b" ↦ (false × Int true 0)
          , "c" ↦ (false × Int true 20)
          ]
     , fwd_expect:
          "{|\"b\":= _0_, \"c\":= _20_|}"
     }
   , { file: "dict/fromRecord"
     , file_expect: "dict/fromRecord.expect"
     , δv:
          const $ Dictionary false $ DictRep $ D.fromFoldable
             [ "a" ↦ (false × Int false 5)
             , "ab" ↦ (true × Int false 6)
             ]
     , fwd_expect:
          "_{|_\"a\"_:= 5, _\"ab\"_:= 6|}_"
     }
   , { file: "dict/get", file_expect: "dict/get.expect", δv: neg, fwd_expect: "_0_" }
   , { file: "dict/map", file_expect: "dict/map.expect", δv: neg, fwd_expect: "_20_" }
   , { file: "divide", file_expect: "divide.expect", δv: neg, fwd_expect: "_40.22222222222222_" }
   , { file: "filter"
     , file_expect: "filter.expect"
     , δv: listCell 0 neg
     , fwd_expect: "(_8_ _:_ (7 : []))"
     }
   , { file: "intersperse"
     , file_expect: "intersperse-1.expect"
     , δv: listCell 1 neg
     , fwd_expect:
          "(1 : (0 _:_ (2 : (0 : (3 : [])))))"
     }
   , { file: "intersperse"
     , file_expect: "intersperse-2.expect"
     , δv: listCell 2 neg
     , fwd_expect:
          "(1 _:_ (0 : (2 _:_ (0 : (3 : [])))))"
     }
   , { file: "length", file_expect: "length.expect", δv: neg, fwd_expect: "_5_" }
   , { file: "list-comp"
     , file_expect: "list-comp-1.expect"
     , δv: listCell 1 neg
     , fwd_expect: "(6.2 : (260 _:_ (19.9 : (91 : []))))"
     }
   , { file: "list-comp"
     , file_expect: "list-comp-2.expect"
     , δv: listCell 2 neg
     , fwd_expect: "(6.2 : (260 : (19.9 _:_ (91 : []))))"
     }
   , { file: "lookup"
     , file_expect: "lookup.expect"
     , δv: constr cSome
     , fwd_expect: "_Some_ \"Germany\""
     }
   , { file: "map"
     , file_expect: "map.expect"
     , δv: listCell 0 neg >>> listCell 1 neg
     , fwd_expect: "(5 _:_ (6 _:_ []))"
     }
   , { file: "multiply", file_expect: "multiply.expect", δv: neg, fwd_expect: "_0_" }
   , { file: "nth", file_expect: "nth.expect", δv: neg, fwd_expect: "_4_" }
   , { file: "section-5-example"
     , file_expect: "section-5-example-1.expect"
     , δv: listCell 0 neg
     , fwd_expect: "(88 _:_ (6 : (4 : [])))"
     }
   , { file: "section-5-example"
     , file_expect: "section-5-example-2.expect"
     , δv: listElement 1 neg
     , fwd_expect: "(_88_ : (_6_ : (_4_ : [])))"
     }
   , { file: "section-5-example"
     , file_expect: "section-5-example-3.expect"
     , δv: listCell 2 neg
     , fwd_expect: "(88 : (6 : (4 _:_ [])))"
     }
   , { file: "zeros"
     , file_expect: "zeros-1.expect"
     , δv: listCell 0 neg >>> listCell 2 neg
     , fwd_expect: "(0 _:_ (0 : _[]_))"
     }
   , { file: "zeros"
     , file_expect: "zeros-2.expect"
     , δv: listCell 2 neg
     , fwd_expect: "(0 : (0 : _[]_))"
     }
   , { file: "zipWith"
     , file_expect: "zipWith-1.expect"
     , δv: listElement 1 neg
     , fwd_expect: "(13.0 : (_25.0_ : (41.0 : [])))"
     }
   ]

test_graphics :: Boolean -> Test Unit
test_graphics = testWithDatasetMany
   [ { dataset: "dataset/renewables-restricted", file: "graphics/background" }
   , { dataset: "dataset/renewables-restricted", file: "graphics/grouped-bar-chart" }
   , { dataset: "dataset/renewables-restricted", file: "graphics/line-chart" }
   , { dataset: "dataset/renewables-restricted", file: "graphics/stacked-bar-chart" }
   ]

test_linking :: Test Unit
test_linking = testLinkMany
   [ { spec:
          { divId: ""
          , file1: File "pairs-1"
          , file2: File "pairs-2"
          , dataFile: File "pairs-data"
          , x: "data"
          }
     , δv1: constrArg cPair 1
          $ constrArg cPair 1
          $ constrArg cPair 0 neg
     , v2_expect: "(3, (_5_, _7_))"
     }
   , { spec:
          { divId: ""
          , file1: File "convolution-1"
          , file2: File "convolution-2"
          , dataFile: File "convolution-data"
          , x: "data"
          }
     , δv1: matrixElement 2 2 neg
     , v2_expect:
          "_18_, _12_, _13_, 9, 19,\n\
          \_20_, _11_, _24_, 9, 14,\n\
          \_15_, _13_, _20_, 11, 14,\n\
          \7, 15, 15, 8, 20,\n\
          \3, 10, 12, 3, 11"
     }
   , { spec:
          { divId: ""
          , file1: File "bar-chart"
          , file2: File "line-chart"
          , dataFile: File "renewables"
          , x: "data"
          }
     , δv1: constrArg cBarChart 0
          $ field f_data
          $ listElement 1
          $ field f_y neg
     , v2_expect:
          "LineChart ({\
          \caption: \"Output of USA relative to China\", \
          \plots: \
          \(LinePlot ({\
          \data: \
          \({x: 2013, y: 2.5483870967741935} : \
          \({x: 2014, y: 1.61} : \
          \({x: 2015, y: _1.6213592233009706_} : \
          \({x: 2016, y: 1.4000000000000001} : \
          \({x: 2017, y: 1.1208053691275166} : \
          \({x: 2018, y: 0.9101123595505617} : [])))))), \
          \name: \"Bio\"\
          \}) : \
          \(LinePlot ({\
          \data: \
          \({x: 2013, y: 0.3} : \
          \({x: 2014, y: 0.28214285714285714} : \
          \({x: 2015, y: _0.8333333333333334_} : \
          \({x: 2016, y: 0.26229508196721313} : \
          \({x: 2017, y: 0.25559105431309903} : \
          \({x: 2018, y: 0.2484472049689441} : [])))))), \
          \name: \"Hydro\"\
          \}) : \
          \(LinePlot ({\
          \data: \
          \({x: 2013, y: 0.6080402010050252} : \
          \({x: 2014, y: 0.6428571428571429} : \
          \({x: 2015, y: _0.5909090909090909_} : \
          \({x: 2016, y: 0.5324675324675324} : \
          \({x: 2017, y: 0.3893129770992366} : \
          \({x: 2018, y: 0.3522727272727273} : [])))))), \
          \name: \"Solar\"\
          \}) : \
          \(LinePlot ({\
          \data: ({x: 2013, y: 0.6703296703296703} : \
          \({x: 2014, y: 0.5739130434782609} : \
          \({x: 2015, y: _0.5103448275862069_} : \
          \({x: 2016, y: 0.48520710059171596} : \
          \({x: 2017, y: 0.4734042553191489} : \
          \({x: 2018, y: 0.45714285714285713} : [])))))), \
          \name: \"Wind\"\
          \}) : []))))\
          \})"
     }
   ]
