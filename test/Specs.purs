module Test.Specs where

import Prelude

import App (linkedOutputsFig1)
import App.Util.Select (constr, constrArg, dict, dictKey, dictVal, field, listCell, listElement, matrixElement)
import DataType (cBarChart, cPair, cSome, f_data, f_y)
import Lattice (neg)
import Module (File(..))
import Test.Util.Many (TestBwdSpec, TestLinkedOutputsSpec, TestSpec, TestWithDatasetSpec, TestLinkedInputsSpec)

misc_cases :: Array TestSpec
misc_cases =
   [ { file: "arithmetic", fwd_expect: "42" }
   , { file: "array", fwd_expect: "(1, (3, 3))" }
   , { file: "compose", fwd_expect: "5" }
   , { file: "dicts"
     , fwd_expect:
          "{d : {||}, e : {|\"a\" := 5, \"ab\" := 6|}, e_ab : 6, f : {|\"a\" := 6, \"ab\" := 7|}, g : {|\"a\" := 5|}, h : {|\"fst\" := 4, \"snd\" := (6 : (7 : []))|}}"
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
   , { file: "foldr-sumSquares", fwd_expect: "661" }
   , { file: "lexicalScoping", fwd_expect: "\"6\"" }
   , { file: "length", fwd_expect: "2" }
   , { file: "lookup", fwd_expect: "Some \"sarah\"" }
   , { file: "map", fwd_expect: "(5 : (7 : (13 : (15 : (4 : (3 : (-3 : [])))))))" }
   , { file: "mergeSort", fwd_expect: "(1 : (2 : (3 : [])))" }
   , { file: "normalise", fwd_expect: "(33, 66)" }
   , { file: "pattern-match", fwd_expect: "4" }
   , { file: "range", fwd_expect: "((0, 0) : ((0, 1) : ((1, 0) : ((1, 1) : []))))" }
   , { file: "records", fwd_expect: "{a : 2, b : 6, c : 7, d : (5 : []), e : 7}" }
   , { file: "record-lookup", fwd_expect: "True" }
   , { file: "reverse", fwd_expect: "(2 : (1 : []))" }
   ]

desugar_cases :: Array TestSpec
desugar_cases =
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

bwd_cases :: Array TestBwdSpec
bwd_cases =
   [ { file: "add", bwd_expect_file: "add.expect", δv: neg, fwd_expect: "⸨8⸩" }
   , { file: "array/lookup", bwd_expect_file: "array/lookup.expect", δv: neg, fwd_expect: "⸨14⸩" }
   , { file: "array/dims", bwd_expect_file: "array/dims.expect", δv: neg, fwd_expect: "⸨(⸨3⸩, ⸨3⸩)⸩" }
   , { file: "convolution/edgeDetect"
     , bwd_expect_file: "convolution/edgeDetect.expect"
     , δv: matrixElement 1 1 neg
     , fwd_expect:
          "⸨0⸩, -1, 2, 0, -1,\n\
          \0, 3, -2, 3, -2,\n\
          \-1, 1, -5, 0, 4,\n\
          \1, -1, 4, 0, -4,\n\
          \1, 0, -3, 2, 0"
     }
   , { file: "convolution/emboss"
     , bwd_expect_file: "convolution/emboss.expect"
     , δv: matrixElement 1 1 neg
     , fwd_expect:
          "⸨5⸩, 4, 2, 5, 2,\n\
          \3, 1, 2, -1, -2,\n\
          \3, 0, 1, 0, -1,\n\
          \2, 1, -2, 0, 0,\n\
          \1, 0, -1, -1, -2"
     }
   , { file: "convolution/gaussian"
     , bwd_expect_file: "convolution/gaussian.expect"
     , δv: matrixElement 1 1 neg
     , fwd_expect:
          "⸨38⸩, 37, 28, 30, 38,\n\
          \38, 36, 46, 31, 34,\n\
          \37, 41, 54, 34, 20,\n\
          \21, 35, 31, 31, 42,\n\
          \13, 32, 35, 19, 26"
     }
   , { file: "dict/create"
     , bwd_expect_file: "dict/create.expect"
     , δv: dictKey "ab" neg
     , fwd_expect: "{|\"a\" := 5, ⸨\"ab\"⸩ := 6|}"
     }
   , { file: "dict/difference"
     , bwd_expect_file: "dict/difference.expect"
     , δv: dict neg
     , fwd_expect: "⸨{|\"a\" := 5|}⸩"
     }
   , { file: "dict/disjointUnion"
     , bwd_expect_file: "dict/disjointUnion.expect"
     , δv: dictKey "a" neg >>> dictVal "c" neg
     , fwd_expect: "{|⸨\"a\"⸩ := 5, \"b\" := 6, \"c\" := ⸨7⸩|}"
     }
   , { file: "dict/foldl", bwd_expect_file: "dict/foldl.expect", δv: neg, fwd_expect: "⸨0⸩" }
   , { file: "dict/intersectionWith"
     , bwd_expect_file: "dict/intersectionWith.expect"
     , δv: dictVal "b" neg >>> dictVal "c" neg
     , fwd_expect: "{|\"b\" := ⸨0⸩, \"c\" := ⸨20⸩|}"
     }
   , { file: "dict/fromRecord"
     , bwd_expect_file: "dict/fromRecord.expect"
     , δv: dictKey "ab" neg
     , fwd_expect: "⸨{|⸨\"a\"⸩ := 5, ⸨\"ab\"⸩ := 6|}⸩"
     }
   , { file: "dict/get", bwd_expect_file: "dict/get.expect", δv: neg, fwd_expect: "⸨0⸩" }
   , { file: "dict/map", bwd_expect_file: "dict/map.expect", δv: neg, fwd_expect: "⸨20⸩" }
   , { file: "divide", bwd_expect_file: "divide.expect", δv: neg, fwd_expect: "⸨40.22222222222222⸩" }
   , { file: "dtw/compute-dtw"
     , bwd_expect_file: "dtw/compute-dtw.expect"
     , fwd_expect: "((1, 1) : (⸨(⸨2⸩, ⸨2⸩)⸩ : ((2, 3) : ((3, 4) : ((4, 5) : ((5, 6) : ((5, 7) : [])))))))"
     , δv: listElement 1 neg
     }
   , { file: "dtw/average-series"
     , bwd_expect_file: "dtw/average-series.expect"
     , fwd_expect: "(2.5 : (0.5 : (⸨0.5⸩ : (2.5 : (2.5 : (1.0 : (0.5 : [])))))))"
     , δv: listElement 2 neg
     }
   , { file: "filter"
     , bwd_expect_file: "filter.expect"
     , δv: listCell 0 neg
     , fwd_expect: "(⸨8⸩ ⸨:⸩ (7 : []))"
     }
   , { file: "intersperse"
     , bwd_expect_file: "intersperse-1.expect"
     , δv: listCell 1 neg
     , fwd_expect: "(1 : (0 ⸨:⸩ (2 : (0 : (3 : [])))))"
     }
   , { file: "intersperse"
     , bwd_expect_file: "intersperse-2.expect"
     , δv: listCell 2 neg
     , fwd_expect: "(1 ⸨:⸩ (0 : (2 ⸨:⸩ (0 : (3 : [])))))"
     }
   , { file: "length", bwd_expect_file: "length.expect", δv: neg, fwd_expect: "⸨5⸩" }
   , { file: "list-comp"
     , bwd_expect_file: "list-comp-1.expect"
     , δv: listCell 1 neg
     , fwd_expect: "(6.2 : (260 ⸨:⸩ (19.9 : (91 : []))))"
     }
   , { file: "list-comp"
     , bwd_expect_file: "list-comp-2.expect"
     , δv: listCell 2 neg
     , fwd_expect: "(6.2 : (260 : (19.9 ⸨:⸩ (91 : []))))"
     }
   , { file: "lookup"
     , bwd_expect_file: "lookup.expect"
     , δv: constr cSome neg
     , fwd_expect: "⸨Some \"Germany\"⸩"
     }
   , { file: "map"
     , bwd_expect_file: "map.expect"
     , δv: listCell 0 neg >>> listCell 1 neg
     , fwd_expect: "(5 ⸨:⸩ (6 ⸨:⸩ []))"
     }
   , { file: "matrix-update"
     , bwd_expect_file: "matrix-update.expect"
     , fwd_expect:
          "15, 13, 6, 9, 16,\n\
          \12, ⸨4000⸩, 15, 4, 13,\n\
          \14, 9, 20, 8, 1,\n\
          \4, 10, 3, 7, 19,\n\
          \3, 11, 15, 2, 9"
     , δv: matrixElement 2 2 neg
     }
   , { file: "multiply", bwd_expect_file: "multiply.expect", δv: neg, fwd_expect: "⸨0⸩" }
   , { file: "nth", bwd_expect_file: "nth.expect", δv: neg, fwd_expect: "⸨4⸩" }
   , { file: "section-5-example"
     , bwd_expect_file: "section-5-example-1.expect"
     , δv: listCell 0 neg
     , fwd_expect: "(88 ⸨:⸩ (6 : (4 : [])))"
     }
   , { file: "section-5-example"
     , bwd_expect_file: "section-5-example-2.expect"
     , δv: listElement 1 neg
     , fwd_expect: "(⸨88⸩ : (⸨6⸩ : (⸨4⸩ : [])))"
     }
   , { file: "section-5-example"
     , bwd_expect_file: "section-5-example-3.expect"
     , δv: listCell 2 neg
     , fwd_expect: "(88 : (6 : (4 ⸨:⸩ [])))"
     }
   , { file: "zeros"
     , bwd_expect_file: "zeros-1.expect"
     , δv: listCell 0 neg >>> listCell 2 neg
     , fwd_expect: "(0 ⸨:⸩ (0 : ⸨[]⸩))"
     }
   , { file: "zeros"
     , bwd_expect_file: "zeros-2.expect"
     , δv: listCell 2 neg
     , fwd_expect: "(0 : (0 : ⸨[]⸩))"
     }
   , { file: "zipWith"
     , bwd_expect_file: "zipWith-1.expect"
     , δv: listElement 1 neg
     , fwd_expect: "(13.0 : (⸨25.0⸩ : (41.0 : [])))"
     }
   ]

graphics_cases :: Array TestWithDatasetSpec
graphics_cases =
   [ { dataset: "dataset/renewables-restricted", file: "graphics/background" }
   , { dataset: "dataset/renewables-restricted", file: "graphics/grouped-bar-chart" }
   , { dataset: "dataset/renewables-restricted", file: "graphics/line-chart" }
   , { dataset: "dataset/renewables-restricted", file: "graphics/stacked-bar-chart" }
   ]

linkedOutputs_cases :: Array TestLinkedOutputsSpec
linkedOutputs_cases =
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
     , v2_expect: "(3, (⸨5⸩, ⸨7⸩))"
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
          "⸨18⸩, ⸨12⸩, ⸨13⸩, 9, 19,\n\
          \⸨20⸩, ⸨11⸩, ⸨24⸩, 9, 14,\n\
          \⸨15⸩, ⸨13⸩, ⸨20⸩, 11, 14,\n\
          \7, 15, 15, 8, 20,\n\
          \3, 10, 12, 3, 11"
     }
   , { spec: linkedOutputsFig1
     , δv1: constrArg cBarChart 0
          $ field f_data
          $ listElement 1
          $ field f_y neg
     , v2_expect:
          "LineChart {\
          \caption : \"Output of USA relative to China\", \
          \plots : \
          \(LinePlot {\
          \data : \
          \({x : 2013, y : 2.5483870967741935} : \
          \({x : 2014, y : 1.61} : \
          \({x : 2015, y : ⸨1.6213592233009706⸩} : \
          \({x : 2016, y : 1.4000000000000001} : \
          \({x : 2017, y : 1.1208053691275166} : \
          \({x : 2018, y : 0.9101123595505617} : [])))))), \
          \name : \"Bio\"\
          \} : \
          \(LinePlot {\
          \data : \
          \({x : 2013, y : 0.3} : \
          \({x : 2014, y : 0.28214285714285714} : \
          \({x : 2015, y : ⸨0.8333333333333334⸩} : \
          \({x : 2016, y : 0.26229508196721313} : \
          \({x : 2017, y : 0.25559105431309903} : \
          \({x : 2018, y : 0.2484472049689441} : [])))))), \
          \name : \"Hydro\"\
          \} : \
          \(LinePlot {\
          \data : \
          \({x : 2013, y : 0.6080402010050252} : \
          \({x : 2014, y : 0.6428571428571429} : \
          \({x : 2015, y : ⸨0.5909090909090909⸩} : \
          \({x : 2016, y : 0.5324675324675324} : \
          \({x : 2017, y : 0.3893129770992366} : \
          \({x : 2018, y : 0.3522727272727273} : [])))))), \
          \name : \"Solar\"\
          \} : \
          \(LinePlot {\
          \data : ({x : 2013, y : 0.6703296703296703} : \
          \({x : 2014, y : 0.5739130434782609} : \
          \({x : 2015, y : ⸨0.5103448275862069⸩} : \
          \({x : 2016, y : 0.48520710059171596} : \
          \({x : 2017, y : 0.4734042553191489} : \
          \({x : 2018, y : 0.45714285714285713} : [])))))), \
          \name : \"Wind\"\
          \} : []))))\
          \}"
     }
   ]

linkedInputs_cases :: Array TestLinkedInputsSpec
linkedInputs_cases =
   [ { spec:
          { divId: ""
          , file: File "water"
          , x1: "all_countries"
          , x2: "all_cities"
          }
     , δv1: listElement 0 neg
     , v2_expect: "({name : \"Berlin\", water : 130} : ({name : \"Munich\", water : 80} : ({name : \"Hamburg\", water : ⸨60⸩} : ({name : \"London\", water : ⸨200⸩} : ({name : \"Birmingham\", water : ⸨50⸩} : ({name : \"Manchester\", water : ⸨35⸩} : []))))))"
     }
   ]
