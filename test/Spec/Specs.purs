module Test.Spec.Specs
   ( bwd_cases
   , desugar_cases
   , graphics_cases
   , misc_cases
   ) where

import Prelude

import App.Util (as𝔹Selector, selectMatrixElement, selectNth, selectNthCell, selectSome)
import Bindings ((↦))
import Dict (fromFoldable) as D
import Lattice (botOf, topOf)
import Test.Util (TestBwdSpec, TestSpec, TestWithDatasetSpec)
import Util ((×))
import Val (DictRep(..), Val(..))

misc_cases :: Array TestSpec
misc_cases =
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
   [ { file: "add", file_expect: "add.expect", δv: const $ Int true 8, fwd_expect: "_8_" }
   , { file: "array/lookup", file_expect: "array/lookup.expect", δv: const $ Int true 14, fwd_expect: "_14_" }
   , { file: "array/dims", file_expect: "array/dims.expect", δv: topOf, fwd_expect: "_(_3_, _3_)_" }
   , { file: "convolution/edgeDetect"
     , file_expect: "convolution/edgeDetect.expect"
     , δv: selectMatrixElement 1 1 # as𝔹Selector
     , fwd_expect:
          "_0_, -1, 2, 0, -1,\n\
          \0, 3, -2, 3, -2,\n\
          \-1, 1, -5, 0, 4,\n\
          \1, -1, 4, 0, -4,\n\
          \1, 0, -3, 2, 0"
     }
   , { file: "convolution/emboss"
     , file_expect: "convolution/emboss.expect"
     , δv: selectMatrixElement 1 1 # as𝔹Selector
     , fwd_expect:
          "_5_, 4, 2, 5, 2,\n\
          \3, 1, 2, -1, -2,\n\
          \3, 0, 1, 0, -1,\n\
          \2, 1, -2, 0, 0,\n\
          \1, 0, -1, -1, -2"
     }
   , { file: "convolution/gaussian"
     , file_expect: "convolution/gaussian.expect"
     , δv: selectMatrixElement 1 1 # as𝔹Selector
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
   , { file: "dict/foldl", file_expect: "dict/foldl.expect", δv: topOf, fwd_expect: "_0_" }
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
   , { file: "dict/get", file_expect: "dict/get.expect", δv: const $ Int true 0, fwd_expect: "_0_" }
   , { file: "dict/map", file_expect: "dict/map.expect", δv: const $ Int true 20, fwd_expect: "_20_" }
   , { file: "divide", file_expect: "divide.expect", δv: topOf, fwd_expect: "_40.22222222222222_" }
   , { file: "filter"
     , file_expect: "filter.expect"
     , δv: selectNthCell 0 # as𝔹Selector
     , fwd_expect: "(_8_ _:_ (7 : []))"
     }
   , { file: "intersperse"
     , file_expect: "intersperse-1.expect"
     , δv: selectNthCell 1 # as𝔹Selector
     , fwd_expect:
          "(1 : (0 _:_ (2 : (0 : (3 : [])))))"
     }
   , { file: "intersperse"
     , file_expect: "intersperse-2.expect"
     , δv: selectNthCell 2 # as𝔹Selector
     , fwd_expect:
          "(1 _:_ (0 : (2 _:_ (0 : (3 : [])))))"
     }
   , { file: "length", file_expect: "length.expect", δv: topOf, fwd_expect: "_5_" }
   , { file: "list-comp"
     , file_expect: "list-comp-1.expect"
     , δv: selectNthCell 1 # as𝔹Selector
     , fwd_expect: "(6.2 : (260 _:_ (19.9 : (91 : []))))"
     }
   , { file: "list-comp"
     , file_expect: "list-comp-2.expect"
     , δv: selectNthCell 2 # as𝔹Selector
     , fwd_expect: "(6.2 : (260 : (19.9 _:_ (91 : []))))"
     }
   , { file: "lookup"
     , file_expect: "lookup.expect"
     , δv: selectSome # as𝔹Selector
     , fwd_expect: "_Some_ \"Germany\""
     }
   , { file: "map"
     , file_expect: "map.expect"
     , δv: selectNthCell 0 <> selectNthCell 1 # as𝔹Selector
     , fwd_expect: "(5 _:_ (6 _:_ []))"
     }
   , { file: "multiply", file_expect: "multiply.expect", δv: const $ Int true 0, fwd_expect: "_0_" }
   , { file: "nth", file_expect: "nth.expect", δv: const $ Int true 4, fwd_expect: "_4_" }
   , { file: "section-5-example"
     , file_expect: "section-5-example-1.expect"
     , δv: selectNthCell 0 # as𝔹Selector
     , fwd_expect: "(88 _:_ (6 : (4 : [])))"
     }
   , { file: "section-5-example"
     , file_expect: "section-5-example-2.expect"
     , δv: botOf >>> selectNth 1 topOf
     , fwd_expect: "(_88_ : (_6_ : (_4_ : [])))"
     }
   , { file: "section-5-example"
     , file_expect: "section-5-example-3.expect"
     , δv: selectNthCell 2 # as𝔹Selector
     , fwd_expect: "(88 : (6 : (4 _:_ [])))"
     }
   , { file: "zeros"
     , file_expect: "zeros-1.expect"
     , δv: selectNthCell 0 <> selectNthCell 2 # as𝔹Selector
     , fwd_expect: "(0 _:_ (0 : _[]_))"
     }
   , { file: "zeros"
     , file_expect: "zeros-2.expect"
     , δv: selectNthCell 2 # as𝔹Selector
     , fwd_expect: "(0 : (0 : _[]_))"
     }
   , { file: "zipWith"
     , file_expect: "zipWith-1.expect"
     , δv: botOf >>> selectNth 1 (const $ Float true 25.0)
     , fwd_expect: "(13.0 : (_25.0_ : (41.0 : [])))"
     }
   ]

graphics_cases :: Array TestWithDatasetSpec
graphics_cases =
   [ { dataset: "dataset/renewables-restricted", file: "graphics/background" }
   , { dataset: "dataset/renewables-restricted", file: "graphics/grouped-bar-chart" }
   , { dataset: "dataset/renewables-restricted", file: "graphics/line-chart" }
   , { dataset: "dataset/renewables-restricted", file: "graphics/stacked-bar-chart" }
   ]