module Test.Specs.Bwd where

import Prelude

import App.Util.Selector (dict, dictKey, dictVal, listCell, listElement, matrixElement, snd, some)
import Lattice (neg)
import Test.Util.Suite (TestBwdSpec)

bwd_cases :: Array TestBwdSpec
bwd_cases =
   [ { file: "add", imports: [], bwd_expect_file: "add.expect", δv: neg, fwd_expect: "⸨8⸩" }
   , { file: "array/lookup", imports: [], bwd_expect_file: "array/lookup.expect", δv: neg, fwd_expect: "⸨14⸩" }
   , { file: "array/dims", imports: [], bwd_expect_file: "array/dims.expect", δv: neg, fwd_expect: "⸨(⸨3⸩, ⸨3⸩)⸩" }
   , { file: "convolution/edgeDetect"
     , imports:
          [ "lib/convolution"
          , "example/slicing/convolution/filter/edge-detect"
          , "example/slicing/convolution/test-image"
          ]
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
     , imports:
          [ "lib/convolution"
          , "example/slicing/convolution/filter/emboss"
          , "example/slicing/convolution/test-image"
          ]
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
     , imports:
          [ "lib/convolution"
          , "example/slicing/convolution/filter/gaussian"
          , "example/slicing/convolution/test-image"
          ]
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
     , imports: []
     , bwd_expect_file: "dict/create.expect"
     , δv: dictKey "ab" neg
     , fwd_expect: "{|\"a\" := 5, ⸨\"ab\"⸩ := 6|}"
     }
   , { file: "dict/difference"
     , imports: []
     , bwd_expect_file: "dict/difference.expect"
     , δv: dict neg
     , fwd_expect: "⸨{|\"a\" := 5|}⸩"
     }
   , { file: "dict/disjointUnion"
     , imports: []
     , bwd_expect_file: "dict/disjointUnion.expect"
     , δv: dictKey "a" neg >>> dictVal "c" neg
     , fwd_expect: "{|⸨\"a\"⸩ := 5, \"b\" := 6, \"c\" := ⸨7⸩|}"
     }
   , { file: "dict/foldl", imports: [], bwd_expect_file: "dict/foldl.expect", δv: neg, fwd_expect: "⸨0⸩" }
   , { file: "dict/intersectionWith"
     , imports: []
     , bwd_expect_file: "dict/intersectionWith.expect"
     , δv: dictVal "b" neg >>> dictVal "c" neg
     , fwd_expect: "{|\"b\" := ⸨0⸩, \"c\" := ⸨20⸩|}"
     }
   , { file: "dict/fromRecord"
     , imports: []
     , bwd_expect_file: "dict/fromRecord.expect"
     , δv: dictKey "ab" neg
     , fwd_expect: "⸨{|⸨\"a\"⸩ := 5, ⸨\"ab\"⸩ := 6|}⸩"
     }
   , { file: "dict/get", imports: [], bwd_expect_file: "dict/get.expect", δv: neg, fwd_expect: "⸨0⸩" }
   , { file: "dict/map", imports: [], bwd_expect_file: "dict/map.expect", δv: neg, fwd_expect: "⸨20⸩" }
   , { file: "divide"
     , imports: []
     , bwd_expect_file: "divide.expect"
     , δv: neg
     , fwd_expect: "⸨40.22222222222222⸩"
     }
   , { file: "dtw/compute-dtw"
     , imports:
          [ "lib/fnum"
          , "lib/dtw"
          ]
     , bwd_expect_file: "dtw/compute-dtw.expect"
     , fwd_expect: "((1, 1) : (⸨(⸨2⸩, ⸨2⸩)⸩ : ((2, 3) : ((3, 4) : ((4, 5) : ((5, 6) : ((5, 7) : [])))))))"
     , δv: listElement 1 neg
     }
   , { file: "dtw/average-series"
     , imports:
          [ "lib/fnum"
          , "lib/dtw"
          ]
     , bwd_expect_file: "dtw/average-series.expect"
     , fwd_expect: "(2.5 : (0.5 : (⸨0.5⸩ : (2.5 : (2.5 : (1.0 : (0.5 : [])))))))"
     , δv: listElement 2 neg
     }
   , { file: "filter"
     , imports: []
     , bwd_expect_file: "filter.expect"
     , δv: listCell 0 neg
     , fwd_expect: "⸨(⸨8⸩ : (7 : []))⸩"
     }
   , { file: "intersperse"
     , imports: []
     , bwd_expect_file: "intersperse-1.expect"
     , δv: listCell 1 neg
     , fwd_expect: "(1 : ⸨(0 : (2 : (0 : (3 : []))))⸩)"
     }
   , { file: "intersperse"
     , imports: []
     , bwd_expect_file: "intersperse-2.expect"
     , δv: listCell 2 neg
     , fwd_expect: "⸨(1 : (0 : ⸨(2 : (0 : (3 : [])))⸩))⸩"
     }
   , { file: "length", imports: [], bwd_expect_file: "length.expect", δv: neg, fwd_expect: "⸨5⸩" }
   , { file: "list-comp"
     , imports: []
     , bwd_expect_file: "list-comp-1.expect"
     , δv: listCell 1 neg
     , fwd_expect: "(6.2 : ⸨(260 : (19.9 : (91 : [])))⸩)"
     }
   , { file: "list-comp"
     , imports: []
     , bwd_expect_file: "list-comp-2.expect"
     , δv: listCell 2 neg
     , fwd_expect: "(6.2 : (260 : ⸨(19.9 : (91 : []))⸩))"
     }
   , { file: "lookup"
     , imports: []
     , bwd_expect_file: "lookup.expect"
     , δv: some neg
     , fwd_expect: "⸨Some \"Germany\"⸩"
     }
   , { file: "map"
     , imports: []
     , bwd_expect_file: "map.expect"
     , δv: listCell 0 neg >>> listCell 1 neg
     , fwd_expect: "⸨(5 : ⸨(6 : [])⸩)⸩"
     }
   , { file: "matrix-update"
     , imports: []
     , bwd_expect_file: "matrix-update.expect"
     , fwd_expect:
          "15, 13, 6, 9, 16,\n\
          \12, ⸨4000⸩, 15, 4, 13,\n\
          \14, 9, 20, 8, 1,\n\
          \4, 10, 3, 7, 19,\n\
          \3, 11, 15, 2, 9"
     , δv: matrixElement 2 2 neg
     }
   , { file: "multiply", imports: [], bwd_expect_file: "multiply.expect", δv: neg, fwd_expect: "⸨0⸩" }
   , { file: "nth", imports: [], bwd_expect_file: "nth.expect", δv: neg, fwd_expect: "⸨4⸩" }
   , { file: "output-not-source"
     , imports: []
     , bwd_expect_file: "output-not-source.expect"
     , fwd_expect: "(⸨3⸩, ⸨True⸩)"
     , δv: snd neg -- selection on just first component will be discarded by bwdSlice; see #818.
     }
   , { file: "section-5-example"
     , imports: []
     , bwd_expect_file: "section-5-example-1.expect"
     , δv: listCell 0 neg
     , fwd_expect: "⸨(88 : (6 : (4 : [])))⸩"
     }
   , { file: "section-5-example"
     , imports: []
     , bwd_expect_file: "section-5-example-2.expect"
     , δv: listElement 1 neg
     , fwd_expect: "(⸨88⸩ : (⸨6⸩ : (⸨4⸩ : [])))"
     }
   , { file: "section-5-example"
     , imports: []
     , bwd_expect_file: "section-5-example-3.expect"
     , δv: listCell 2 neg
     , fwd_expect: "(88 : (6 : ⸨(4 : [])⸩))"
     }
   , { file: "zeros"
     , imports: []
     , bwd_expect_file: "zeros-1.expect"
     , δv: listCell 0 neg >>> listCell 2 neg
     , fwd_expect: "⸨(0 : (0 : ⸨[]⸩))⸩"
     }
   , { file: "zeros"
     , imports: []
     , bwd_expect_file: "zeros-2.expect"
     , δv: listCell 2 neg
     , fwd_expect: "(0 : (0 : ⸨[]⸩))"
     }
   , { file: "zipWith"
     , imports: []
     , bwd_expect_file: "zipWith-1.expect"
     , δv: listElement 1 neg
     , fwd_expect: "(13.0 : (⸨25.0⸩ : (41.0 : [])))"
     }
   ]
