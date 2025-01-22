module Test.Specs.Misc where

import Test.Util.Suite (TestSpec)

misc_cases :: Array TestSpec
misc_cases =
   [ { file: "example/arithmetic", imports: [], fwd_expect: "42" }
   , { file: "example/array", imports: [], fwd_expect: "(1, (3, 3))" }
   , { file: "example/compose", imports: [], fwd_expect: "5" }
   , { file: "example/dicts"
     , imports: []
     , fwd_expect:
          "{[\"d\"] : {}, [\"e\"] : {[\"a\"] : 5, [\"ab\"] : 6}, [\"e_ab\"] : 6, [\"f\"] : {[\"a\"] : 6, [\"ab\"] : 7}, [\"g\"] : {[\"a\"] : 5}}"
     }
   , { file: "example/div-mod-quot-rem"
     , imports: []
     , fwd_expect:
          "((1 : (-1 : (-2 : (2 : [])))) : \
          \((2 : (2 : (1 : (1 : [])))) : \
          \((1 : (-1 : (-1 : (1 : [])))) : \
          \((2 : (2 : (-2 : (-2 : [])))) : []))))"
     }
   , { file: "example/factorial", imports: [], fwd_expect: "40320" }
   , { file: "example/filter", imports: [], fwd_expect: "(8 : (7 : []))" }
   , { file: "example/first-class-constr", imports: [], fwd_expect: "((10 : []) : ((12 : []) : ((20 : []) : [])))" }
   , { file: "example/flatten"
     , imports: []
     , fwd_expect: "((3, \"simon\") : ((4, \"john\") : ((6, \"sarah\") : ((7, \"claire\") : []))))"
     }
   , { file: "example/foldr-sumSquares", imports: [], fwd_expect: "661" }
   , { file: "example/include-input-into-output"
     , imports: [ "example/lib/some-constants" ]
     , fwd_expect: "(1, 1)"
     }
   , { file: "example/lexicalScoping", imports: [], fwd_expect: "\"6\"" }
   , { file: "example/length", imports: [], fwd_expect: "2" }
   , { file: "example/lookup", imports: [], fwd_expect: "Some \"sarah\"" }
   , { file: "example/map", imports: [], fwd_expect: "(5 : (7 : (13 : (15 : (4 : (3 : (-3 : [])))))))" }
   , { file: "example/mergeSort", imports: [], fwd_expect: "(1 : (2 : (3 : [])))" }
   , { file: "example/normalise", imports: [], fwd_expect: "(33, 66)" }
   , { file: "example/nub", imports: [], fwd_expect: "(1 : (2 : (3 : (4 : []))))" }
   , { file: "example/pattern-match", imports: [], fwd_expect: "4" }
   , { file: "example/range", imports: [], fwd_expect: "((0, 0) : ((0, 1) : ((1, 0) : ((1, 1) : []))))" }
   , { file: "example/records", imports: [], fwd_expect: "{[\"a\"] : 2, [\"b\"] : 6, [\"c\"] : 7, [\"d\"] : (5 : []), [\"e\"] : 7}" }
   , { file: "example/record-lookup", imports: [], fwd_expect: "True" }
   , { file: "example/reverse", imports: [], fwd_expect: "(2 : (1 : []))" }
   ]
