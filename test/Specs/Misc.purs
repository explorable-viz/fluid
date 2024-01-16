module Test.Specs.Misc where

import Test.Util.Suite (TestSpec)

misc_cases :: Array TestSpec
misc_cases =
   [ { file: "arithmetic", imports: [], fwd_expect: "42" }
   , { file: "array", imports: [], fwd_expect: "(1, (3, 3))" }
   , { file: "compose", imports: [], fwd_expect: "5" }
   , { file: "dicts"
     , imports: []
     , fwd_expect:
          "{d : {||}, e : {|\"a\" := 5, \"ab\" := 6|}, e_ab : 6, f : {|\"a\" := 6, \"ab\" := 7|}, g : {|\"a\" := 5|}, h : {|\"fst\" := 4, \"snd\" := (6 : (7 : []))|}}"
     }
   , { file: "div-mod-quot-rem"
     , imports: []
     , fwd_expect:
          "((1 : (-1 : (-2 : (2 : [])))) : \
          \((2 : (2 : (1 : (1 : [])))) : \
          \((1 : (-1 : (-1 : (1 : [])))) : \
          \((2 : (2 : (-2 : (-2 : [])))) : []))))"
     }
   , { file: "factorial", imports: [], fwd_expect: "40320" }
   , { file: "filter", imports: [], fwd_expect: "(8 : (7 : []))" }
   , { file: "first-class-constr", imports: [], fwd_expect: "((10 : []) : ((12 : []) : ((20 : []) : [])))" }
   , { file: "flatten"
     , imports: []
     , fwd_expect: "((3, \"simon\") : ((4, \"john\") : ((6, \"sarah\") : ((7, \"claire\") : []))))"
     }
   , { file: "foldr-sumSquares", imports: [], fwd_expect: "661" }
   , { file: "include-input-into-output"
     , imports: [ "example/lib/some-constants" ]
     , fwd_expect: "(1, 1)"
     }
   , { file: "lexicalScoping", imports: [], fwd_expect: "\"6\"" }
   , { file: "length", imports: [], fwd_expect: "2" }
   , { file: "lookup", imports: [], fwd_expect: "Some \"sarah\"" }
   , { file: "map", imports: [], fwd_expect: "(5 : (7 : (13 : (15 : (4 : (3 : (-3 : [])))))))" }
   , { file: "mergeSort", imports: [], fwd_expect: "(1 : (2 : (3 : [])))" }
   , { file: "normalise", imports: [], fwd_expect: "(33, 66)" }
   , { file: "nub", imports: [], fwd_expect: "(1 : (2 : (3 : (4 : []))))" }
   , { file: "pattern-match", imports: [], fwd_expect: "4" }
   , { file: "range", imports: [], fwd_expect: "((0, 0) : ((0, 1) : ((1, 0) : ((1, 1) : []))))" }
   , { file: "records", imports: [], fwd_expect: "{a : 2, b : 6, c : 7, d : (5 : []), e : 7}" }
   , { file: "record-lookup", imports: [], fwd_expect: "True" }
   , { file: "reverse", imports: [], fwd_expect: "(2 : (1 : []))" }
   ]
