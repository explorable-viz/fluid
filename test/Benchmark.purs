module Test.Benchmark
   ( bench_desugaring
   , main
   , minimalFail
   , minimalPass
   ) where

import Prelude hiding (add)

import Benchmark.Util (BenchAcc)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow, log)
import Benchmark.Runners (shouldSatisfy, benchMany)

main :: Effect Unit
main = launchAff_ do
   log "Running benchmarks!"
   arr <- bench_desugaring <> bench_misc
   logShow arr

-- runMocha [ minimalPass ]
-- runMocha [ minimalEval ]

minimalPass :: Aff Unit
minimalPass = liftEffect (log "it worked!")

minimalFail :: Aff Unit
minimalFail = shouldSatisfy "bool was false!" false (\x -> x)

bench_desugaring :: Aff BenchAcc
bench_desugaring = benchMany
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

bench_misc :: Aff BenchAcc
bench_misc = benchMany
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
