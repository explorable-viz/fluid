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
   arr <- bench_desugaring
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