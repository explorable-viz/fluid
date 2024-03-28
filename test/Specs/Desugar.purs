module Test.Specs.Desugar where

import Test.Util.Suite (TestSpec)

desugar_cases :: Array TestSpec
desugar_cases =
   [ { file: "desugar/list-comp-1"
     , imports: []
     , fwd_expect: "(14 : (12 : (10 : (13 : (11 : (9 : (12 : (10 : (8 : [])))))))))"
     }
   , { file: "desugar/list-comp-2"
     , imports: []
     , fwd_expect:
          "(14 : (14 : (14 : (12 : (12 : (12 : (10 : (10 : (10 : (13 : (13 : (13 : (11 : (11 : (11 : (9 : \
          \(9 : (9 : (12 : (12 : (12 : (10 : (10 : (10 : (8 : (8 : (8 : [])))))))))))))))))))))))))))"
     }
   , { file: "desugar/list-comp-3", imports: [], fwd_expect: "(9 : (8 : []))" }
   , { file: "desugar/list-comp-4", imports: [], fwd_expect: "(5 : (4 : (3 : [])))" }
   , { file: "desugar/list-comp-5", imports: [], fwd_expect: "(5 : (4 : (3 : [])))" }
   , { file: "desugar/list-comp-6", imports: [], fwd_expect: "(5 : [])" }
   , { file: "desugar/list-comp-7", imports: [], fwd_expect: "([] : [])" }
   , { file: "desugar/list-comp-8", imports: [], fwd_expect: "(5 : (4 : (3 : [])))" }
   , { file: "desugar/list-comp-9", imports: [], fwd_expect: "(10 : (19 : []))" }
   , { file: "desugar/list-enum", imports: [], fwd_expect: "(3 : (4 : (5 : (6 : (7 : [])))))" }
   ]
