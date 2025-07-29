module Test.Specs.Comments where

import Test.Util.Suite (TestSpec)

comments_cases :: Array TestSpec
comments_cases =
   [ { file: "comments/nested-constr"

     , fwd_expect: "\"\"\" This is a ${\"\"\" This is a nested docComment! \"\"\" True} docComment! \"\"\" False"
     }
   , { file: "comments/dicts"

     , fwd_expect:
          "\"\"\" We can have a docComment before a dict! \"\"\" {[\"d\"] : {}, [\"e\"] : {[\"a\"] : 5, [\"ab\"] : 6}, [\"e_ab\"] : 6, [\"f\"] : {[\"a\"] : 6, [\"ab\"] : 7}, [\"g\"] : {[\"a\"] : 5}}"
     }
   , { file: "comments/app-arg", fwd_expect: "(5 : (7 : (13 : (15 : (4 : (3 : (-3 : [])))))))" }
   , { file: "comments/list-comp", fwd_expect: "\"\"\" We can add comments to list comprehensions \"\"\" (14 : (12 : (10 : (13 : (11 : (9 : (12 : (10 : (8 : [])))))))))" }
   , { file: "comments/app", fwd_expect: "\"\"\" This function application returns the length of a list.\"\"\" 2" }
   , { file: "comments/int", fwd_expect: "\"\"\" Comment on ${1} \"\"\" 1" }
   , { file: "comments/projection", fwd_expect: "\"\"\" Test \"\"\" 1" }
   ]
