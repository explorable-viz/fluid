module Test.Specs.Comments where

import Test.Util.Suite (TestSpec)

comments_cases :: Array TestSpec
comments_cases =
   [ { file: "comments/nested-constr.fld"
     , fwd_expect: "@doc (\"\"\" This is a ${@doc (\"\"\" This is a nested docComment! \"\"\") True} docComment! \"\"\") False"
     }
   , { file: "comments/dicts.fld"
     , fwd_expect:
          "@doc (\"\"\" We can have a docComment before a dict! \"\"\") {[\"d\"] : {}, [\"e\"] : {[\"a\"] : 5, [\"ab\"] : 6}, [\"e_ab\"] : 6, [\"f\"] : {[\"a\"] : 6, [\"ab\"] : 7}, [\"g\"] : {[\"a\"] : 5}}"
     }
   , { file: "comments/app-arg.fld", fwd_expect: "@doc (\"\"\" Comments on arguments don't surface on the outermost application \"\"\") (5 : (7 : (13 : (15 : (4 : (3 : (-3 : [])))))))" }
   , { file: "comments/list-comp.fld", fwd_expect: "@doc (\"\"\" We can add comments to list comprehensions \"\"\") (14 : (12 : (10 : (13 : (11 : (9 : (12 : (10 : (8 : [])))))))))" }
   , { file: "comments/app.fld", fwd_expect: "@doc (\"\"\" This function application returns the length of a list.\"\"\") 2" }
   , { file: "comments/int.fld", fwd_expect: "@doc (\"\"\" Comment on ${1} \"\"\") 1" }
   , { file: "comments/projection.fld", fwd_expect: "@doc (\"\"\" Test \"\"\") 1" }
   ]
