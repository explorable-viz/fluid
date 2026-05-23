module Test.Specs.IllFormed where

import Test.Util.Suite (IllFormedSpec)

illFormed_cases :: Array IllFormedSpec
illFormed_cases =
   [ { file: "unreachable.fld", expected_error: "Unreachable statement" }
   , { file: "mutual_split.fld", expected_error: "Unbound name: odd" }
   , { file: "cond_partial_def.fld", expected_error: "Not definitely assigned: x" }
   , { file: "no_else.fld", expected_error: "Not definitely assigned: x" }
   ]
