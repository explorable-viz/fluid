module Test.Specs.IllFormed where

import Test.Util.Suite (IllFormedSpec)

illFormed_cases :: Array IllFormedSpec
illFormed_cases =
   [ { file: "unreachable.fld", expected_error: "Unreachable statement" }
   , { file: "mutual_split.fld", expected_error: "Unbound name: odd" }
   , { file: "cond_partial_def.fld", expected_error: "Not definitely assigned: x" }
   , { file: "no_else.fld", expected_error: "Not definitely assigned: x" }
   , { file: "shadow_captured.fld", expected_error: "Captured variable reassigned: x" }
   , { file: "shadow_captured_global.fld", expected_error: "Captured variable reassigned: x" }
   , { file: "shadow_captured_mutual.fld", expected_error: "Captured variable reassigned: g" }
   , { file: "self_capture.fld", expected_error: "Variable captured by its own definition: x" }
   , { file: "self_capture_lambda.fld", expected_error: "Variable captured by its own definition: f" }
   , { file: "unbound_local.fld", expected_error: "Not definitely assigned: y" }
   , { file: "duplicate_def_in_region.fld", expected_error: "Shape mismatch" }
   ]
