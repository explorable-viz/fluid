module Test.Specs.IllFormed where

import Test.Util.Suite (IllFormedSpec)

-- Spec-derived (PurePy corpus): each entry corresponds to a test in
-- pure-py-spec/test/ill-formed/semantic.
purepy_cases :: Array IllFormedSpec
purepy_cases =
   [ { file: "purepy/unreachable.fld", expected_error: "Unreachable statement" }
   , { file: "purepy/mutual_split.fld", expected_error: "Unbound name: odd" }
   , { file: "purepy/cond_partial_def.fld", expected_error: "Not definitely assigned: x" }
   , { file: "purepy/no_else.fld", expected_error: "Not definitely assigned: x" }
   , { file: "purepy/shadow_captured.fld", expected_error: "Captured variable reassigned: x" }
   , { file: "purepy/shadow_captured_global.fld", expected_error: "Captured variable reassigned: x" }
   , { file: "purepy/shadow_captured_mutual.fld", expected_error: "Captured variable reassigned: g" }
   , { file: "purepy/self_capture.fld", expected_error: "Variable captured by its own definition: x" }
   , { file: "purepy/self_capture_lambda.fld", expected_error: "Variable captured by its own definition: f" }
   , { file: "purepy/unbound_local.fld", expected_error: "Not definitely assigned: y" }
   , { file: "purepy/duplicate_def_in_region.fld", expected_error: "Shape mismatch" }
   , { file: "purepy/mutual_split_by_assign.fld", expected_error: "Unbound name: g" }
   , { file: "purepy/mutual_def_block_local.fld", expected_error: "Not definitely assigned: g" }
   , { file: "purepy/match_var_leak.fld", expected_error: "Not definitely assigned: x" }
   , { file: "purepy/dataclass_dup_field.fld", expected_error: "Duplicate field names in class: Point" }
   , { file: "purepy/dataclass_unknown_base.fld", expected_error: "Unknown class: Unknown" }
   , { file: "purepy/dataclass_field_clash.fld", expected_error: "Class Sub redeclares inherited field(s): (\"x\" : Nil)" }
   , { file: "purepy/dataclass_duplicate.fld", expected_error: "Conflicting class declarations: Point" }
   ]

-- Fluid-specific ill-formed cases (no PurePy correspondent).
illFormed_cases :: Array IllFormedSpec
illFormed_cases =
   [ { file: "non_contiguous_def.fld", expected_error: "Non-contiguous clauses for: f" }
   , { file: "match_partial_def.fld", expected_error: "Not definitely assigned: result" }
   , { file: "match_exhaustive_assign.fld", expected_error: "Not definitely assigned: result" }
   ]
