module Test.Specs.IllFormed where

import Test.Util.Suite (IllFormedSpec)

-- Spec-derived (PurePy corpus): each entry corresponds to a test in
-- pure-py-spec/test/ill-formed/semantic.
purepy_cases :: Array IllFormedSpec
purepy_cases =
   [ { file: "purepy/attr_unknown_member.fld", expected_error: "module qual_lib has no member unknown" }
   , { file: "purepy/cond_partial_def.fld", expected_error: "Not definitely assigned: x" }
   , { file: "purepy/constr_bad_keyword.fld", expected_error: "Class Coord keyword fields mismatch: expected (\"y\" : Nil), got (\"z\" : Nil)" }
   , { file: "purepy/construct_arity.fld", expected_error: "Point expects 2 argument(s); got 3" }
   , { file: "purepy/dataclass_dup_class.fld", expected_error: "Conflicting class declarations: Point" }
   , { file: "purepy/dataclass_dup_field.fld", expected_error: "Duplicate field names in class: Point" }
   , { file: "purepy/dataclass_field_default.fld", expected_error: "\"ParseError on line 3, column 10:\\nExpected EOF\"" }
   , { file: "purepy/dataclass_inherited_field_clash.fld", expected_error: "Class Sub redeclares inherited field(s): (\"x\" : Nil)" }
   , { file: "purepy/dataclass_int_field.fld", expected_error: "\"ParseError on line 3, column 6:\\nExpected uppercase letter\"" }
   , { file: "purepy/dataclass_two_bases.fld", expected_error: "\"ParseError on line 10, column 10:\\nExpected ')'\"" }
   , { file: "purepy/dataclass_unknown_base.fld", expected_error: "Unknown class: Unknown" }
   , { file: "purepy/duplicate_def_in_region.fld", expected_error: "Shape mismatch" }
   , { file: "purepy/forward_class_in_def.fld", expected_error: "Unknown dataclass: Point" }
   , { file: "purepy/forward_class_top.fld", expected_error: "Unknown dataclass: Point" }
   , { file: "purepy/from_import_unknown_member.fld", expected_error: "Cannot import name baz from module module.two_vals_lib" }
   , { file: "purepy/import_in_def.fld", expected_error: "Import not at top level: module.foo" }
   , { file: "purepy/import_in_if.fld", expected_error: "Import not at top level: module.foo" }
   , { file: "purepy/import_in_match_case.fld", expected_error: "Import not at top level: module.foo" }
   , { file: "purepy/match_var_leak.fld", expected_error: "Not definitely assigned: x" }
   , { file: "purepy/mutual_def_block_local.fld", expected_error: "Not definitely assigned: g" }
   , { file: "purepy/mutual_split.fld", expected_error: "Unbound name: odd" }
   , { file: "purepy/mutual_split_by_assign.fld", expected_error: "Unbound name: g" }
   , { file: "purepy/no_else.fld", expected_error: "Not definitely assigned: x" }
   , { file: "purepy/pat_class_arity.fld", expected_error: "Assertion failure: " }
   , { file: "purepy/pat_class_bad_keyword.fld", expected_error: "Class Coord keyword fields mismatch: expected (\"y\" : Nil), got (\"z\" : Nil)" }
   , { file: "purepy/pat_class_unknown.fld", expected_error: "Unknown dataclass: NotAClass" }
   , { file: "purepy/self_capture.fld", expected_error: "Variable captured by its own definition: x" }
   , { file: "purepy/self_capture_lambda.fld", expected_error: "Variable captured by its own definition: f" }
   , { file: "purepy/shadow_captured.fld", expected_error: "Captured variable reassigned: x" }
   , { file: "purepy/shadow_captured_global.fld", expected_error: "Captured variable reassigned: x" }
   , { file: "purepy/shadow_captured_mutual.fld", expected_error: "Captured variable reassigned: g" }
   , { file: "purepy/unbound_local.fld", expected_error: "Not definitely assigned: y" }
   , { file: "purepy/unreachable.fld", expected_error: "Unreachable statement" }
   ]

-- Fluid-specific ill-formed cases (no PurePy correspondent).
illFormed_cases :: Array IllFormedSpec
illFormed_cases =
   [ { file: "bare_module.fld", expected_error: "module qual_lib is not a value" }
   , { file: "from_import_selective.fld", expected_error: "Unbound name: bar" }
   , { file: "match_exhaustive_assign.fld", expected_error: "Not definitely assigned: result" }
   , { file: "match_partial_def.fld", expected_error: "Not definitely assigned: result" }
   , { file: "non_contiguous_def.fld", expected_error: "Non-contiguous clauses for: f" }
   , { file: "use_before_import.fld", expected_error: "Unbound name: c\nChecking module use_before_import_mod" }
   ]
