module Test.Specs.Misc where

import Test.Util.Suite (TestSpec)

misc_cases :: Array TestSpec
misc_cases =
   [ { file: "arithmetic.fld", fwd_expect: "42" }
   , { file: "array.fld", fwd_expect: "(0, (3, 3))" }
   , { file: "boolean_precedence.fld", fwd_expect: "True" }
   , { file: "compose.fld", fwd_expect: "5" }
   , { file: "custom_infix.fld", fwd_expect: "True" }
   , { file: "dicts.fld"
     , fwd_expect: "{ d: {}, e: { a: 5, ab: 6 }, e_ab: 6, f: { a: 6, ab: 7 }, g: { a: 5 } }"
     }
   , { file: "div_mod_quot_rem.fld"
     , fwd_expect:
          "(1 :| -2 :| -2 :| 1 :| []) :| \
          \(2 :| -1 :| 1 :| -2 :| []) :| \
          \(1 :| -1 :| -1 :| 1 :| []) :| \
          \(2 :| 2 :| -2 :| -2 :| []) :| []"
     }
   , { file: "elif.fld", fwd_expect: """"much more" :| "more" :| "less" :| "much less" :| []""" }
   , { file: "factorial.fld", fwd_expect: "40320" }
   , { file: "filter.fld", fwd_expect: "8 :| 7 :| []" }
   , { file: "first_class_constr.fld", fwd_expect: "(10 :| []) :| (12 :| []) :| (20 :| []) :| []" }
   , { file: "flatten.fld"
     , fwd_expect: """(3, "simon") :| (4, "john") :| (6, "sarah") :| (7, "claire") :| []"""
     }
   , { file: "foldr_sum_squares.fld", fwd_expect: "661" }
   , { file: "include_input_into_output.fld"
     , fwd_expect: "(1, 1)"
     }
   , { file: "lexical_scoping.fld", fwd_expect: "\"6\"" } -- avoid triple-quotes here as VSCode gets confused
   , { file: "length.fld", fwd_expect: "2" }
   , { file: "lookup.fld", fwd_expect: """Some("sarah")""" }
   , { file: "map.fld", fwd_expect: "5 :| 7 :| 13 :| 15 :| 4 :| 3 :| -3 :| []" }
   , { file: "merge_sort.fld", fwd_expect: "1 :| 2 :| 3 :| []" }
   , { file: "normalise.fld", fwd_expect: "(33, 66)" }
   , { file: "not_parens_op.fld", fwd_expect: """@doc("hello") -42""" }
   , { file: "nub.fld", fwd_expect: "1 :| 2 :| 3 :| 4 :| []" }
   , { file: "paragraph.fld"
     , fwd_expect: """Paragraph(Text("As shown in Table 3, BiLSTM gives significantly  ") :| Text("better") :| [])"""
     }
   , { file: "pattern_match.fld", fwd_expect: "4" }
   , { file: "prefix_op.fld", fwd_expect: "True" }
   , { file: "range.fld", fwd_expect: "(0, 0) :| (0, 1) :| (1, 0) :| (1, 1) :| []" }
   , { file: "records.fld", fwd_expect: "{ a: 2, b: 6, c: 7, d: 5 :| [], e: 7 }" }
   , { file: "record_lookup.fld", fwd_expect: "True" }
   , { file: "reverse.fld", fwd_expect: "2 :| 1 :| []" }
   , { file: "module/import_simple.fld", fwd_expect: "84" }
   , { file: "module/import_simple_unused.fld", fwd_expect: "84" }
   , { file: "module/import_modules.fld", fwd_expect: "84" }
   ]
