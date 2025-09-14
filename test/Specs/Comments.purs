module Test.Specs.Comments where

import Test.Util.Suite (TestSpec)

comments_cases :: Array TestSpec
comments_cases =
   [ { file: "comments/nested-constr.fld"
     , fwd_expect: "@doc(Paragraph(Text(\"This\") :| Text(\"is\") :| Text(\"a\") :| @doc(Paragraph(Text(\"This\") :| Text(\"is\") :| Text(\"a\") :| Text(\"nested\") :| Text(\"docComment!\") :| [])) \"some string\" :| Text(\"docComment!\") :| [])) False"
     }
   , { file: "comments/dicts.fld"
     , fwd_expect:
          """@doc(Paragraph(Text("We") :| Text("can") :| Text("have") :| Text("a") :| Text("docComment") :| Text("before") :| Text("a") :| Text("dict!") :| [])) {
  d: {},
  e: { a: 5, ab: 6 },
  e_ab: 6,
  f: { a: 6, ab: 7 },
  g: { a: 5 }
}"""

     }
   , { file: "comments/app-arg.fld"
     , fwd_expect: "@doc(Paragraph(Text(\"Comments\") :| Text(\"on\") :| Text(\"arguments\") :| Text(\"don't\") :| Text(\"surface\") :| Text(\"on\") :| Text(\"the\") :| Text(\"outermost\") :| Text(\"application\") :| [])) 5 :| 7 :| 13 :| 15 :| 4 :| 3 :| -3 :| []"
     }
   , { file: "comments/list-comp.fld"
     , fwd_expect: "@doc(Paragraph(Text(\"We\") :| Text(\"can\") :| Text(\"add\") :| Text(\"comments\") :| Text(\"to\") :| Text(\"list\") :| Text(\"comprehensions\") :| [])) 14 :| 12 :| 10 :| 13 :| 11 :| 9 :| 12 :| 10 :| 8 :| []"
     }
   , { file: "comments/app.fld"
     , fwd_expect: "@doc(Paragraph(Text(\"This\") :| Text(\"function\") :| Text(\"application\") :| Text(\"returns\") :| Text(\"the\") :| Text(\"length\") :| Text(\"of\") :| Text(\"a\") :| Text(\"list.\") :| [])) 2"
     }
   , { file: "comments/int.fld"
     , fwd_expect: "@doc(Paragraph(Text(\"Comment\") :| Text(\"on\") :| Text(\"1\") :| [])) 1"
     }
   , { file: "comments/projection.fld", fwd_expect: "@doc(Paragraph(Text(\"Test\") :| [])) 1" }
   ]
