const comments_cases = [
  {
    file: "comments/nested-constr.fld",
    fwd_expect: "@doc(Paragraph(\"This\" :| \"is\" :| \"a\" :| (@doc(Paragraph(\"This\" :| \"is\" :| \"a\" :| \"nested\" :| \"docComment!\" :| [])) \"some string\") :| \"docComment!\" :| [])) False"
  },
  {
    file: "comments/dicts.fld",
    fwd_expect: "@doc(Paragraph(\"We\" :| \"can\" :| \"have\" :| \"a\" :| \"docComment\" :| \"before\" :| \"a\" :| \"dict!\" :| [])) {\n  d: {},\n  e: { a: 5, ab: 6 },\n  e_ab: 6,\n  f: { a: 6, ab: 7 },\n  g: { a: 5 }\n}"
  },
  {
    file: "comments/app-arg.fld",
    fwd_expect: "@doc(Paragraph(\"Comments\" :| \"on\" :| \"arguments\" :| \"don't\" :| \"surface\" :| \"on\" :| \"the\" :| \"outermost\" :| \"application\" :| [])) 5 :| 7 :| 13 :| 15 :| 4 :| 3 :| -3 :| []"
  },
  {
    file: "comments/list-comp.fld",
    fwd_expect: "@doc(Paragraph(\"We\" :| \"can\" :| \"add\" :| \"comments\" :| \"to\" :| \"list\" :| \"comprehensions\" :| [])) 14 :| 12 :| 10 :| 13 :| 11 :| 9 :| 12 :| 10 :| 8 :| []"
  },
  {
    file: "comments/app.fld",
    fwd_expect: "@doc(Paragraph(\"This\" :| \"function\" :| \"application\" :| \"returns\" :| \"the\" :| \"length\" :| \"of\" :| \"a\" :| \"list.\" :| [])) 2"
  },
  {file: "comments/int.fld", fwd_expect: "@doc(Paragraph(\"Comment\" :| \"on\" :| \"1\" :| [])) 1"},
  {file: "comments/projection.fld", fwd_expect: "1"}
];
export {comments_cases};
