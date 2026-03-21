const paragraph_cases = [
  {file: "paragraph/basic.fld", fwd_expect: "Paragraph(\"Hello\" :| \"there,\" :| Paragraph(\"Alice\" :| []) :| \"!\" :| [])"},
  {file: "paragraph/explicit.fld", fwd_expect: "Paragraph(Text(\"Hi \") :| \"Alice\" :| 5 :| \"+\" :| \"6\" :| \"is\" :| Text(\"16\") :| [])"}
];
export {paragraph_cases};
