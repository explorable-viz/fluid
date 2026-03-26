import * as $runtime from "../runtime.js";
import * as Pretty$dDoc from "../Pretty.Doc/index.js";
const vsep = v => {
  if (v.tag === "Nil") { return Pretty$dDoc.Empty; }
  if (v.tag === "Cons") {
    if (v._2.tag === "Nil") { return v._1; }
    return Pretty$dDoc.$Doc("Concat", v._1, Pretty$dDoc.$Doc("Concat", Pretty$dDoc.Line, vsep(v._2)));
  }
  $runtime.fail();
};
const sep$p = v => v1 => {
  if (v1.tag === "Nil") { return Pretty$dDoc.Empty; }
  if (v1.tag === "Cons") {
    if (v1._2.tag === "Nil") { return v1._1; }
    return Pretty$dDoc.$Doc("Concat", v1._1, Pretty$dDoc.$Doc("Concat", v, sep$p(v)(v1._2)));
  }
  $runtime.fail();
};
const sep = v => {
  if (v.tag === "Nil") { return Pretty$dDoc.Empty; }
  if (v.tag === "Cons") {
    if (v._2.tag === "Nil") { return v._1; }
    return Pretty$dDoc.$Doc("Concat", v._1, sep(v._2));
  }
  $runtime.fail();
};
const record = ds => Pretty$dDoc.$Doc(
  "InlOrMul",
  Pretty$dDoc.$Doc(
    "Concat",
    Pretty$dDoc.$Doc("Text", "{"),
    Pretty$dDoc.$Doc(
      "Concat",
      Pretty$dDoc.$Doc("Text", " "),
      Pretty$dDoc.$Doc("Concat", sep$p(Pretty$dDoc.$Doc("Text", ", "))(ds), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), Pretty$dDoc.$Doc("Text", "}")))
    )
  ),
  Pretty$dDoc.$Doc(
    "Concat",
    Pretty$dDoc.$Doc("Text", "{"),
    Pretty$dDoc.$Doc(
      "Concat",
      Pretty$dDoc.$Doc("Indent", Pretty$dDoc.$Doc("Concat", Pretty$dDoc.Line, sep$p(Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", ","), Pretty$dDoc.Line))(ds))),
      Pretty$dDoc.$Doc("Concat", Pretty$dDoc.Line, Pretty$dDoc.$Doc("Text", "}"))
    )
  )
);
const number = dictShow => s => Pretty$dDoc.$Doc("Text", dictShow.show(s));
const matrix = p => Pretty$dDoc.$Doc(
  "Concat",
  Pretty$dDoc.$Doc(
    "InlOrMul",
    Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "[|"), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), p)),
    Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "[|"), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.Line, p))
  ),
  Pretty$dDoc.$Doc("Text", "|]")
);
const hsep = v => {
  if (v.tag === "Nil") { return Pretty$dDoc.Empty; }
  if (v.tag === "Cons") {
    if (v._2.tag === "Nil") { return v._1; }
    return Pretty$dDoc.$Doc("Concat", v._1, Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), hsep(v._2)));
  }
  $runtime.fail();
};
const enclose = l => r => d => Pretty$dDoc.$Doc("Concat", l, Pretty$dDoc.$Doc("Concat", d, r));
const parens = d => Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "("), Pretty$dDoc.$Doc("Concat", d, Pretty$dDoc.$Doc("Text", ")")));
const pair = f => x => y => Pretty$dDoc.$Doc(
  "Concat",
  Pretty$dDoc.$Doc("Text", "("),
  Pretty$dDoc.$Doc(
    "Concat",
    Pretty$dDoc.$Doc("Concat", f(x), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", ","), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), f(y)))),
    Pretty$dDoc.$Doc("Text", ")")
  )
);
const quotes = d => Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "\""), Pretty$dDoc.$Doc("Concat", d, Pretty$dDoc.$Doc("Text", "\"")));
const string = s => Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "\""), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", s), Pretty$dDoc.$Doc("Text", "\"")));
const brackets = d => Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "["), Pretty$dDoc.$Doc("Concat", d, Pretty$dDoc.$Doc("Text", "]")));
const braces = d => Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "{"), Pretty$dDoc.$Doc("Concat", d, Pretty$dDoc.$Doc("Text", "}")));
const block = d => Pretty$dDoc.$Doc(
  "StmtOrExpr",
  Pretty$dDoc.$Doc(
    "Concat",
    Pretty$dDoc.$Doc("Text", ":"),
    Pretty$dDoc.$Doc("InlOrMul", Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), d), Pretty$dDoc.$Doc("Indent", Pretty$dDoc.$Doc("Concat", Pretty$dDoc.Line, d)))
  ),
  Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", ":"), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), Pretty$dDoc.$Doc("Concat", d, Pretty$dDoc.$Doc("Text", ";"))))
);
export {    hsep,      record,  sep$p,  vsep};
