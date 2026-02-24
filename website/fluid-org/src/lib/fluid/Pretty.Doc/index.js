import * as $runtime from "../runtime.js";
import * as Data$dString$dCodePoints from "../Data.String.CodePoints/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const $Doc = (tag, _1, _2) => ({tag, _1, _2});
const $Format = tag => tag;
const $Mode = tag => tag;
const Stmt = /* #__PURE__ */ $Mode("Stmt");
const Expr = /* #__PURE__ */ $Mode("Expr");
const Inline = /* #__PURE__ */ $Format("Inline");
const Multiline = /* #__PURE__ */ $Format("Multiline");
const Empty = /* #__PURE__ */ $Doc("Empty");
const Line = /* #__PURE__ */ $Doc("Line");
const Text = value0 => $Doc("Text", value0);
const Indent = value0 => $Doc("Indent", value0);
const Concat = value0 => value1 => $Doc("Concat", value0, value1);
const Mode = value0 => value1 => $Doc("Mode", value0, value1);
const StmtOrExpr = value0 => value1 => $Doc("StmtOrExpr", value0, value1);
const InlOrMul = value0 => value1 => $Doc("InlOrMul", value0, value1);
const semigroupDoc = {append: Concat};
const monoidDoc = {mempty: Empty, Semigroup0: () => semigroupDoc};
const width = m => doc => {
  if (doc.tag === "Empty") { return 0; }
  if (doc.tag === "Line") { return 0; }
  if (doc.tag === "Text") { return Data$dString$dCodePoints.toCodePointArray(doc._1).length; }
  if (doc.tag === "Indent") { return width(m)(doc._1); }
  if (doc.tag === "Concat") { return width(m)(doc._1) + width(m)(doc._2) | 0; }
  if (doc.tag === "Mode") { return width(doc._1)(doc._2); }
  if (doc.tag === "StmtOrExpr") {
    if (m === "Stmt") { return width(m)(doc._1); }
    if (m === "Expr") { return width(m)(doc._2); }
    $runtime.fail();
  }
  if (doc.tag === "InlOrMul") { return width(m)(doc._1); }
  $runtime.fail();
};
const text = Text;
const stmtOrExpr = StmtOrExpr;
const stmt = /* #__PURE__ */ Mode(Stmt);
const spaces = n => {
  if (n <= 0) { return ""; }
  return " " + spaces(n - 1 | 0);
};
const line = Line;
const inlinable = m => doc => {
  if (doc.tag === "Empty") { return true; }
  if (doc.tag === "Line") { return false; }
  if (doc.tag === "Text") { return true; }
  if (doc.tag === "Indent") { return false; }
  if (doc.tag === "Concat") { return inlinable(m)(doc._1) && inlinable(m)(doc._2); }
  if (doc.tag === "Mode") { return inlinable(doc._1)(doc._2); }
  if (doc.tag === "StmtOrExpr") {
    if (m === "Stmt") { return false; }
    if (m === "Expr") { return inlinable(m)(doc._2); }
    $runtime.fail();
  }
  if (doc.tag === "InlOrMul") { return inlinable(m)(doc._1); }
  $runtime.fail();
};
const inlOrMul = InlOrMul;
const indent = Indent;
const expr = /* #__PURE__ */ Mode(Expr);
const empty = Empty;
const config = {indentation: 2, lineWidth: 80};
const format = m => w => doc => {
  if (inlinable(m)(doc) && width(m)(doc) < (80 - w | 0)) { return Inline; }
  return Multiline;
};
const renderWithIndent = m => i => w => doc => {
  const indentation = i * 2 | 0;
  const fmt = format(m)(w)(doc);
  const $0 = (d1, d2) => {
    const v = renderWithIndent(m)(i)(w)(d1);
    const v1 = renderWithIndent(m)(i)(v._2)(d2);
    return Data$dTuple.$Tuple(v._1 + v1._1, v1._2);
  };
  if (doc.tag === "Empty") { return Data$dTuple.$Tuple("", w); }
  if (doc.tag === "Line") { return Data$dTuple.$Tuple(indentation <= 0 ? "\n" : "\n " + spaces(indentation - 1 | 0), indentation); }
  if (doc.tag === "Concat") {
    if (doc._1.tag === "Line") {
      if (doc._2.tag === "Empty") { return Data$dTuple.$Tuple("\n", 0); }
      if (doc._2.tag === "Line") { return Data$dTuple.$Tuple(indentation <= 0 ? "\n\n" : "\n\n " + spaces(indentation - 1 | 0), indentation); }
    }
    return $0(doc._1, doc._2);
  }
  if (doc.tag === "Text") { return Data$dTuple.$Tuple(doc._1, w + Data$dString$dCodePoints.toCodePointArray(doc._1).length | 0); }
  if (doc.tag === "Indent") { return renderWithIndent(m)(i + 1 | 0)(w)(doc._1); }
  if (doc.tag === "Mode") { return renderWithIndent(doc._1)(i)(w)(doc._2); }
  if (doc.tag === "StmtOrExpr") {
    if (m === "Stmt") { return renderWithIndent(m)(i)(w)(doc._1); }
    if (m === "Expr") { return renderWithIndent(m)(i)(w)(doc._2); }
    $runtime.fail();
  }
  if (doc.tag === "InlOrMul") {
    if (fmt === "Inline") { return renderWithIndent(m)(i)(w)(doc._1); }
    if (fmt === "Multiline") { return renderWithIndent(m)(i)(w)(doc._2); }
  }
  $runtime.fail();
};
const render = d => renderWithIndent(Stmt)(0)(0)(d)._1;
const beside = a => b => $Doc("Concat", a, $Doc("Concat", $Doc("Text", " "), b));
const above = a => b => $Doc("Concat", a, $Doc("Concat", Line, b));
const sep = a => b => $Doc("InlOrMul", $Doc("Concat", a, $Doc("Concat", $Doc("Text", " "), b)), $Doc("Concat", a, $Doc("Concat", Line, b)));
export {
  $Doc,
  
  
  
  Empty,
  
  
  
  
  Line,
  
  
  Stmt,
  
  Text,
  
  
  
  
  expr,
  
  
  
  
  
  
  
  renderWithIndent,
  
  
  
  stmt,
  
  
  
};
