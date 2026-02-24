import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dMap$dInternal from "../Data.Map.Internal/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Parsing$dExpr from "../Parsing.Expr/index.js";
const $InfixParser = tag => tag;
const $OpDef = (_1, _2, _3, _4) => ({tag: "Infix", _1, _2, _3, _4});
const $$Symbol = /* #__PURE__ */ $InfixParser("Symbol");
const Ident = /* #__PURE__ */ $InfixParser("Ident");
const ConsOp = /* #__PURE__ */ $InfixParser("ConsOp");
const Custom = /* #__PURE__ */ $InfixParser("Custom");
const Infix = value0 => value1 => value2 => value3 => $OpDef(value0, value1, value2, value3);
const prec = v => v._4;
const opDefs = [
  /* #__PURE__ */ $OpDef($$Symbol, "!", Parsing$dExpr.AssocLeft, 9),
  /* #__PURE__ */ $OpDef($$Symbol, "**", Parsing$dExpr.AssocRight, 8),
  /* #__PURE__ */ $OpDef($$Symbol, "*", Parsing$dExpr.AssocLeft, 7),
  /* #__PURE__ */ $OpDef($$Symbol, "/", Parsing$dExpr.AssocLeft, 7),
  /* #__PURE__ */ $OpDef($$Symbol, "//", Parsing$dExpr.AssocLeft, 7),
  /* #__PURE__ */ $OpDef($$Symbol, "%", Parsing$dExpr.AssocLeft, 7),
  /* #__PURE__ */ $OpDef($$Symbol, "+", Parsing$dExpr.AssocLeft, 6),
  /* #__PURE__ */ $OpDef($$Symbol, "-", Parsing$dExpr.AssocLeft, 6),
  /* #__PURE__ */ $OpDef(ConsOp, ":", Parsing$dExpr.AssocRight, 5),
  /* #__PURE__ */ $OpDef($$Symbol, "++", Parsing$dExpr.AssocRight, 4),
  /* #__PURE__ */ $OpDef(Custom, "|x|", Parsing$dExpr.AssocLeft, 3),
  /* #__PURE__ */ $OpDef($$Symbol, "==", Parsing$dExpr.AssocNone, 2),
  /* #__PURE__ */ $OpDef($$Symbol, "/=", Parsing$dExpr.AssocNone, 2),
  /* #__PURE__ */ $OpDef($$Symbol, "<", Parsing$dExpr.AssocLeft, 2),
  /* #__PURE__ */ $OpDef($$Symbol, ">", Parsing$dExpr.AssocLeft, 2),
  /* #__PURE__ */ $OpDef($$Symbol, "<=", Parsing$dExpr.AssocLeft, 2),
  /* #__PURE__ */ $OpDef($$Symbol, ">=", Parsing$dExpr.AssocLeft, 2),
  /* #__PURE__ */ $OpDef(Ident, "and", Parsing$dExpr.AssocLeft, 1),
  /* #__PURE__ */ $OpDef(Ident, "or", Parsing$dExpr.AssocLeft, 0)
];
const name = v => v._2;
const opMap = /* #__PURE__ */ Data$dMap$dInternal.fromFoldable(Data$dOrd.ordString)(Data$dFoldable.foldableArray)(/* #__PURE__ */ Data$dFunctor.arrayMap(def => Data$dTuple.$Tuple(
  def._2,
  def
))(opDefs));
export {        opDefs, opMap, };
