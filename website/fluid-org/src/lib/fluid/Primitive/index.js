import * as $runtime from "../runtime.js";
import * as Control$dCategory from "../Control.Category/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dInt from "../Data.Int/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMap$dInternal from "../Data.Map.Internal/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dProfunctor$dChoice from "../Data.Profunctor.Choice/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Effect$dException from "../Effect.Exception/index.js";
import * as Graph from "../Graph/index.js";
import * as Pretty from "../Pretty/index.js";
import * as Pretty$dDoc from "../Pretty.Doc/index.js";
import * as Val from "../Val/index.js";
const fanin = /* #__PURE__ */ Data$dProfunctor$dChoice.fanin(Control$dCategory.categoryFn)(Data$dProfunctor$dChoice.choiceFn);
const isZeroNumber = {isZero: $0 => 0.0 === $0};
const isZeroInt = {isZero: $0 => 0 === $0};
const unpack = toFrom => v => Data$dTuple.$Tuple(toFrom.unpack(v._3), v._1);
const union1 = v => v1 => v2 => {
  if (v2.tag === "Left") { return v(v2._1); }
  if (v2.tag === "Right") { return v1(v2._1); }
  $runtime.fail();
};
const unary = dictBoundedJoinSemilattice => {
  const bot = dictBoundedJoinSemilattice.bot;
  return id => f => Data$dTuple.$Tuple(
    id,
    Val.$Val(
      bot,
      Data$dMaybe.Nothing,
      Val.$BaseVal(
        "Fun",
        Val.$Fun(
          "Foreign",
          Data$dTuple.$Tuple(
            id,
            Val.$ForeignOp$p({
              arity: 1,
              op: dictMonadWithGraphAlloc => {
                const val = Val.val(dictMonadWithGraphAlloc);
                return dictMonadError => dictMonadAff => dictMonadReader => dictLoadFile => doc_opt => v => {
                  if (v.tag === "Cons" && v._2.tag === "Nil") {
                    return val(doc_opt)(Data$dMap$dInternal.$$$Map("Node", 1, 1, v._1._1, undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf))(f.o.pack(f.fwd(f.i.unpack(v._1._3))));
                  }
                  $runtime.fail();
                };
              }
            })
          ),
          Data$dList$dTypes.Nil
        )
      )
    )
  );
};
const typeError = v => typeName => Effect$dException.throwException(Effect$dException.error(typeName + " expected; got " + Pretty$dDoc.renderWithIndent(Pretty$dDoc.Stmt)(0)(0)(Pretty.prettyBaseVal(Val.highlightableUnit).pretty(Val.functorBaseVal.map(v$1 => {})(v)))._1))();
const string = {
  pack: Val.Str,
  unpack: v => {
    if (v.tag === "Str") { return v._1; }
    return typeError(v)("Str");
  }
};
const pack = toFrom => v => Val.$Val(v._2, Data$dMaybe.Nothing, toFrom.pack(v._1));
const number = {
  pack: Val.Float,
  unpack: v => {
    if (v.tag === "Float") { return v._1; }
    return typeError(v)("Float");
  }
};
const matrixRep = {
  pack: Val.Matrix,
  unpack: v => {
    if (v.tag === "Matrix") { return v._1; }
    return typeError(v)("Matrix");
  }
};
const isZero = dict1 => dict1.isZero;
const isZero$x43 = dictIsZero => {
  const isZero1 = dictIsZero.isZero;
  return dictIsZero1 => ({isZero: fanin(isZero1)(dictIsZero1.isZero)});
};
const intOrNumberOrString = {
  pack: v => {
    if (v.tag === "Left") { return Val.$BaseVal("Int", v._1); }
    if (v.tag === "Right") {
      if (v._1.tag === "Left") { return Val.$BaseVal("Float", v._1._1); }
      if (v._1.tag === "Right") { return Val.$BaseVal("Str", v._1._1); }
    }
    $runtime.fail();
  },
  unpack: v => {
    if (v.tag === "Int") { return Data$dEither.$Either("Left", v._1); }
    if (v.tag === "Float") { return Data$dEither.$Either("Right", Data$dEither.$Either("Left", v._1)); }
    if (v.tag === "Str") { return Data$dEither.$Either("Right", Data$dEither.$Either("Right", v._1)); }
    return typeError(v)("Int, Float or Str");
  }
};
const intOrNumber = {
  pack: v => {
    if (v.tag === "Left") { return Val.$BaseVal("Int", v._1); }
    if (v.tag === "Right") { return Val.$BaseVal("Float", v._1); }
    $runtime.fail();
  },
  unpack: v => {
    if (v.tag === "Int") { return Data$dEither.$Either("Left", v._1); }
    if (v.tag === "Float") { return Data$dEither.$Either("Right", v._1); }
    return typeError(v)("Int or Float");
  }
};
const $$int = {
  pack: Val.Int,
  unpack: v => {
    if (v.tag === "Int") { return v._1; }
    return typeError(v)("Int");
  }
};
const intPair = {
  pack: v => Val.$BaseVal(
    "Constr",
    "Pair",
    Data$dList$dTypes.$List(
      "Cons",
      Val.$Val(v._1._2, Data$dMaybe.Nothing, Val.$BaseVal("Int", v._1._1)),
      Data$dList$dTypes.$List("Cons", Val.$Val(v._2._2, Data$dMaybe.Nothing, Val.$BaseVal("Int", v._2._1)), Data$dList$dTypes.Nil)
    )
  ),
  unpack: v => {
    if (v.tag === "Constr" && v._2.tag === "Cons" && v._2._2.tag === "Cons" && v._2._2._2.tag === "Nil" && v._1 === "Pair") {
      return Data$dTuple.$Tuple(
        Data$dTuple.$Tuple(v._2._1._3.tag === "Int" ? v._2._1._3._1 : typeError(v._2._1._3)("Int"), v._2._1._1),
        Data$dTuple.$Tuple(v._2._2._1._3.tag === "Int" ? v._2._2._1._3._1 : typeError(v._2._2._1._3)("Int"), v._2._2._1._1)
      );
    }
    return typeError(v)("Pair");
  }
};
const dict = {
  pack: x => Val.$BaseVal("Dictionary", x),
  unpack: v => {
    if (v.tag === "Dictionary") { return v._1; }
    return typeError(v)("Dictionary");
  }
};
const $$boolean = {
  pack: v => {
    if (v) { return Val.$BaseVal("Constr", "True", Data$dList$dTypes.Nil); }
    return Val.$BaseVal("Constr", "False", Data$dList$dTypes.Nil);
  },
  unpack: v => {
    if (v.tag === "Constr" && v._2.tag === "Nil") {
      if (v._1 === "True") { return true; }
      if (v._1 === "False") { return false; }
    }
    return typeError(v)("Boolean");
  }
};
const binaryZero = dictBoundedJoinSemilattice => {
  const bot = dictBoundedJoinSemilattice.bot;
  return dictIsZero => id => f => Data$dTuple.$Tuple(
    id,
    Val.$Val(
      bot,
      Data$dMaybe.Nothing,
      Val.$BaseVal(
        "Fun",
        Val.$Fun(
          "Foreign",
          Data$dTuple.$Tuple(
            id,
            Val.$ForeignOp$p({
              arity: 2,
              op: dictMonadWithGraphAlloc => {
                const val = Val.val(dictMonadWithGraphAlloc);
                return dictMonadError => dictMonadAff => dictMonadReader => dictLoadFile => doc_opt => v => {
                  if (v.tag === "Cons" && v._2.tag === "Cons" && v._2._2.tag === "Nil") {
                    const $0 = f.i.unpack(v._1._3);
                    const $1 = f.i.unpack(v._2._1._3);
                    return val(doc_opt)((() => {
                      if (dictIsZero.isZero($0)) { return Data$dMap$dInternal.$$$Map("Node", 1, 1, v._1._1, undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf); }
                      if (dictIsZero.isZero($1)) { return Data$dMap$dInternal.$$$Map("Node", 1, 1, v._2._1._1, undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf); }
                      return Data$dMap$dInternal.insert(Graph.ordVertex)(v._2._1._1)()(Data$dMap$dInternal.$$$Map(
                        "Node",
                        1,
                        1,
                        v._1._1,
                        undefined,
                        Data$dMap$dInternal.Leaf,
                        Data$dMap$dInternal.Leaf
                      ));
                    })())(f.o.pack(f.fwd($0)($1)));
                  }
                  $runtime.fail();
                };
              }
            })
          ),
          Data$dList$dTypes.Nil
        )
      )
    )
  );
};
const binary = dictBoundedJoinSemilattice => {
  const bot = dictBoundedJoinSemilattice.bot;
  return id => f => Data$dTuple.$Tuple(
    id,
    Val.$Val(
      bot,
      Data$dMaybe.Nothing,
      Val.$BaseVal(
        "Fun",
        Val.$Fun(
          "Foreign",
          Data$dTuple.$Tuple(
            id,
            Val.$ForeignOp$p({
              arity: 2,
              op: dictMonadWithGraphAlloc => {
                const val = Val.val(dictMonadWithGraphAlloc);
                return dictMonadError => dictMonadAff => dictMonadReader => dictLoadFile => doc_opt => v => {
                  if (v.tag === "Cons" && v._2.tag === "Cons" && v._2._2.tag === "Nil") {
                    return val(doc_opt)(Data$dMap$dInternal.insert(Graph.ordVertex)(v._2._1._1)()(Data$dMap$dInternal.$$$Map(
                      "Node",
                      1,
                      1,
                      v._1._1,
                      undefined,
                      Data$dMap$dInternal.Leaf,
                      Data$dMap$dInternal.Leaf
                    )))(f.o.pack(f.fwd(f.i1.unpack(v._1._3))(f.i2.unpack(v._2._1._3))));
                  }
                  $runtime.fail();
                };
              }
            })
          ),
          Data$dList$dTypes.Nil
        )
      )
    )
  );
};
const asNumberString = {as: v => Effect$dException.throwException(Effect$dException.error("Non-uniform argument types"))()};
const asNumberIntOrNumber = {as: Data$dEither.Right};
const asIntNumberOrString = {as: x => Data$dEither.$Either("Left", Data$dInt.toNumber(x))};
const asIntNumber = {as: Data$dInt.toNumber};
const asIntIntOrNumber = {as: Data$dEither.Left};
const asBooleanBoolean = {as: x => x};
const as = dict1 => dict1.as;
const asIntorNumberNumber = {
  as: v => {
    if (v.tag === "Left") { return Data$dInt.toNumber(v._1); }
    if (v.tag === "Right") { return v._1; }
    $runtime.fail();
  }
};
const union = dictAs => dictAs1 => dictAs2 => dictAs3 => v => v1 => v2 => v3 => {
  if (v2.tag === "Left") {
    if (v3.tag === "Left") { return dictAs.as(v(v2._1)(v3._1)); }
    if (v3.tag === "Right") { return dictAs1.as(v1(dictAs2.as(v2._1))(v3._1)); }
    $runtime.fail();
  }
  if (v2.tag === "Right") {
    if (v3.tag === "Right") { return dictAs1.as(v1(v2._1)(v3._1)); }
    if (v3.tag === "Left") { return dictAs1.as(v1(v2._1)(dictAs3.as(v3._1))); }
  }
  $runtime.fail();
};
const unionStr = dictAs => dictAs1 => union(dictAs)(dictAs)(dictAs1)(dictAs1);
export {
  
  asBooleanBoolean,
  asIntIntOrNumber,
  asIntNumber,
  asIntNumberOrString,
  
  asNumberIntOrNumber,
  asNumberString,
  binary,
  binaryZero,
  $$boolean as boolean,
  
  fanin,
  $$int as int,
  intOrNumber,
  intOrNumberOrString,
  intPair,
  
  
  isZeroInt,
  isZeroNumber,
  
  number,
  
  string,
  typeError,
  unary,
  union,
  
  unionStr,
  
};
