import * as $runtime from "../runtime.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dSet from "../Data.Set/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Dict from "../Dict/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
import * as Pretty$dDoc from "../Pretty.Doc/index.js";
import * as Pretty$dUtil from "../Pretty.Util/index.js";
import * as Primitive$dParse from "../Primitive.Parse/index.js";
import * as SExpr from "../SExpr/index.js";
const lookup = k => {
  const go = go$a0$copy => {
    let go$a0 = go$a0$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0;
      if (v.tag === "Leaf") {
        go$c = false;
        go$r = Data$dMaybe.Nothing;
        continue;
      }
      if (v.tag === "Node") {
        const v1 = Data$dOrd.ordString.compare(k)(v._3);
        if (v1 === "LT") {
          go$a0 = v._5;
          continue;
        }
        if (v1 === "GT") {
          go$a0 = v._6;
          continue;
        }
        if (v1 === "EQ") {
          go$c = false;
          go$r = Data$dMaybe.$Maybe("Just", v._4);
          continue;
        }
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go;
};
const lessThanOrEq = a1 => a2 => a1 <= a2;
const lessThan = a1 => a2 => a1 < a2;
const toUnfoldable = /* #__PURE__ */ Foreign$dObject.toAscUnfoldable(Data$dList$dTypes.unfoldableList);
const fromFoldable = /* #__PURE__ */ Data$dFoldable.foldrArray(Data$dList$dTypes.Cons)(Data$dList$dTypes.Nil);
const PrettyShow = x => x;
const rootOpPattern = {
  rootOp: v => {
    if (v.tag === "PConstr" && v._1 === ":") { return Data$dMaybe.$Maybe("Just", ":"); }
    return Data$dMaybe.Nothing;
  }
};
const prettyString = {pretty: Pretty$dDoc.Text};
const newtypePrettyShow_ = {Coercible0: () => {}};
const isSimplePattern = {isSimple: v => true};
const vcommas = v => {
  if (v.tag === "Nil") { return Pretty$dDoc.Empty; }
  if (v.tag === "Cons") {
    if (v._2.tag === "Nil") { return v._1; }
    return Pretty$dDoc.$Doc("Concat", v._1, Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", ","), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.Line, vcommas(v._2))));
  }
  $runtime.fail();
};
const pretty = dict => dict.pretty;
const prettyP = dictPretty => x => Pretty$dDoc.renderWithIndent(Pretty$dDoc.Stmt)(0)(0)(dictPretty.pretty(x))._1;
const prettyForeignOp = {pretty: v => Pretty$dDoc.$Doc("Text", v._1)};
const showPrettyShow = dictPretty => ({show: v => Pretty$dDoc.renderWithIndent(Pretty$dDoc.Stmt)(0)(0)(dictPretty.pretty(v))._1});
const getPrec = x => {
  const v = lookup(x)(Primitive$dParse.opMap);
  if (v.tag === "Just") { return v._1._4; }
  if (v.tag === "Nothing") { return -1; }
  $runtime.fail();
};
const prettyConsArg = dictRootOp => dictIsSimple => dictPretty => e => lhs => {
  const v = dictRootOp.rootOp(e);
  if (v.tag === "Nothing") {
    if (dictIsSimple.isSimple(e)) { return dictPretty.pretty(e); }
    return Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "("), Pretty$dDoc.$Doc("Concat", dictPretty.pretty(e), Pretty$dDoc.$Doc("Text", ")")));
  }
  if (v.tag === "Just") {
    if ((lhs ? lessThanOrEq : lessThan)(getPrec(v._1))(getPrec(":"))) {
      return Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "("), Pretty$dDoc.$Doc("Concat", dictPretty.pretty(e), Pretty$dDoc.$Doc("Text", ")")));
    }
    return dictPretty.pretty(e);
  }
  $runtime.fail();
};
const compare = dictBotOf => dictNeg => dictMeetSemilattice => dictEq => dictPretty => op1 => op2 => x => y => {
  const $0 = dictMeetSemilattice.meet(dictNeg.neg(y))(x);
  const $1 = dictMeetSemilattice.meet(dictNeg.neg(x))(y);
  return Data$dTuple.$Tuple(
    dictEq.eq($0)(dictBotOf.botOf(x)) ? "" : op1 + " but not " + op2 + ":\n" + Pretty$dDoc.renderWithIndent(Pretty$dDoc.Stmt)(0)(0)(dictPretty.pretty($0))._1,
    dictEq.eq($1)(dictBotOf.botOf(x)) ? "" : op2 + " but not " + op1 + ":\n" + Pretty$dDoc.renderWithIndent(Pretty$dDoc.Stmt)(0)(0)(dictPretty.pretty($1))._1
  );
};
const commas = v => {
  if (v.tag === "Nil") { return Pretty$dDoc.Empty; }
  if (v.tag === "Cons") {
    if (v._2.tag === "Nil") { return v._1; }
    return Pretty$dDoc.$Doc("Concat", v._1, Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", ","), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), commas(v._2))));
  }
  $runtime.fail();
};
const prettyList = dictFoldable => {
  const fromFoldable1 = dictFoldable.foldr(Data$dList$dTypes.Cons)(Data$dList$dTypes.Nil);
  return dictPretty => {
    const pretty4 = dictPretty.pretty;
    return xs => commas(Data$dList$dTypes.listMap(pretty4)(fromFoldable1(xs)));
  };
};
const prettyList1 = /* #__PURE__ */ prettyList(Data$dList$dTypes.foldableList);
const prettyList2 = /* #__PURE__ */ prettyList(Data$dFoldable.foldableArray);
const prettyList3 = /* #__PURE__ */ prettyList(Dict.foldableDict);
const prettyList4 = /* #__PURE__ */ prettyList(Data$dSet.foldableSet)(prettyString);
const prettyConstr = dictRootOp => dictIsSimple => dictPretty => {
  const prettyList6 = prettyList1(dictPretty);
  return v => v1 => {
    if (v1.tag === "Nil") {
      if (v === "Nil") { return Pretty$dDoc.$Doc("Text", "[]"); }
      return Pretty$dDoc.$Doc("Text", v);
    }
    if (v1.tag === "Cons" && v1._2.tag === "Cons" && v1._2._2.tag === "Nil") {
      if (v === "Pair") {
        return Pretty$dDoc.$Doc(
          "Concat",
          Pretty$dDoc.$Doc("Text", "("),
          Pretty$dDoc.$Doc(
            "Concat",
            Pretty$dDoc.$Doc(
              "Concat",
              dictPretty.pretty(v1._1),
              Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", ","), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), dictPretty.pretty(v1._2._1)))
            ),
            Pretty$dDoc.$Doc("Text", ")")
          )
        );
      }
      if (v === ":") {
        return Pretty$dDoc.$Doc(
          "Concat",
          prettyConsArg(dictRootOp)(dictIsSimple)(dictPretty)(v1._1)(true),
          Pretty$dDoc.$Doc(
            "Concat",
            Pretty$dDoc.$Doc("Text", " "),
            Pretty$dDoc.$Doc(
              "Concat",
              Pretty$dDoc.$Doc("Text", ":|"),
              Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), prettyConsArg(dictRootOp)(dictIsSimple)(dictPretty)(v1._2._1)(false))
            )
          )
        );
      }
    }
    return Pretty$dDoc.$Doc(
      "Concat",
      Pretty$dDoc.$Doc("Text", v),
      Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "("), Pretty$dDoc.$Doc("Concat", prettyList6(v1), Pretty$dDoc.$Doc("Text", ")")))
    );
  };
};
const prettyString$x215Pattern = {
  pretty: v => Pretty$dDoc.$Doc(
    "Concat",
    Pretty$dDoc.$Doc("Text", v._1),
    Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", ":"), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), prettyPattern.pretty(v._2)))
  )
};
const prettyPattern = {
  pretty: v => {
    if (v.tag === "PVar") { return Pretty$dDoc.$Doc("Text", v._1); }
    if (v.tag === "PRecord") { return Pretty$dUtil.record(Data$dList$dTypes.listMap(prettyString$x215Pattern.pretty)(v._1)); }
    if (v.tag === "PConstr") {
      if (v._2.tag === "Nil") { return Pretty$dDoc.$Doc("Text", v._1); }
      return prettyConstr(rootOpPattern)(isSimplePattern)(prettyPattern)(v._1)(v._2);
    }
    if (v.tag === "PListEmpty") { return Pretty$dDoc.$Doc("Text", "[]"); }
    if (v.tag === "PListNonEmpty") {
      return Pretty$dDoc.$Doc(
        "Concat",
        Pretty$dDoc.$Doc("Text", "["),
        Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Concat", prettyPattern.pretty(v._1), prettyListRestPattern.pretty(v._2)), Pretty$dDoc.$Doc("Text", "]"))
      );
    }
    $runtime.fail();
  }
};
const prettyListRestPattern = {
  pretty: v => {
    if (v.tag === "PListVar") { return Pretty$dDoc.$Doc("Text", v._1); }
    if (v.tag === "PListNext") {
      return Pretty$dDoc.$Doc(
        "Concat",
        Pretty$dDoc.$Doc("Text", ","),
        Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), Pretty$dDoc.$Doc("Concat", prettyPattern.pretty(v._1), prettyListRestPattern.pretty(v._2)))
      );
    }
    if (v.tag === "PListEnd") { return Pretty$dDoc.Empty; }
    $runtime.fail();
  }
};
const prettyList5 = /* #__PURE__ */ prettyList1(prettyPattern);
const prettyVar$x215$x215Val = dictHighlightable => (
  {
    pretty: v => Pretty$dDoc.$Doc(
      "Concat",
      dictHighlightable.highlightIf(v._2._1)(Pretty$dDoc.$Doc("Text", v._1)),
      Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", ":"), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), prettyVal(dictHighlightable).pretty(v._2._2)))
    )
  }
);
const prettyVal = dictHighlightable => (
  {
    pretty: v => {
      if (v._2.tag === "Nothing") { return dictHighlightable.highlightIf(v._1)(prettyBaseVal(dictHighlightable).pretty(v._3)); }
      if (v._2.tag === "Just") {
        return Pretty$dDoc.$Doc(
          "Concat",
          Pretty$dDoc.$Doc("Text", "@doc"),
          Pretty$dDoc.$Doc(
            "Concat",
            Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "("), Pretty$dDoc.$Doc("Concat", prettyVal(dictHighlightable).pretty(v._2._1), Pretty$dDoc.$Doc("Text", ")"))),
            Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), dictHighlightable.highlightIf(v._1)(prettyBaseVal(dictHighlightable).pretty(v._3)))
          )
        );
      }
      $runtime.fail();
    }
  }
);
const prettyFun = dictHighlightable => {
  const prettyConstr2 = prettyConstr({
    rootOp: v => {
      if (v._2.tag === "Nothing") {
        if (v._3.tag === "Constr" && v._3._1 === ":") { return Data$dMaybe.$Maybe("Just", ":"); }
        return Data$dMaybe.Nothing;
      }
      if (v._2.tag === "Just") { return Data$dMaybe.Nothing; }
      $runtime.fail();
    }
  })({
    isSimple: v => {
      if (v._2.tag === "Nothing") { return true; }
      if (v._2.tag === "Just") { return false; }
      $runtime.fail();
    }
  });
  return {
    pretty: v => {
      if (v.tag === "Closure") { return Pretty$dDoc.$Doc("Text", "cl"); }
      if (v.tag === "Foreign") { return Pretty$dDoc.$Doc("Text", v._1._1); }
      if (v.tag === "PartialConstr") { return prettyConstr2(prettyVal(dictHighlightable))(v._1)(v._2); }
      $runtime.fail();
    }
  };
};
const prettyBaseVal = dictHighlightable => {
  const prettyConstr2 = prettyConstr({
    rootOp: v => {
      if (v._2.tag === "Nothing") {
        if (v._3.tag === "Constr" && v._3._1 === ":") { return Data$dMaybe.$Maybe("Just", ":"); }
        return Data$dMaybe.Nothing;
      }
      if (v._2.tag === "Just") { return Data$dMaybe.Nothing; }
      $runtime.fail();
    }
  })({
    isSimple: v => {
      if (v._2.tag === "Nothing") { return true; }
      if (v._2.tag === "Just") { return false; }
      $runtime.fail();
    }
  });
  return {
    pretty: v => {
      if (v.tag === "Int") { return Pretty$dDoc.$Doc("Text", Data$dShow.showIntImpl(v._1)); }
      if (v.tag === "Float") { return Pretty$dDoc.$Doc("Text", Data$dShow.showNumberImpl(v._1)); }
      if (v.tag === "Str") {
        return Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "\""), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", v._1), Pretty$dDoc.$Doc("Text", "\"")));
      }
      if (v.tag === "Dictionary") {
        if (Foreign$dObject.isEmpty(v._1)) { return Pretty$dDoc.$Doc("Text", "{}"); }
        return Pretty$dUtil.record(Data$dList$dTypes.listMap(prettyVar$x215$x215Val(dictHighlightable).pretty)(toUnfoldable(v._1)));
      }
      if (v.tag === "Constr") { return prettyConstr2(prettyVal(dictHighlightable))(v._1)(v._2); }
      if (v.tag === "Matrix") { return vcommas(fromFoldable(Data$dFunctor.arrayMap(prettyList2(prettyVal(dictHighlightable)))(v._1._1))); }
      if (v.tag === "Fun") { return prettyFun(dictHighlightable).pretty(v._1); }
      $runtime.fail();
    }
  };
};
const prettyEnv = dictHighlightable => (
  {
    pretty: v => {
      const go = v1 => {
        if (v1.tag === "Nil") { return Pretty$dDoc.Empty; }
        if (v1.tag === "Cons") {
          return Pretty$dDoc.$Doc(
            "Concat",
            Pretty$dDoc.$Doc(
              "Concat",
              Pretty$dDoc.$Doc("Text", v1._1._1),
              Pretty$dDoc.$Doc(
                "Concat",
                Pretty$dDoc.$Doc("Text", " "),
                Pretty$dDoc.$Doc(
                  "Concat",
                  Pretty$dDoc.$Doc("Text", "->"),
                  Pretty$dDoc.$Doc(
                    "Concat",
                    Pretty$dDoc.$Doc("Text", " "),
                    Pretty$dDoc.$Doc(
                      "Concat",
                      prettyVal(dictHighlightable).pretty(v1._1._2),
                      Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), Pretty$dDoc.$Doc("Text", ","))
                    )
                  )
                )
              )
            ),
            Pretty$dDoc.$Doc("Concat", Pretty$dDoc.Line, go(v1._2))
          );
        }
        $runtime.fail();
      };
      return Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "["), Pretty$dDoc.$Doc("Concat", go(toUnfoldable(v)), Pretty$dDoc.$Doc("Text", "]")));
    }
  }
);
const prettyPairExpr = dictHighlightable => (
  {
    pretty: v => Pretty$dDoc.$Doc(
      "Concat",
      prettyExpr(dictHighlightable).pretty(v._1),
      Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", ":"), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), prettyExpr(dictHighlightable).pretty(v._2)))
    )
  }
);
const prettyExpr = dictHighlightable => {
  const prettyConstr2 = prettyConstr({
    rootOp: v => {
      if (v.tag === "Constr" && v._2 === ":") { return Data$dMaybe.$Maybe("Just", ":"); }
      return Data$dMaybe.Nothing;
    }
  })({
    isSimple: v => {
      if (v.tag === "Constr") { return v._2 !== ":"; }
      if (v.tag === "Lambda") { return false; }
      if (v.tag === "Let") { return false; }
      return v.tag !== "LetRec";
    }
  });
  return {
    pretty: v => {
      if (v.tag === "Var") { return Pretty$dDoc.$Doc("Text", v._1); }
      if (v.tag === "Op") {
        return Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "("), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", v._1), Pretty$dDoc.$Doc("Text", ")")));
      }
      if (v.tag === "Int") { return dictHighlightable.highlightIf(v._1)(Pretty$dDoc.$Doc("Text", Data$dShow.showIntImpl(v._2))); }
      if (v.tag === "Float") { return dictHighlightable.highlightIf(v._1)(Pretty$dDoc.$Doc("Text", Data$dShow.showNumberImpl(v._2))); }
      if (v.tag === "Str") {
        return dictHighlightable.highlightIf(v._1)(Pretty$dDoc.$Doc(
          "Concat",
          Pretty$dDoc.$Doc("Text", "\""),
          Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", v._2), Pretty$dDoc.$Doc("Text", "\""))
        ));
      }
      if (v.tag === "Dictionary") { return dictHighlightable.highlightIf(v._1)(Pretty$dUtil.record(Data$dList$dTypes.listMap(prettyPairExpr(dictHighlightable).pretty)(v._2))); }
      if (v.tag === "Constr") { return dictHighlightable.highlightIf(v._1)(prettyConstr2(prettyExpr(dictHighlightable))(v._2)(v._3)); }
      if (v.tag === "Matrix") {
        return dictHighlightable.highlightIf(v._1)((() => {
          const $0 = Pretty$dDoc.$Doc(
            "Concat",
            prettyExpr(dictHighlightable).pretty(v._2),
            Pretty$dDoc.$Doc(
              "Concat",
              Pretty$dDoc.$Doc("Text", " "),
              Pretty$dDoc.$Doc(
                "Concat",
                Pretty$dDoc.$Doc("Text", "for"),
                Pretty$dDoc.$Doc(
                  "Concat",
                  Pretty$dDoc.$Doc("Text", " "),
                  Pretty$dDoc.$Doc(
                    "Concat",
                    Pretty$dDoc.$Doc(
                      "Concat",
                      Pretty$dDoc.$Doc("Text", "("),
                      Pretty$dDoc.$Doc(
                        "Concat",
                        Pretty$dDoc.$Doc(
                          "Concat",
                          Pretty$dDoc.$Doc("Text", v._3._1),
                          Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", ","), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), Pretty$dDoc.$Doc("Text", v._3._2)))
                        ),
                        Pretty$dDoc.$Doc("Text", ")")
                      )
                    ),
                    Pretty$dDoc.$Doc(
                      "Concat",
                      Pretty$dDoc.$Doc("Text", " "),
                      Pretty$dDoc.$Doc(
                        "Concat",
                        Pretty$dDoc.$Doc("Text", "in"),
                        Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), prettyExpr(dictHighlightable).pretty(v._4))
                      )
                    )
                  )
                )
              )
            )
          );
          return Pretty$dDoc.$Doc(
            "Concat",
            Pretty$dDoc.$Doc(
              "InlOrMul",
              Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "[|"), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), $0)),
              Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "[|"), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.Line, $0))
            ),
            Pretty$dDoc.$Doc("Text", "|]")
          );
        })());
      }
      if (v.tag === "Lambda") {
        return Pretty$dDoc.$Doc(
          "Concat",
          dictHighlightable.highlightIf(v._1)(Pretty$dDoc.$Doc("Text", "lambda")),
          Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), prettyElim(dictHighlightable).pretty(v._2))
        );
      }
      if (v.tag === "DProject") {
        return Pretty$dDoc.$Doc(
          "Concat",
          prettyExpr(dictHighlightable).pretty(v._1),
          Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "["), Pretty$dDoc.$Doc("Concat", prettyExpr(dictHighlightable).pretty(v._2), Pretty$dDoc.$Doc("Text", "]")))
        );
      }
      if (v.tag === "App") {
        return Pretty$dDoc.$Doc(
          "Concat",
          prettyExpr(dictHighlightable).pretty(v._1),
          Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "("), Pretty$dDoc.$Doc("Concat", prettyExpr(dictHighlightable).pretty(v._2), Pretty$dDoc.$Doc("Text", ")")))
        );
      }
      if (v.tag === "Let") {
        return Pretty$dDoc.$Doc(
          "Concat",
          Pretty$dDoc.$Doc("Text", "def"),
          Pretty$dDoc.$Doc(
            "Concat",
            Pretty$dDoc.$Doc("Text", " "),
            Pretty$dDoc.$Doc(
              "Concat",
              prettyElim(dictHighlightable).pretty(v._1._1),
              (() => {
                const $0 = prettyExpr(dictHighlightable).pretty(v._1._2);
                return Pretty$dDoc.$Doc(
                  "Concat",
                  Pretty$dDoc.$Doc(
                    "StmtOrExpr",
                    Pretty$dDoc.$Doc(
                      "Concat",
                      Pretty$dDoc.$Doc("Text", ":"),
                      Pretty$dDoc.$Doc(
                        "InlOrMul",
                        Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), $0),
                        Pretty$dDoc.$Doc("Indent", Pretty$dDoc.$Doc("Concat", Pretty$dDoc.Line, $0))
                      )
                    ),
                    Pretty$dDoc.$Doc(
                      "Concat",
                      Pretty$dDoc.$Doc("Text", ":"),
                      Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), Pretty$dDoc.$Doc("Concat", $0, Pretty$dDoc.$Doc("Text", ";")))
                    )
                  ),
                  Pretty$dDoc.$Doc("Concat", Pretty$dDoc.Line, prettyExpr(dictHighlightable).pretty(v._2))
                );
              })()
            )
          )
        );
      }
      if (v.tag === "LetRec") {
        return Pretty$dDoc.$Doc(
          "Concat",
          Pretty$dDoc.$Doc("Text", "def"),
          Pretty$dDoc.$Doc(
            "Concat",
            Pretty$dDoc.$Doc("Text", " "),
            Pretty$dDoc.$Doc("Concat", prettyDictElim(dictHighlightable).pretty(v._1._2), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.Line, prettyExpr(dictHighlightable).pretty(v._2)))
          )
        );
      }
      if (v.tag === "DocExpr") {
        return Pretty$dDoc.$Doc(
          "Concat",
          Pretty$dDoc.$Doc("Text", "@doc"),
          Pretty$dDoc.$Doc(
            "Concat",
            Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "("), Pretty$dDoc.$Doc("Concat", prettyExpr(dictHighlightable).pretty(v._1), Pretty$dDoc.$Doc("Text", ")"))),
            Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), prettyExpr(dictHighlightable).pretty(v._2))
          )
        );
      }
      $runtime.fail();
    }
  };
};
const prettyElim = dictHighlightable => (
  {
    pretty: v => {
      if (v.tag === "ElimVar") {
        return Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", v._1), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "->"), prettyCont(dictHighlightable).pretty(v._2)));
      }
      if (v.tag === "ElimConstr") { return prettyList3(prettyCont(dictHighlightable))(v._1); }
      if (v.tag === "ElimDict") {
        return Pretty$dDoc.$Doc(
          "Concat",
          Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "{"), Pretty$dDoc.$Doc("Concat", prettyList4(v._1), Pretty$dDoc.$Doc("Text", "}"))),
          Pretty$dDoc.$Doc(
            "Concat",
            Pretty$dDoc.$Doc("Text", " "),
            Pretty$dDoc.$Doc(
              "Concat",
              Pretty$dDoc.$Doc("Text", "->"),
              Pretty$dDoc.$Doc(
                "Concat",
                Pretty$dDoc.$Doc("Text", " "),
                Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "{"), Pretty$dDoc.$Doc("Concat", prettyCont(dictHighlightable).pretty(v._2), Pretty$dDoc.$Doc("Text", "}")))
              )
            )
          )
        );
      }
      $runtime.fail();
    }
  }
);
const prettyDictElim = dictHighlightable => (
  {
    pretty: ρ => {
      const go = v => {
        if (v.tag === "Nil") { return Pretty$dDoc.Empty; }
        if (v.tag === "Cons") {
          if (v._2.tag === "Nil") { return prettyBindElim(dictHighlightable).pretty(v._1); }
          return Pretty$dDoc.$Doc(
            "Concat",
            Pretty$dDoc.$Doc("Concat", go(v._2), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), Pretty$dDoc.$Doc("Text", ";"))),
            Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), prettyBindElim(dictHighlightable).pretty(v._1))
          );
        }
        $runtime.fail();
      };
      return go(toUnfoldable(ρ));
    }
  }
);
const prettyCont = dictHighlightable => (
  {
    pretty: v => {
      if (v.tag === "ContExpr") { return prettyExpr(dictHighlightable).pretty(v._1); }
      if (v.tag === "ContElim") { return prettyElim(dictHighlightable).pretty(v._1); }
      $runtime.fail();
    }
  }
);
const prettyBindElim = dictHighlightable => (
  {
    pretty: v => Pretty$dDoc.$Doc(
      "Concat",
      Pretty$dDoc.$Doc("Text", v._1),
      Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", ":"), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), prettyElim(dictHighlightable).pretty(v._2)))
    )
  }
);
const prettyEnvExpr = dictHighlightable => (
  {pretty: v => Pretty$dDoc.$Doc("Concat", prettyEnv(dictHighlightable).pretty(v._1), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.Line, prettyExpr(dictHighlightable).pretty(v._2)))}
);
const prettyVarDefs = dictAnn => (
  {
    pretty: ds => Pretty$dUtil.sep$p(Pretty$dDoc.$Doc("StmtOrExpr", Pretty$dDoc.Line, Pretty$dDoc.$Doc("Text", " ")))((() => {
      const $0 = prettyVarDef(dictAnn);
      return Data$dList$dTypes.$List("Cons", $0.pretty(ds._1), Data$dList$dTypes.listMap($0.pretty)(ds._2));
    })())
  }
);
const prettyVarDef = dictAnn => (
  {
    pretty: v => Pretty$dDoc.$Doc(
      "Concat",
      Pretty$dDoc.$Doc("Text", "def"),
      Pretty$dDoc.$Doc(
        "Concat",
        Pretty$dDoc.$Doc("Text", " "),
        Pretty$dDoc.$Doc(
          "Concat",
          prettyPattern.pretty(v._1),
          (() => {
            const $0 = prettyExpr1(dictAnn).pretty(v._2);
            return Pretty$dDoc.$Doc(
              "StmtOrExpr",
              Pretty$dDoc.$Doc(
                "Concat",
                Pretty$dDoc.$Doc("Text", ":"),
                Pretty$dDoc.$Doc(
                  "InlOrMul",
                  Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), $0),
                  Pretty$dDoc.$Doc("Indent", Pretty$dDoc.$Doc("Concat", Pretty$dDoc.Line, $0))
                )
              ),
              Pretty$dDoc.$Doc(
                "Concat",
                Pretty$dDoc.$Doc("Text", ":"),
                Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), Pretty$dDoc.$Doc("Concat", $0, Pretty$dDoc.$Doc("Text", ";")))
              )
            );
          })()
        )
      )
    )
  }
);
const prettyRecDefs = dictAnn => (
  {
    pretty: bs => Pretty$dUtil.sep$p(Pretty$dDoc.$Doc("StmtOrExpr", Pretty$dDoc.Line, Pretty$dDoc.$Doc("Text", " ")))((() => {
      const $0 = prettyBranch(dictAnn);
      return Data$dList$dTypes.$List("Cons", $0.pretty(bs._1), Data$dList$dTypes.listMap($0.pretty)(bs._2));
    })())
  }
);
const prettyPattern$x215Expr = dictAnn => (
  {
    pretty: v => Pretty$dDoc.$Doc(
      "Concat",
      Pretty$dDoc.$Doc("Text", "case"),
      Pretty$dDoc.$Doc(
        "Concat",
        Pretty$dDoc.$Doc("Text", " "),
        Pretty$dDoc.$Doc(
          "Concat",
          prettyPattern.pretty(v._1),
          (() => {
            const $0 = prettyExpr1(dictAnn).pretty(v._2);
            return Pretty$dDoc.$Doc(
              "StmtOrExpr",
              Pretty$dDoc.$Doc(
                "Concat",
                Pretty$dDoc.$Doc("Text", ":"),
                Pretty$dDoc.$Doc(
                  "InlOrMul",
                  Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), $0),
                  Pretty$dDoc.$Doc("Indent", Pretty$dDoc.$Doc("Concat", Pretty$dDoc.Line, $0))
                )
              ),
              Pretty$dDoc.$Doc(
                "Concat",
                Pretty$dDoc.$Doc("Text", ":"),
                Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), Pretty$dDoc.$Doc("Concat", $0, Pretty$dDoc.$Doc("Text", ";")))
              )
            );
          })()
        )
      )
    )
  }
);
const prettyParagraphElem = dictAnn => (
  {
    pretty: v => {
      if (v.tag === "Token") { return Pretty$dDoc.$Doc("Text", v._1); }
      if (v.tag === "Unquote") {
        return Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "{"), Pretty$dDoc.$Doc("Concat", prettyExpr1(dictAnn).pretty(v._1), Pretty$dDoc.$Doc("Text", "}")));
      }
      $runtime.fail();
    }
  }
);
const prettyNonEmptyListPattern = dictAnn => (
  {
    pretty: cs => Pretty$dUtil.vsep((() => {
      const $0 = prettyPattern$x215Expr(dictAnn);
      return Data$dList$dTypes.$List("Cons", $0.pretty(cs._1), Data$dList$dTypes.listMap($0.pretty)(cs._2));
    })())
  }
);
const prettyListQualifier = dictAnn => (
  {
    pretty: v => {
      const $0 = (q, qs) => Pretty$dDoc.$Doc(
        "Concat",
        prettyListQualifier(dictAnn).pretty(Data$dList$dTypes.$List("Cons", q, Data$dList$dTypes.Nil)),
        Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), prettyListQualifier(dictAnn).pretty(qs))
      );
      if (v.tag === "Cons") {
        if (v._2.tag === "Nil") {
          if (v._1.tag === "ListCompDecl") {
            return Pretty$dDoc.$Doc(
              "Concat",
              Pretty$dDoc.$Doc("Text", "def"),
              Pretty$dDoc.$Doc(
                "Concat",
                Pretty$dDoc.$Doc("Text", " "),
                Pretty$dDoc.$Doc(
                  "Concat",
                  prettyPattern.pretty(v._1._1._1),
                  Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", ":"), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), prettyExpr1(dictAnn).pretty(v._1._1._2)))
                )
              )
            );
          }
          if (v._1.tag === "ListCompGuard") {
            return Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "if"), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), prettyExpr1(dictAnn).pretty(v._1._1)));
          }
          if (v._1.tag === "ListCompGen") {
            return Pretty$dDoc.$Doc(
              "Concat",
              Pretty$dDoc.$Doc("Text", "for"),
              Pretty$dDoc.$Doc(
                "Concat",
                Pretty$dDoc.$Doc("Text", " "),
                Pretty$dDoc.$Doc(
                  "Concat",
                  prettyPattern.pretty(v._1._1),
                  Pretty$dDoc.$Doc(
                    "Concat",
                    Pretty$dDoc.$Doc("Text", " "),
                    Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "in"), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), prettyExpr1(dictAnn).pretty(v._1._2)))
                  )
                )
              )
            );
          }
        }
        return $0(v._1, v._2);
      }
      if (v.tag === "Nil") { return Pretty$dDoc.Empty; }
      $runtime.fail();
    }
  }
);
const prettyListParagraphElem = dictAnn => (
  {
    pretty: xs => Pretty$dDoc.$Doc(
      "Concat",
      Pretty$dDoc.$Doc("Text", "f\"\"\""),
      Pretty$dDoc.$Doc("Concat", Pretty$dUtil.hsep(Data$dList$dTypes.listMap(prettyParagraphElem(dictAnn).pretty)(xs)), Pretty$dDoc.$Doc("Text", "\"\"\""))
    )
  }
);
const prettyExpr1 = dictAnn => {
  const $0 = dictAnn.Highlightable0();
  const isSimpleExpr2 = {
    isSimple: v => {
      if (v.tag === "BinaryApp") { return false; }
      if (v.tag === "Constr") { return v._2 !== ":"; }
      if (v.tag === "Lambda") { return false; }
      if (v.tag === "Let") { return false; }
      if (v.tag === "LetRec") { return false; }
      if (v.tag === "IfElse") { return false; }
      return v.tag !== "MatchAs";
    }
  };
  const prettyConstr2 = prettyConstr({
    rootOp: v => {
      if (v.tag === "Constr") {
        if (v._2 === ":") { return Data$dMaybe.$Maybe("Just", ":"); }
        return Data$dMaybe.Nothing;
      }
      if (v.tag === "BinaryApp") { return Data$dMaybe.$Maybe("Just", v._2); }
      return Data$dMaybe.Nothing;
    }
  })(isSimpleExpr2);
  return {
    pretty: v => {
      if (v.tag === "Var") { return Pretty$dDoc.$Doc("Text", v._1); }
      if (v.tag === "Op") {
        return Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "("), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", v._1), Pretty$dDoc.$Doc("Text", ")")));
      }
      if (v.tag === "Int") { return $0.highlightIf(v._1)(Pretty$dDoc.$Doc("Text", Data$dShow.showIntImpl(v._2))); }
      if (v.tag === "Float") { return $0.highlightIf(v._1)(Pretty$dDoc.$Doc("Text", Data$dShow.showNumberImpl(v._2))); }
      if (v.tag === "Str") {
        return $0.highlightIf(v._1)(Pretty$dDoc.$Doc(
          "Concat",
          Pretty$dDoc.$Doc("Text", "\""),
          Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", v._2), Pretty$dDoc.$Doc("Text", "\""))
        ));
      }
      if (v.tag === "Constr") {
        if (v._3.tag === "Nil") { return $0.highlightIf(v._1)(Pretty$dDoc.$Doc("Text", v._2)); }
        return $0.highlightIf(v._1)(Pretty$dDoc.expr(prettyConstr2(prettyExpr1(dictAnn))(v._2)(v._3)));
      }
      if (v.tag === "Dictionary") {
        if (v._2.tag === "Nil") { return $0.highlightIf(v._1)(Pretty$dDoc.$Doc("Text", "{}")); }
        return $0.highlightIf(v._1)(Pretty$dDoc.expr(Pretty$dUtil.record(Data$dList$dTypes.listMap(prettyDictEntry$x215Expr(dictAnn).pretty)(v._2))));
      }
      if (v.tag === "Matrix") {
        return $0.highlightIf(v._1)(Pretty$dDoc.expr((() => {
          const $1 = Pretty$dDoc.$Doc(
            "Concat",
            prettyExpr1(dictAnn).pretty(v._2),
            Pretty$dDoc.$Doc(
              "Concat",
              Pretty$dDoc.$Doc("Text", " "),
              Pretty$dDoc.$Doc(
                "Concat",
                Pretty$dDoc.$Doc("Text", "for"),
                Pretty$dDoc.$Doc(
                  "Concat",
                  Pretty$dDoc.$Doc("Text", " "),
                  Pretty$dDoc.$Doc(
                    "Concat",
                    Pretty$dDoc.$Doc(
                      "Concat",
                      Pretty$dDoc.$Doc("Text", "("),
                      Pretty$dDoc.$Doc(
                        "Concat",
                        Pretty$dDoc.$Doc(
                          "Concat",
                          Pretty$dDoc.$Doc("Text", v._3._1),
                          Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", ","), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), Pretty$dDoc.$Doc("Text", v._3._2)))
                        ),
                        Pretty$dDoc.$Doc("Text", ")")
                      )
                    ),
                    Pretty$dDoc.$Doc(
                      "Concat",
                      Pretty$dDoc.$Doc("Text", " "),
                      Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "in"), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), prettyExpr1(dictAnn).pretty(v._4)))
                    )
                  )
                )
              )
            )
          );
          return Pretty$dDoc.$Doc(
            "Concat",
            Pretty$dDoc.$Doc(
              "InlOrMul",
              Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "[|"), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), $1)),
              Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "[|"), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.Line, $1))
            ),
            Pretty$dDoc.$Doc("Text", "|]")
          );
        })()));
      }
      if (v.tag === "Lambda") { return prettyClauses(dictAnn).pretty(v._1); }
      if (v.tag === "Project") {
        return Pretty$dDoc.expr(Pretty$dDoc.$Doc(
          "Concat",
          (() => {
            const $1 = prettyExpr1(dictAnn);
            if (isSimpleExpr2.isSimple(v._1)) { return $1.pretty(v._1); }
            return Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "("), Pretty$dDoc.$Doc("Concat", $1.pretty(v._1), Pretty$dDoc.$Doc("Text", ")")));
          })(),
          Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "."), Pretty$dDoc.$Doc("Text", v._2))
        ));
      }
      if (v.tag === "DProject") {
        return Pretty$dDoc.expr(Pretty$dDoc.$Doc(
          "Concat",
          (() => {
            const $1 = prettyExpr1(dictAnn);
            if (isSimpleExpr2.isSimple(v._1)) { return $1.pretty(v._1); }
            return Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "("), Pretty$dDoc.$Doc("Concat", $1.pretty(v._1), Pretty$dDoc.$Doc("Text", ")")));
          })(),
          Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "["), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.expr(prettyExpr1(dictAnn).pretty(v._2)), Pretty$dDoc.$Doc("Text", "]")))
        ));
      }
      if (v.tag === "App") { return Pretty$dDoc.expr(prettyAppChain(dictAnn)(SExpr.$Expr("App", v._1, v._2))(Data$dList$dTypes.Nil)); }
      if (v.tag === "BinaryApp") { return Pretty$dDoc.expr(binaryApp(dictAnn)(0)(SExpr.$Expr("BinaryApp", v._1, v._2, v._3))); }
      if (v.tag === "MatchAs") {
        return Pretty$dDoc.$Doc(
          "Concat",
          Pretty$dDoc.$Doc("Text", "match"),
          Pretty$dDoc.$Doc(
            "Concat",
            Pretty$dDoc.$Doc("Text", " "),
            Pretty$dDoc.$Doc(
              "Concat",
              prettyExpr1(dictAnn).pretty(v._1),
              (() => {
                const $1 = prettyNonEmptyListPattern(dictAnn).pretty(v._2);
                return Pretty$dDoc.$Doc(
                  "StmtOrExpr",
                  Pretty$dDoc.$Doc(
                    "Concat",
                    Pretty$dDoc.$Doc("Text", ":"),
                    Pretty$dDoc.$Doc(
                      "InlOrMul",
                      Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), $1),
                      Pretty$dDoc.$Doc("Indent", Pretty$dDoc.$Doc("Concat", Pretty$dDoc.Line, $1))
                    )
                  ),
                  Pretty$dDoc.$Doc(
                    "Concat",
                    Pretty$dDoc.$Doc("Text", ":"),
                    Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), Pretty$dDoc.$Doc("Concat", $1, Pretty$dDoc.$Doc("Text", ";")))
                  )
                );
              })()
            )
          )
        );
      }
      if (v.tag === "IfElse") {
        const prettyClause1 = w => v1 => Pretty$dDoc.$Doc(
          "Concat",
          Pretty$dDoc.$Doc("Text", w),
          Pretty$dDoc.$Doc(
            "Concat",
            Pretty$dDoc.$Doc("Text", " "),
            Pretty$dDoc.$Doc(
              "Concat",
              Pretty$dDoc.expr(prettyExpr1(dictAnn).pretty(v1._1)),
              (() => {
                const $1 = prettyExpr1(dictAnn).pretty(v1._2);
                return Pretty$dDoc.$Doc(
                  "StmtOrExpr",
                  Pretty$dDoc.$Doc(
                    "Concat",
                    Pretty$dDoc.$Doc("Text", ":"),
                    Pretty$dDoc.$Doc(
                      "InlOrMul",
                      Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), $1),
                      Pretty$dDoc.$Doc("Indent", Pretty$dDoc.$Doc("Concat", Pretty$dDoc.Line, $1))
                    )
                  ),
                  Pretty$dDoc.$Doc(
                    "Concat",
                    Pretty$dDoc.$Doc("Text", ":"),
                    Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), Pretty$dDoc.$Doc("Concat", $1, Pretty$dDoc.$Doc("Text", ";")))
                  )
                );
              })()
            )
          )
        );
        return Pretty$dDoc.$Doc(
          "Concat",
          Pretty$dUtil.vsep(Data$dList$dTypes.$List("Cons", prettyClause1("if")(v._1._1), Data$dList$dTypes.listMap(prettyClause1("elif"))(v._1._2))),
          Pretty$dDoc.$Doc(
            "Concat",
            Pretty$dDoc.Line,
            Pretty$dDoc.$Doc(
              "Concat",
              Pretty$dDoc.$Doc("Text", "else"),
              (() => {
                const $1 = prettyExpr1(dictAnn).pretty(v._2);
                return Pretty$dDoc.$Doc(
                  "StmtOrExpr",
                  Pretty$dDoc.$Doc(
                    "Concat",
                    Pretty$dDoc.$Doc("Text", ":"),
                    Pretty$dDoc.$Doc(
                      "InlOrMul",
                      Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), $1),
                      Pretty$dDoc.$Doc("Indent", Pretty$dDoc.$Doc("Concat", Pretty$dDoc.Line, $1))
                    )
                  ),
                  Pretty$dDoc.$Doc(
                    "Concat",
                    Pretty$dDoc.$Doc("Text", ":"),
                    Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), Pretty$dDoc.$Doc("Concat", $1, Pretty$dDoc.$Doc("Text", ";")))
                  )
                );
              })()
            )
          )
        );
      }
      if (v.tag === "ListEmpty") { return $0.highlightIf(v._1)(Pretty$dDoc.$Doc("Text", "[]")); }
      if (v.tag === "ListNonEmpty") {
        const collect = v1 => v2 => {
          if (v1.tag === "Next") {
            return Pretty$dDoc.$Doc(
              "Concat",
              $0.highlightIf(v1._1)(Pretty$dDoc.$Doc("Text", ",")),
              Pretty$dDoc.$Doc(
                "Concat",
                v2
                  ? Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), prettyExpr1(dictAnn).pretty(v1._2))
                  : Pretty$dDoc.$Doc("Indent", Pretty$dDoc.$Doc("Concat", Pretty$dDoc.Line, prettyExpr1(dictAnn).pretty(v1._2))),
                collect(v1._3)(v2)
              )
            );
          }
          if (v1.tag === "End") {
            if (v2) { return $0.highlightIf(v1._1)(Pretty$dDoc.$Doc("Text", "]")); }
            return Pretty$dDoc.$Doc("Concat", Pretty$dDoc.Line, $0.highlightIf(v1._1)(Pretty$dDoc.$Doc("Text", "]")));
          }
          $runtime.fail();
        };
        return Pretty$dDoc.$Doc(
          "Concat",
          $0.highlightIf(v._1)(Pretty$dDoc.$Doc("Text", "[")),
          Pretty$dDoc.$Doc(
            "InlOrMul",
            Pretty$dDoc.$Doc("Concat", prettyExpr1(dictAnn).pretty(v._2), collect(v._3)(true)),
            Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Indent", Pretty$dDoc.$Doc("Concat", Pretty$dDoc.Line, prettyExpr1(dictAnn).pretty(v._2))), collect(v._3)(false))
          )
        );
      }
      if (v.tag === "ListEnum") {
        return Pretty$dDoc.$Doc(
          "Concat",
          Pretty$dDoc.$Doc("Text", "["),
          Pretty$dDoc.$Doc(
            "Concat",
            Pretty$dDoc.expr(Pretty$dDoc.$Doc(
              "Concat",
              prettyExpr1(dictAnn).pretty(v._1),
              Pretty$dDoc.$Doc(
                "Concat",
                Pretty$dDoc.$Doc("Text", " "),
                Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", ".."), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), prettyExpr1(dictAnn).pretty(v._2)))
              )
            )),
            Pretty$dDoc.$Doc("Text", "]")
          )
        );
      }
      if (v.tag === "ListComp") {
        return $0.highlightIf(v._1)(Pretty$dDoc.$Doc(
          "Concat",
          Pretty$dDoc.$Doc("Text", "["),
          Pretty$dDoc.$Doc(
            "Concat",
            Pretty$dDoc.$Doc(
              "Concat",
              Pretty$dDoc.expr(prettyExpr1(dictAnn).pretty(v._2)),
              Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), prettyListQualifier(dictAnn).pretty(v._3))
            ),
            Pretty$dDoc.$Doc("Text", "]")
          )
        ));
      }
      if (v.tag === "Let") {
        return Pretty$dDoc.$Doc(
          "Concat",
          prettyVarDefs(dictAnn).pretty(v._1),
          Pretty$dDoc.$Doc(
            "Concat",
            Pretty$dDoc.$Doc("StmtOrExpr", Pretty$dDoc.$Doc("Concat", Pretty$dDoc.Line, Pretty$dDoc.Line), Pretty$dDoc.$Doc("Text", " ")),
            prettyExpr1(dictAnn).pretty(v._2)
          )
        );
      }
      if (v.tag === "LetRec") {
        return Pretty$dDoc.$Doc(
          "Concat",
          prettyRecDefs(dictAnn).pretty(v._1),
          Pretty$dDoc.$Doc(
            "Concat",
            Pretty$dDoc.$Doc("StmtOrExpr", Pretty$dDoc.$Doc("Concat", Pretty$dDoc.Line, Pretty$dDoc.Line), Pretty$dDoc.$Doc("Text", " ")),
            prettyExpr1(dictAnn).pretty(v._2)
          )
        );
      }
      if (v.tag === "Paragraph") { return prettyListParagraphElem(dictAnn).pretty(v._1); }
      if (v.tag === "DocExpr") {
        return Pretty$dDoc.$Doc(
          "Concat",
          Pretty$dDoc.$Doc("Text", "@doc"),
          (() => {
            const $1 = prettyExpr1(dictAnn).pretty(v._1);
            const $2 = prettyExpr1(dictAnn).pretty(v._2);
            return Pretty$dDoc.$Doc(
              "InlOrMul",
              Pretty$dDoc.$Doc(
                "Concat",
                Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "("), Pretty$dDoc.$Doc("Concat", $1, Pretty$dDoc.$Doc("Text", ")"))),
                Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), $2)
              ),
              Pretty$dDoc.$Doc(
                "Concat",
                Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "("), Pretty$dDoc.$Doc("Concat", $1, Pretty$dDoc.$Doc("Text", ")"))),
                Pretty$dDoc.$Doc("Concat", Pretty$dDoc.Line, $2)
              )
            );
          })()
        );
      }
      $runtime.fail();
    }
  };
};
const prettyDictEntry$x215Expr = dictAnn => (
  {
    pretty: v => Pretty$dDoc.$Doc(
      "Concat",
      prettyDictEntry(dictAnn).pretty(v._1),
      Pretty$dDoc.stmt(Pretty$dDoc.$Doc(
        "InlOrMul",
        Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", ":"), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), prettyExpr1(dictAnn).pretty(v._2))),
        Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", ":"), Pretty$dDoc.$Doc("Indent", Pretty$dDoc.$Doc("Concat", Pretty$dDoc.Line, prettyExpr1(dictAnn).pretty(v._2))))
      ))
    )
  }
);
const prettyDictEntry = dictAnn => (
  {
    pretty: v => {
      if (v.tag === "ExprKey") {
        return Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "["), Pretty$dDoc.$Doc("Concat", prettyExpr1(dictAnn).pretty(v._1), Pretty$dDoc.$Doc("Text", "]")));
      }
      if (v.tag === "VarKey") { return dictAnn.Highlightable0().highlightIf(v._1)(Pretty$dDoc.$Doc("Text", v._2)); }
      $runtime.fail();
    }
  }
);
const prettyClauses = dictAnn => ({pretty: v => prettyClause(dictAnn).pretty(v._1)});
const prettyClause = dictAnn => ({pretty: v => lambda(dictAnn)(Data$dList$dTypes.$List("Cons", v._1._1, v._1._2))(v._2)});
const prettyBranch = dictAnn => (
  {
    pretty: v => Pretty$dDoc.$Doc(
      "Concat",
      Pretty$dDoc.$Doc("Text", "def"),
      Pretty$dDoc.$Doc(
        "Concat",
        Pretty$dDoc.$Doc("Text", " "),
        Pretty$dDoc.$Doc(
          "Concat",
          Pretty$dDoc.$Doc("Text", v._1),
          Pretty$dDoc.$Doc(
            "Concat",
            Pretty$dDoc.$Doc(
              "Concat",
              Pretty$dDoc.$Doc("Text", "("),
              Pretty$dDoc.$Doc("Concat", prettyList5(Data$dList$dTypes.$List("Cons", v._2._1._1, v._2._1._2)), Pretty$dDoc.$Doc("Text", ")"))
            ),
            (() => {
              const $0 = prettyExpr1(dictAnn).pretty(v._2._2);
              return Pretty$dDoc.$Doc(
                "StmtOrExpr",
                Pretty$dDoc.$Doc(
                  "Concat",
                  Pretty$dDoc.$Doc("Text", ":"),
                  Pretty$dDoc.$Doc(
                    "InlOrMul",
                    Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), $0),
                    Pretty$dDoc.$Doc("Indent", Pretty$dDoc.$Doc("Concat", Pretty$dDoc.Line, $0))
                  )
                ),
                Pretty$dDoc.$Doc(
                  "Concat",
                  Pretty$dDoc.$Doc("Text", ":"),
                  Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), Pretty$dDoc.$Doc("Concat", $0, Pretty$dDoc.$Doc("Text", ";")))
                )
              );
            })()
          )
        )
      )
    )
  }
);
const prettyAppChain = dictAnn => v => v1 => {
  if (v.tag === "App") { return prettyAppChain(dictAnn)(v._1)(Data$dList$dTypes.$List("Cons", v._2, v1)); }
  return Pretty$dDoc.$Doc(
    "Concat",
    (() => {
      const $0 = prettyExpr1(dictAnn);
      if (
        (() => {
          if (v.tag === "BinaryApp") { return false; }
          if (v.tag === "Constr") { return v._2 !== ":"; }
          if (v.tag === "Lambda") { return false; }
          if (v.tag === "Let") { return false; }
          if (v.tag === "LetRec") { return false; }
          if (v.tag === "IfElse") { return false; }
          return v.tag !== "MatchAs";
        })()
      ) {
        return $0.pretty(v);
      }
      return Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "("), Pretty$dDoc.$Doc("Concat", $0.pretty(v), Pretty$dDoc.$Doc("Text", ")")));
    })(),
    Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "("), Pretty$dDoc.$Doc("Concat", prettyList1(prettyExpr1(dictAnn))(v1), Pretty$dDoc.$Doc("Text", ")")))
  );
};
const lambda = dictAnn => ps => e => Pretty$dDoc.$Doc(
  "Concat",
  Pretty$dDoc.$Doc("Text", "lambda"),
  Pretty$dDoc.$Doc(
    "Concat",
    Pretty$dDoc.$Doc("Text", " "),
    Pretty$dDoc.$Doc(
      "Concat",
      prettyList5(ps),
      Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", ":"), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), prettyExpr1(dictAnn).pretty(e)))
    )
  )
);
const binaryApp = dictAnn => v => v1 => {
  if (v1.tag === "BinaryApp") {
    const v2 = getPrec(v1._2);
    if (v2 === -1) {
      const customPrec = getPrec("|x|");
      return Pretty$dDoc.$Doc(
        "Concat",
        binaryApp(dictAnn)(customPrec)(v1._1),
        Pretty$dDoc.$Doc(
          "Concat",
          Pretty$dDoc.$Doc("Text", " "),
          Pretty$dDoc.$Doc(
            "Concat",
            Pretty$dDoc.$Doc("Text", "|"),
            Pretty$dDoc.$Doc(
              "Concat",
              Pretty$dDoc.$Doc("Text", v1._2),
              Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "|"), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), binaryApp(dictAnn)(customPrec)(v1._3)))
            )
          )
        )
      );
    }
    if (v2 <= v) {
      return Pretty$dDoc.$Doc(
        "Concat",
        Pretty$dDoc.$Doc("Text", "("),
        Pretty$dDoc.$Doc(
          "Concat",
          Pretty$dDoc.$Doc(
            "Concat",
            binaryApp(dictAnn)(v2)(v1._1),
            Pretty$dDoc.$Doc(
              "Concat",
              Pretty$dDoc.$Doc("Text", " "),
              Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", v1._2), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), binaryApp(dictAnn)(v2)(v1._3)))
            )
          ),
          Pretty$dDoc.$Doc("Text", ")")
        )
      );
    }
    return Pretty$dDoc.$Doc(
      "Concat",
      binaryApp(dictAnn)(v2)(v1._1),
      Pretty$dDoc.$Doc(
        "Concat",
        Pretty$dDoc.$Doc("Text", " "),
        Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", v1._2), Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", " "), binaryApp(dictAnn)(v2)(v1._3)))
      )
    );
  }
  const $0 = prettyExpr1(dictAnn);
  if (
    (() => {
      if (v1.tag === "BinaryApp") { return false; }
      if (v1.tag === "Constr") { return v1._2 !== ":"; }
      if (v1.tag === "Lambda") { return false; }
      if (v1.tag === "Let") { return false; }
      if (v1.tag === "LetRec") { return false; }
      if (v1.tag === "IfElse") { return false; }
      return v1.tag !== "MatchAs";
    })()
  ) {
    return $0.pretty(v1);
  }
  return Pretty$dDoc.$Doc("Concat", Pretty$dDoc.$Doc("Text", "("), Pretty$dDoc.$Doc("Concat", $0.pretty(v1), Pretty$dDoc.$Doc("Text", ")")));
};
export {
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  prettyBaseVal,
  
  
  
  
  
  
  
  
  
  
  
  prettyEnv,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  prettyP,
  
  
  
  
  
  
  
  prettyVal,
  
  
  
  
  
  
  
};
