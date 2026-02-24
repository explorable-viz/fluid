import * as $runtime from "../runtime.js";
import * as Control$dMonad$dRec$dClass from "../Control.Monad.Rec.Class/index.js";
import * as Control$dMonad$dState$dTrans from "../Control.Monad.State.Trans/index.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dCodePoint$dUnicode from "../Data.CodePoint.Unicode/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dEnum from "../Data.Enum/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dString$dCodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data$dString$dCommon from "../Data.String.Common/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Parse$dNumber from "../Parse.Number/index.js";
import * as Parse$dParser from "../Parse.Parser/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Parsing$dCombinators from "../Parsing.Combinators/index.js";
import * as Parsing$dExpr from "../Parsing.Expr/index.js";
import * as Parsing$dIndent from "../Parsing.Indent/index.js";
import * as Parsing$dString from "../Parsing.String/index.js";
import * as Primitive$dParse from "../Primitive.Parse/index.js";
import * as SExpr from "../SExpr/index.js";
import * as Util from "../Util/index.js";
const runParserT = /* #__PURE__ */ Parsing.runParserT(/* #__PURE__ */ Control$dMonad$dState$dTrans.monadRecStateT(Control$dMonad$dRec$dClass.monadRecIdentity));
const choice = /* #__PURE__ */ Parsing$dCombinators.choice(Data$dFoldable.foldableArray);
const topLevel = p => (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => more(v2$1 => more(v1$1 => more(v2$2 => more(v1$2 => Parse$dParser.whitespace(
  state1,
  more,
  lift1,
  $$throw,
  (state2, a) => more(v2$3 => more(v3 => {
    const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
    return Parsing$dIndent.withPos(p)(
      state2$p,
      more,
      lift1,
      $$throw,
      (state3, a$1) => more(v4 => {
        const $0 = state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3;
        return more(v2$4 => more(v3$1 => {
          const state2$p$1 = state1._3 && !$0._3 ? Parsing.$ParseState($0._1, $0._2, true) : $0;
          return Parse$dParser.whitespace(
            state2$p$1,
            more,
            lift1,
            $$throw,
            (state3$1, a$2) => more(v4$1 => {
              const $1 = state2$p$1._3 && !state3$1._3 ? Parsing.$ParseState(state3$1._1, state3$1._2, true) : state3$1;
              return more(v2$5 => more(v3$2 => {
                const state2$p$2 = state1._3 && !$1._3 ? Parsing.$ParseState($1._1, $1._2, true) : $1;
                return Parsing$dString.eof(
                  state2$p$2,
                  more,
                  lift1,
                  $$throw,
                  (state3$2, a$3) => more(v4$2 => done(state2$p$2._3 && !state3$2._3 ? Parsing.$ParseState(state3$2._1, state3$2._2, true) : state3$2, a$1))
                );
              }));
            })
          );
        }));
      })
    );
  }))
)))))));
const parse = parser => input => {
  const $0 = runParserT(input)(parser)(Parsing.initialPos)._1;
  if ($0.tag === "Left") {
    return Data$dEither.$Either("Left", "ParseError on line " + Data$dShow.showIntImpl($0._1._2.line) + ", column " + Data$dShow.showIntImpl($0._1._2.column) + ":\n" + $0._1._1);
  }
  if ($0.tag === "Right") { return Data$dEither.$Either("Right", $0._1); }
  $runtime.fail();
};
const pConsOp = (state1, more, lift1, $$throw, done) => more(v1 => Parse$dParser.reservedOperator(":|")(
  state1,
  more,
  lift1,
  $$throw,
  (state2, a) => more(v2 => done(
    state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2,
    e => e$p => SExpr.$Pattern("PConstr", ":", Data$dList$dTypes.$List("Cons", e, Data$dList$dTypes.$List("Cons", e$p, Data$dList$dTypes.Nil)))
  ))
));
const simplePattern$lazy = /* #__PURE__ */ $runtime.binding(() => {
  const $0 = Parsing.lazyParserT.defer(v => (state1, more, lift1, $$throw, done) => more(v1 => Parse$dParser.braces(Parse$dParser.fields(Parse$dParser.variable)(pattern$lazy()))(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2 => done(state2, SExpr.$Pattern("PRecord", a)))
  )));
  const $1 = Parsing.lazyParserT.defer(v => (state1, more, lift1, $$throw, done) => more(v1 => Parse$dParser.brackets(Parse$dParser.trailingCommas(pattern$lazy()))(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2 => done(
      state2,
      (() => {
        if (a.tag === "Nil") { return SExpr.PListEmpty; }
        if (a.tag === "Cons") { return SExpr.$Pattern("PListNonEmpty", a._1, Data$dList$dTypes.foldableList.foldr(SExpr.PListNext)(SExpr.PListEnd)(a._2)); }
        $runtime.fail();
      })()
    ))
  )));
  const $2 = Parse$dParser.delim(Parse$dParser.parseableChar)("(");
  return (v2, $3, $4, $5, $6) => {
    const $7 = v2._1;
    const $8 = v2._2;
    return $3(v3 => $3(v1 => Parse$dParser.variable(
      Parsing.$ParseState($7, $8, false),
      $3,
      $4,
      (v4, $9) => {
        const $10 = v4._3;
        return $3(v5 => {
          if ($10) { return $5(v4, $9); }
          const $11 = v2._1;
          const $12 = v2._2;
          return $3(v3$1 => {
            const $13 = (v4$1, $13) => {
              const $14 = v4$1._3;
              return $3(v5$1 => {
                if ($14) { return $5(v4$1, $13); }
                const $15 = v2._1;
                const $16 = v2._2;
                return $3(v3$2 => $0(
                  Parsing.$ParseState($15, $16, false),
                  $3,
                  $4,
                  (v4$2, $17) => {
                    const $18 = v4$2._3;
                    return $3(v5$2 => {
                      if ($18) { return $5(v4$2, $17); }
                      const $19 = v2._1;
                      const $20 = v2._2;
                      return $3(v3$3 => $1(
                        Parsing.$ParseState($19, $20, false),
                        $3,
                        $4,
                        (v4$3, $21) => {
                          const $22 = v4$3._3;
                          return $3(v5$3 => {
                            if ($22) { return $5(v4$3, $21); }
                            return $3(v1$1 => $2(
                              v2,
                              $3,
                              $4,
                              $5,
                              (state2, a) => $3(v2$1 => {
                                const $23 = v2._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
                                return $3(v1$2 => pattern$lazy()(
                                  $23,
                                  $3,
                                  $4,
                                  $5,
                                  (state2$1, a$1) => $3(v2$2 => choice([
                                    (() => {
                                      const $24 = Parse$dParser.delim(Parse$dParser.parseableChar)(")");
                                      return (state1, more, lift1, $$throw, done) => more(v1$3 => $24(
                                        state1,
                                        more,
                                        lift1,
                                        $$throw,
                                        (state2$2, a$2) => more(v2$3 => done(state1._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2, a$1))
                                      ));
                                    })(),
                                    (() => {
                                      const $24 = Parse$dParser.delim(Parse$dParser.parseableChar)(",");
                                      return (state1, more, lift1, $$throw, done) => more(v1$3 => $24(
                                        state1,
                                        more,
                                        lift1,
                                        $$throw,
                                        (state2$2, a$2) => more(v2$3 => {
                                          const $25 = state1._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2;
                                          return more(v1$4 => pattern$lazy()(
                                            $25,
                                            more,
                                            lift1,
                                            $$throw,
                                            (state2$3, a$3) => more(v2$4 => {
                                              const $26 = Parse$dParser.delim(Parse$dParser.parseableChar)(")");
                                              const $27 = $25._3 && !state2$3._3 ? Parsing.$ParseState(state2$3._1, state2$3._2, true) : state2$3;
                                              return more(v1$5 => $26(
                                                $27,
                                                more,
                                                lift1,
                                                $$throw,
                                                (state2$4, a$4) => more(v2$5 => done(
                                                  $27._3 && !state2$4._3 ? Parsing.$ParseState(state2$4._1, state2$4._2, true) : state2$4,
                                                  SExpr.$Pattern(
                                                    "PConstr",
                                                    "Pair",
                                                    Data$dList$dTypes.$List("Cons", a$1, Data$dList$dTypes.$List("Cons", a$3, Data$dList$dTypes.Nil))
                                                  )
                                                ))
                                              ));
                                            })
                                          ));
                                        })
                                      ));
                                    })()
                                  ])($23._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1, $3, $4, $5, $6))
                                ));
                              })
                            ));
                          });
                        },
                        $6
                      ));
                    });
                  },
                  $6
                ));
              });
            };
            return $3(v1$1 => Parse$dParser.constructor(
              Parsing.$ParseState($11, $12, false),
              $3,
              $4,
              $13,
              (state2, a) => $3(v2$1 => $3(v1$2 => {
                const $14 = (state2$1, a$1) => $3(v2$2 => $6(
                  state2._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1,
                  SExpr.$Pattern("PConstr", a, a$1)
                ));
                const $15 = state2._1;
                const $16 = state2._2;
                return $3(v3$2 => Parse$dParser.parens(Parse$dParser.commas(simplePattern$lazy()))(
                  Parsing.$ParseState($15, $16, false),
                  $3,
                  $4,
                  (v4$1, $17) => {
                    const $18 = v4$1._3;
                    return $3(v5$1 => {
                      if ($18) { return $13(v4$1, $17); }
                      return $14(state2, Data$dList$dTypes.Nil);
                    });
                  },
                  $14
                ));
              }))
            ));
          });
        });
      },
      (state2, a) => $3(v2$1 => $6(state2, SExpr.$Pattern("PVar", a)))
    )));
  };
});
const pattern$lazy = /* #__PURE__ */ $runtime.binding(() => Parsing.lazyParserT.defer(v => Data$dFoldable.foldlArray(Parsing$dExpr.makeParser)(simplePattern$lazy())([
  [Parsing$dExpr.$Operator("Infix", pConsOp, Parsing$dExpr.AssocRight)]
])));
const simplePattern = /* #__PURE__ */ simplePattern$lazy();
const pattern = /* #__PURE__ */ pattern$lazy();
const infixSymbol = op => (state1, more, lift1, $$throw, done) => more(v1 => Parse$dParser.reservedOperator(op)(
  state1,
  more,
  lift1,
  $$throw,
  (state2, a) => more(v2 => done(state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2, e => e$p => SExpr.$Expr("BinaryApp", e, op, e$p)))
));
const infixIdent = op => {
  const $0 = Parse$dParser.reserved(op);
  return (state1, more, lift1, $$throw, done) => more(v1 => $0(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2 => done(state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2, e => e$p => SExpr.$Expr("BinaryApp", e, op, e$p)))
  ));
};
const infixCustom = /* #__PURE__ */ (() => {
  const $0 = Parse$dParser.delim(Parse$dParser.parseableChar)("|");
  return (state1, more, lift1, $$throw, done) => more(v1 => {
    const $1 = state1._3;
    return more(v2 => more(v1$1 => $0(
      state1,
      more,
      lift1,
      (v2$1, $2) => $$throw(Parsing.$ParseState(v2$1._1, v2$1._2, $1), $2),
      (state2, a) => more(v2$1 => more(v3 => {
        const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
        return Parse$dParser.variable(
          state2$p,
          more,
          lift1,
          (v2$2, $2) => $$throw(Parsing.$ParseState(v2$2._1, v2$2._2, $1), $2),
          (state3, a$1) => more(v4 => {
            const $2 = state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3;
            return more(v2$2 => {
              const $3 = Parse$dParser.delim(Parse$dParser.parseableChar)("|");
              const $4 = state1._3 && !$2._3 ? Parsing.$ParseState($2._1, $2._2, true) : $2;
              return more(v1$2 => $3(
                $4,
                more,
                lift1,
                $$throw,
                (state2$1, a$2) => more(v2$3 => done(
                  $4._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1,
                  e => e$p => SExpr.$Expr("BinaryApp", e, a$1, e$p)
                ))
              ));
            });
          })
        );
      }))
    )));
  });
})();
const imports_ = /* #__PURE__ */ Parsing$dCombinators.manyRec(/* #__PURE__ */ (() => {
  const $0 = Parse$dParser.reserved("import");
  const $1 = Data$dString$dCommon.joinWith("/");
  const $2 = Parsing$dCombinators.sepBy1(Parse$dParser.variable)(Parse$dParser.delim(Parse$dParser.parseableChar)("."));
  return (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => more(v2$1 => more(v1$1 => $0(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2$2 => more(v3 => {
      const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
      return more(v1$2 => $2(
        state2$p,
        more,
        lift1,
        $$throw,
        (state2$1, a$1) => more(v2$3 => {
          const $3 = $1(Data$dArray.fromFoldableImpl(Data$dList$dTypes.foldableNonEmptyList.foldr, a$1));
          return more(v4 => {
            const $4 = state2$p._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1;
            return more(v2$4 => more(v3$1 => {
              const state2$p$1 = state1._3 && !$4._3 ? Parsing.$ParseState($4._1, $4._2, true) : $4;
              return Parse$dParser.whitespace(
                state2$p$1,
                more,
                lift1,
                $$throw,
                (state3, a$2) => more(v4$1 => done(state2$p$1._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, $3))
              );
            }));
          });
        })
      ));
    }))
  )))));
})());
const withImports = p => topLevel((state1, more, lift1, $$throw, done) => more(v1 => imports_(
  state1,
  more,
  lift1,
  $$throw,
  (state2, a) => more(v2 => {
    const $0 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
    return more(v1$1 => p(
      $0,
      more,
      lift1,
      $$throw,
      (state2$1, a$1) => more(v2$1 => done($0._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1, Data$dTuple.$Tuple(a$1, a)))
    ));
  })
)));
const consOp = (state1, more, lift1, $$throw, done) => more(v1 => Parse$dParser.reservedOperator(":|")(
  state1,
  more,
  lift1,
  $$throw,
  (state2, a) => more(v2 => done(
    state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2,
    e => e$p => SExpr.$Expr("Constr", undefined, ":", Data$dList$dTypes.$List("Cons", e, Data$dList$dTypes.$List("Cons", e$p, Data$dList$dTypes.Nil)))
  ))
));
const opTable = /* #__PURE__ */ Data$dFunctor.arrayMap(/* #__PURE__ */ Data$dFunctor.arrayMap(v => Parsing$dExpr.$Operator(
  "Infix",
  (() => {
    if (v._1 === "Symbol") { return infixSymbol(v._2); }
    if (v._1 === "Ident") { return infixIdent(v._2); }
    if (v._1 === "ConsOp") { return consOp; }
    if (v._1 === "Custom") { return infixCustom; }
    $runtime.fail();
  })(),
  v._3
)))(/* #__PURE__ */ Data$dArray.sortBy(x => y => Data$dOrd.ordInt.compare((() => {
  if (0 < x.length) { return -x[0]._4; }
  $runtime.fail();
})())((() => {
  if (0 < y.length) { return -y[0]._4; }
  $runtime.fail();
})()))(/* #__PURE__ */ Data$dArray.groupBy(a => b => a._4 === b._4)(Primitive$dParse.opDefs)));
const varDefs$lazy = /* #__PURE__ */ $runtime.binding(() => Parsing$dCombinators.many1((() => {
  const $0 = Parse$dParser.reserved("def");
  const $1 = Parse$dParser.delim(Parse$dParser.parseableChar)(":");
  return (state1, more, lift1, $$throw, done) => more(v1 => {
    const $2 = state1._3;
    return more(v2 => more(v1$1 => more(v2$1 => more(v1$2 => $0(
      state1,
      more,
      lift1,
      (v2$2, $3) => $$throw(Parsing.$ParseState(v2$2._1, v2$2._2, $2), $3),
      (state2, a) => more(v2$2 => more(v3 => {
        const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
        return pattern(
          state2$p,
          more,
          lift1,
          (v2$3, $3) => $$throw(Parsing.$ParseState(v2$3._1, v2$3._2, $2), $3),
          (state3, a$1) => more(v4 => {
            const $3 = state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3;
            return more(v2$3 => more(v3$1 => {
              const state2$p$1 = state1._3 && !$3._3 ? Parsing.$ParseState($3._1, $3._2, true) : $3;
              return $1(
                state2$p$1,
                more,
                lift1,
                (v2$4, $4) => $$throw(Parsing.$ParseState(v2$4._1, v2$4._2, $2), $4),
                (state3$1, a$2) => more(v4$1 => {
                  const $4 = state2$p$1._3 && !state3$1._3 ? Parsing.$ParseState(state3$1._1, state3$1._2, true) : state3$1;
                  return more(v2$4 => {
                    const $5 = state1._3 && !$4._3 ? Parsing.$ParseState($4._1, $4._2, true) : $4;
                    return more(v1$3 => more(v2$5 => more(v1$4 => Parsing$dIndent.sameOrIndented(
                      $5,
                      more,
                      lift1,
                      $$throw,
                      (state2$1, a$3) => more(v2$6 => more(v3$2 => {
                        const state2$p$2 = $5._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1;
                        return Parsing$dIndent.withPos(expr$lazy())(
                          state2$p$2,
                          more,
                          lift1,
                          $$throw,
                          (state3$2, a$4) => more(v4$2 => {
                            const $6 = state2$p$2._3 && !state3$2._3 ? Parsing.$ParseState(state3$2._1, state3$2._2, true) : state3$2;
                            return more(v2$7 => done($5._3 && !$6._3 ? Parsing.$ParseState($6._1, $6._2, true) : $6, SExpr.$VarDef(a$1, a$4)));
                          })
                        );
                      }))
                    ))));
                  });
                })
              );
            }));
          })
        );
      }))
    )))));
  });
})()));
const recDefs$lazy = /* #__PURE__ */ $runtime.binding(() => Parsing$dCombinators.many1((() => {
  const $0 = Parse$dParser.reserved("def");
  const $1 = Parse$dParser.delim(Parse$dParser.parseableChar)("(");
  return (state1, more, lift1, $$throw, done) => more(v1 => {
    const $2 = state1._3;
    return more(v2 => more(v1$1 => more(v2$1 => more(v1$2 => $0(
      state1,
      more,
      lift1,
      (v2$2, $3) => $$throw(Parsing.$ParseState(v2$2._1, v2$2._2, $2), $3),
      (state2, a) => more(v2$2 => more(v3 => {
        const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
        return Parse$dParser.variable(
          state2$p,
          more,
          lift1,
          (v2$3, $3) => $$throw(Parsing.$ParseState(v2$3._1, v2$3._2, $2), $3),
          (state3, a$1) => more(v4 => {
            const $3 = state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3;
            return more(v2$3 => more(v3$1 => {
              const state2$p$1 = state1._3 && !$3._3 ? Parsing.$ParseState($3._1, $3._2, true) : $3;
              return $1(
                state2$p$1,
                more,
                lift1,
                (v2$4, $4) => $$throw(Parsing.$ParseState(v2$4._1, v2$4._2, $2), $4),
                (state3$1, a$2) => more(v4$1 => {
                  const $4 = state2$p$1._3 && !state3$1._3 ? Parsing.$ParseState(state3$1._1, state3$1._2, true) : state3$1;
                  return more(v2$4 => {
                    const $5 = state1._3 && !$4._3 ? Parsing.$ParseState($4._1, $4._2, true) : $4;
                    return more(v1$3 => Parse$dParser.commas1(pattern)(
                      $5,
                      more,
                      lift1,
                      $$throw,
                      (state2$1, a$3) => more(v2$5 => {
                        const $6 = Parse$dParser.delim(Parse$dParser.parseableChar)(")");
                        const $7 = $5._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1;
                        return more(v1$4 => $6(
                          $7,
                          more,
                          lift1,
                          $$throw,
                          (state2$2, a$4) => more(v2$6 => {
                            const $8 = Parse$dParser.block(expr$lazy());
                            const $9 = $7._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2;
                            return more(v1$5 => $8(
                              $9,
                              more,
                              lift1,
                              $$throw,
                              (state2$3, a$5) => more(v2$7 => done(
                                $9._3 && !state2$3._3 ? Parsing.$ParseState(state2$3._1, state2$3._2, true) : state2$3,
                                Data$dTuple.$Tuple(a$1, Data$dTuple.$Tuple(a$3, a$5))
                              ))
                            ));
                          })
                        ));
                      })
                    ));
                  });
                })
              );
            }));
          })
        );
      }))
    )))));
  });
})()));
const expr$lazy = /* #__PURE__ */ $runtime.binding(() => {
  const opTree$lazy = $runtime.binding(() => {
    const $0 = Parse$dParser.context("opTree")((() => {
      const chain = e => {
        const $0 = Parse$dParser.delim(Parse$dParser.parseableChar)(".");
        const $1 = Parse$dParser.delim(Parse$dParser.parseableChar)("[");
        const $2 = Parse$dParser.delim(Parse$dParser.parseableChar)("(");
        return (v2, $3, $4, $5, $6) => {
          const $7 = v2._1;
          const $8 = v2._2;
          return $3(v3 => {
            const $9 = (v4, $9) => {
              const $10 = v4._3;
              return $3(v5 => {
                if ($10) { return $5(v4, $9); }
                return $6(v2, e);
              });
            };
            return $3(v2$1 => $3(v1 => Parsing$dIndent.sameOrIndented(
              Parsing.$ParseState($7, $8, false),
              $3,
              $4,
              $9,
              (state2, a) => $3(v2$2 => $3(v3$1 => {
                const $10 = (state3, a$1) => $3(v4 => $6(state2._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a$1));
                const $11 = state2._1;
                const $12 = state2._2;
                return $3(v3$2 => {
                  const $13 = (v4, $13) => {
                    const $14 = v4._3;
                    return $3(v5 => {
                      if ($14) { return $9(v4, $13); }
                      const $15 = state2._1;
                      const $16 = state2._2;
                      return $3(v3$3 => {
                        const $17 = (v4$1, $17) => {
                          const $18 = v4$1._3;
                          return $3(v5$1 => {
                            if ($18) { return $9(v4$1, $17); }
                            return $3(v1$1 => $2(
                              state2,
                              $3,
                              $4,
                              $9,
                              (state2$1, a$1) => $3(v2$3 => {
                                const $19 = state2._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1;
                                return $3(v1$2 => Parse$dParser.commas(opTree$lazy())(
                                  $19,
                                  $3,
                                  $4,
                                  $9,
                                  (state2$2, a$2) => $3(v2$4 => {
                                    const $20 = Parse$dParser.close(Parse$dParser.parseableChar)(")");
                                    const $21 = $19._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2;
                                    return $3(v1$3 => $20(
                                      $21,
                                      $3,
                                      $4,
                                      $9,
                                      (state2$3, a$3) => $3(v2$5 => (e.tag === "Constr"
                                        ? chain(SExpr.$Expr(
                                            "Constr",
                                            e._1,
                                            e._2,
                                            Data$dList$dTypes.foldableList.foldr(Data$dList$dTypes.Cons)(Data$dList$dTypes.foldableList.foldr(Data$dList$dTypes.Cons)(Data$dList$dTypes.Nil)(a$2))(e._3)
                                          ))
                                        : chain((() => {
                                            const go = go$a0$copy => go$a1$copy => {
                                              let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
                                              while (go$c) {
                                                const b = go$a0, v = go$a1;
                                                if (v.tag === "Nil") {
                                                  go$c = false;
                                                  go$r = b;
                                                  continue;
                                                }
                                                if (v.tag === "Cons") {
                                                  go$a0 = SExpr.$Expr("App", b, v._1);
                                                  go$a1 = v._2;
                                                  continue;
                                                }
                                                $runtime.fail();
                                              }
                                              return go$r;
                                            };
                                            return go(e)(a$2);
                                          })()))($21._3 && !state2$3._3 ? Parsing.$ParseState(state2$3._1, state2$3._2, true) : state2$3, $3, $4, $9, $10))
                                    ));
                                  })
                                ));
                              })
                            ));
                          });
                        };
                        return $3(v1$1 => $1(
                          Parsing.$ParseState($15, $16, false),
                          $3,
                          $4,
                          $17,
                          (state2$1, a$1) => $3(v2$3 => $3(v1$2 => opTree$lazy()(
                            state2$1,
                            $3,
                            $4,
                            $17,
                            (state2$2, a$2) => $3(v2$4 => {
                              const $18 = Parse$dParser.close(Parse$dParser.parseableChar)("]");
                              const $19 = state2$1._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2;
                              return $3(v1$3 => $18(
                                $19,
                                $3,
                                $4,
                                $17,
                                (state2$3, a$3) => $3(v2$5 => chain(SExpr.$Expr("DProject", e, a$2))(
                                  $19._3 && !state2$3._3 ? Parsing.$ParseState(state2$3._1, state2$3._2, true) : state2$3,
                                  $3,
                                  $4,
                                  $17,
                                  $10
                                ))
                              ));
                            })
                          )))
                        ));
                      });
                    });
                  };
                  return $3(v1$1 => $3(v1$2 => $0(
                    Parsing.$ParseState($11, $12, false),
                    $3,
                    $4,
                    (v2$3, $14) => $13(Parsing.$ParseState(v2$3._1, v2$3._2, false), $14),
                    (state2$1, a$1) => $3(v2$3 => Parse$dParser.variable(
                      state2$1,
                      $3,
                      $4,
                      (v2$4, $14) => $13(Parsing.$ParseState(v2$4._1, v2$4._2, false), $14),
                      (state2$2, a$2) => $3(v2$4 => chain(SExpr.$Expr("Project", e, a$2))(state2$2, $3, $4, $13, $10))
                    ))
                  )));
                });
              }))
            )));
          });
        };
      };
      return Data$dFoldable.foldlArray(Parsing$dExpr.makeParser)(Parsing$dIndent.withPos((() => {
        const $0 = Parse$dParser.context("simple")((() => {
          const $0 = Parse$dParser.context("letExpr")((() => {
            const $0 = Parsing$dCombinators.many1((() => {
              const $0 = Parse$dParser.reserved("def");
              const $1 = Parse$dParser.delim(Parse$dParser.parseableChar)(":");
              return (state1, more, lift1, $$throw, done) => more(v1 => {
                const $2 = state1._3;
                return more(v2 => more(v1$1 => more(v2$1 => more(v1$2 => $0(
                  state1,
                  more,
                  lift1,
                  (v2$2, $3) => $$throw(Parsing.$ParseState(v2$2._1, v2$2._2, $2), $3),
                  (state2, a) => more(v2$2 => more(v3 => {
                    const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
                    return pattern(
                      state2$p,
                      more,
                      lift1,
                      (v2$3, $3) => $$throw(Parsing.$ParseState(v2$3._1, v2$3._2, $2), $3),
                      (state3, a$1) => more(v4 => {
                        const $3 = state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3;
                        return more(v2$3 => more(v3$1 => {
                          const state2$p$1 = state1._3 && !$3._3 ? Parsing.$ParseState($3._1, $3._2, true) : $3;
                          return $1(
                            state2$p$1,
                            more,
                            lift1,
                            (v2$4, $4) => $$throw(Parsing.$ParseState(v2$4._1, v2$4._2, $2), $4),
                            (state3$1, a$2) => more(v4$1 => {
                              const $4 = state2$p$1._3 && !state3$1._3 ? Parsing.$ParseState(state3$1._1, state3$1._2, true) : state3$1;
                              return more(v2$4 => {
                                const $5 = state1._3 && !$4._3 ? Parsing.$ParseState($4._1, $4._2, true) : $4;
                                return more(v1$3 => opTree$lazy()(
                                  $5,
                                  more,
                                  lift1,
                                  $$throw,
                                  (state2$1, a$3) => more(v2$5 => {
                                    const $6 = Parse$dParser.delim(Parse$dParser.parseableChar)(";");
                                    const $7 = $5._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1;
                                    return more(v1$4 => $6(
                                      $7,
                                      more,
                                      lift1,
                                      $$throw,
                                      (state2$2, a$4) => more(v2$6 => done(
                                        $7._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2,
                                        SExpr.$VarDef(a$1, a$3)
                                      ))
                                    ));
                                  })
                                ));
                              });
                            })
                          );
                        }));
                      })
                    );
                  }))
                )))));
              });
            })());
            return (state1, more, lift1, $$throw, done) => more(v1 => $0(
              state1,
              more,
              lift1,
              $$throw,
              (state2, a) => more(v2 => {
                const $1 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
                return more(v1$1 => opTree$lazy()(
                  $1,
                  more,
                  lift1,
                  $$throw,
                  (state2$1, a$1) => more(v2$1 => done($1._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1, SExpr.$Expr("Let", a, a$1)))
                ));
              })
            ));
          })());
          const $1 = Parse$dParser.context("letRecExpr")((() => {
            const $1 = Parsing$dCombinators.many1((() => {
              const $1 = Parse$dParser.reserved("def");
              const $2 = Parse$dParser.delim(Parse$dParser.parseableChar)("(");
              return (state1, more, lift1, $$throw, done) => more(v1 => {
                const $3 = state1._3;
                return more(v2 => more(v1$1 => more(v2$1 => more(v1$2 => $1(
                  state1,
                  more,
                  lift1,
                  (v2$2, $4) => $$throw(Parsing.$ParseState(v2$2._1, v2$2._2, $3), $4),
                  (state2, a) => more(v2$2 => more(v3 => {
                    const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
                    return Parse$dParser.variable(
                      state2$p,
                      more,
                      lift1,
                      (v2$3, $4) => $$throw(Parsing.$ParseState(v2$3._1, v2$3._2, $3), $4),
                      (state3, a$1) => more(v4 => {
                        const $4 = state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3;
                        return more(v2$3 => more(v3$1 => {
                          const state2$p$1 = state1._3 && !$4._3 ? Parsing.$ParseState($4._1, $4._2, true) : $4;
                          return $2(
                            state2$p$1,
                            more,
                            lift1,
                            (v2$4, $5) => $$throw(Parsing.$ParseState(v2$4._1, v2$4._2, $3), $5),
                            (state3$1, a$2) => more(v4$1 => {
                              const $5 = state2$p$1._3 && !state3$1._3 ? Parsing.$ParseState(state3$1._1, state3$1._2, true) : state3$1;
                              return more(v2$4 => {
                                const $6 = state1._3 && !$5._3 ? Parsing.$ParseState($5._1, $5._2, true) : $5;
                                return more(v1$3 => Parse$dParser.commas1(pattern)(
                                  $6,
                                  more,
                                  lift1,
                                  $$throw,
                                  (state2$1, a$3) => more(v2$5 => {
                                    const $7 = Parse$dParser.delim(Parse$dParser.parseableChar)(")");
                                    const $8 = $6._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1;
                                    return more(v1$4 => $7(
                                      $8,
                                      more,
                                      lift1,
                                      $$throw,
                                      (state2$2, a$4) => more(v2$6 => {
                                        const $9 = Parse$dParser.delim(Parse$dParser.parseableChar)(":");
                                        const $10 = $8._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2;
                                        return more(v1$5 => $9(
                                          $10,
                                          more,
                                          lift1,
                                          $$throw,
                                          (state2$3, a$5) => more(v2$7 => {
                                            const $11 = $10._3 && !state2$3._3 ? Parsing.$ParseState(state2$3._1, state2$3._2, true) : state2$3;
                                            return more(v1$6 => opTree$lazy()(
                                              $11,
                                              more,
                                              lift1,
                                              $$throw,
                                              (state2$4, a$6) => more(v2$8 => {
                                                const $12 = Parse$dParser.delim(Parse$dParser.parseableChar)(";");
                                                const $13 = $11._3 && !state2$4._3 ? Parsing.$ParseState(state2$4._1, state2$4._2, true) : state2$4;
                                                return more(v1$7 => $12(
                                                  $13,
                                                  more,
                                                  lift1,
                                                  $$throw,
                                                  (state2$5, a$7) => more(v2$9 => done(
                                                    $13._3 && !state2$5._3 ? Parsing.$ParseState(state2$5._1, state2$5._2, true) : state2$5,
                                                    Data$dTuple.$Tuple(a$1, Data$dTuple.$Tuple(a$3, a$6))
                                                  ))
                                                ));
                                              })
                                            ));
                                          })
                                        ));
                                      })
                                    ));
                                  })
                                ));
                              });
                            })
                          );
                        }));
                      })
                    );
                  }))
                )))));
              });
            })());
            return (state1, more, lift1, $$throw, done) => more(v1 => $1(
              state1,
              more,
              lift1,
              $$throw,
              (state2, a) => more(v2 => {
                const $2 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
                return more(v1$1 => opTree$lazy()(
                  $2,
                  more,
                  lift1,
                  $$throw,
                  (state2$1, a$1) => more(v2$1 => done($2._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1, SExpr.$Expr("LetRec", a, a$1)))
                ));
              })
            ));
          })());
          const $2 = Parse$dParser.context("matrix")((() => {
            const $2 = Parse$dParser.delim(Parse$dParser.parseableString)("[|");
            return (state1, more, lift1, $$throw, done) => more(v1 => $2(
              state1,
              more,
              lift1,
              $$throw,
              (state2, a) => more(v2 => {
                const $3 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
                return more(v1$1 => opTree$lazy()(
                  $3,
                  more,
                  lift1,
                  $$throw,
                  (state2$1, a$1) => more(v2$1 => {
                    const $4 = Parse$dParser.reserved("for");
                    const $5 = $3._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1;
                    return more(v1$2 => $4(
                      $5,
                      more,
                      lift1,
                      $$throw,
                      (state2$2, a$2) => more(v2$2 => {
                        const $6 = Parse$dParser.delim(Parse$dParser.parseableChar)("(");
                        const $7 = $5._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2;
                        return more(v1$3 => $6(
                          $7,
                          more,
                          lift1,
                          $$throw,
                          (state2$3, a$3) => more(v2$3 => {
                            const $8 = $7._3 && !state2$3._3 ? Parsing.$ParseState(state2$3._1, state2$3._2, true) : state2$3;
                            return more(v1$4 => Parse$dParser.variable(
                              $8,
                              more,
                              lift1,
                              $$throw,
                              (state2$4, a$4) => more(v2$4 => {
                                const $9 = Parse$dParser.delim(Parse$dParser.parseableChar)(",");
                                const $10 = $8._3 && !state2$4._3 ? Parsing.$ParseState(state2$4._1, state2$4._2, true) : state2$4;
                                return more(v1$5 => $9(
                                  $10,
                                  more,
                                  lift1,
                                  $$throw,
                                  (state2$5, a$5) => more(v2$5 => {
                                    const $11 = $10._3 && !state2$5._3 ? Parsing.$ParseState(state2$5._1, state2$5._2, true) : state2$5;
                                    return more(v1$6 => Parse$dParser.variable(
                                      $11,
                                      more,
                                      lift1,
                                      $$throw,
                                      (state2$6, a$6) => more(v2$6 => {
                                        const $12 = Parse$dParser.delim(Parse$dParser.parseableChar)(")");
                                        const $13 = $11._3 && !state2$6._3 ? Parsing.$ParseState(state2$6._1, state2$6._2, true) : state2$6;
                                        return more(v1$7 => $12(
                                          $13,
                                          more,
                                          lift1,
                                          $$throw,
                                          (state2$7, a$7) => more(v2$7 => {
                                            const $14 = Parse$dParser.reserved("in");
                                            const $15 = $13._3 && !state2$7._3 ? Parsing.$ParseState(state2$7._1, state2$7._2, true) : state2$7;
                                            return more(v1$8 => $14(
                                              $15,
                                              more,
                                              lift1,
                                              $$throw,
                                              (state2$8, a$8) => more(v2$8 => {
                                                const $16 = $15._3 && !state2$8._3 ? Parsing.$ParseState(state2$8._1, state2$8._2, true) : state2$8;
                                                return more(v1$9 => opTree$lazy()(
                                                  $16,
                                                  more,
                                                  lift1,
                                                  $$throw,
                                                  (state2$9, a$9) => more(v2$9 => {
                                                    const $17 = Parse$dParser.delim(Parse$dParser.parseableString)("|]");
                                                    const $18 = $16._3 && !state2$9._3 ? Parsing.$ParseState(state2$9._1, state2$9._2, true) : state2$9;
                                                    return more(v1$10 => $17(
                                                      $18,
                                                      more,
                                                      lift1,
                                                      $$throw,
                                                      (state2$10, a$10) => more(v2$10 => done(
                                                        $18._3 && !state2$10._3 ? Parsing.$ParseState(state2$10._1, state2$10._2, true) : state2$10,
                                                        SExpr.$Expr("Matrix", undefined, a$1, Data$dTuple.$Tuple(a$4, a$6), a$9)
                                                      ))
                                                    ));
                                                  })
                                                ));
                                              })
                                            ));
                                          })
                                        ));
                                      })
                                    ));
                                  })
                                ));
                              })
                            ));
                          })
                        ));
                      })
                    ));
                  })
                ));
              })
            ));
          })());
          const $3 = Parse$dParser.context("brackets")((() => {
            const $3 = Parse$dParser.delim(Parse$dParser.parseableChar)("[");
            return (state1, more, lift1, $$throw, done) => more(v1 => $3(
              state1,
              more,
              lift1,
              $$throw,
              (state2, a) => more(v2 => choice([
                (() => {
                  const $4 = Parse$dParser.close(Parse$dParser.parseableChar)("]");
                  return (state1$1, more$1, lift1$1, throw$1, done$1) => more$1(v1$1 => $4(
                    state1$1,
                    more$1,
                    lift1$1,
                    throw$1,
                    (state2$1, a$1) => more$1(v2$1 => done$1(
                      state1$1._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1,
                      SExpr.$Expr("ListEmpty", undefined)
                    ))
                  ));
                })(),
                (state1$1, more$1, lift1$1, throw$1, done$1) => more$1(v1$1 => opTree$lazy()(
                  state1$1,
                  more$1,
                  lift1$1,
                  throw$1,
                  (state2$1, a$1) => more$1(v2$1 => choice([
                    Parse$dParser.context("listNonEmpty")((() => {
                      const $4 = Parse$dParser.delim(Parse$dParser.parseableChar)(",");
                      return (state1$2, more$2, lift1$2, throw$2, done$2) => more$2(v1$2 => $4(
                        state1$2,
                        more$2,
                        lift1$2,
                        throw$2,
                        (state2$2, a$2) => more$2(v2$2 => {
                          const $5 = state1$2._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2;
                          return more$2(v1$3 => Parse$dParser.trailingCommas(opTree$lazy())(
                            $5,
                            more$2,
                            lift1$2,
                            throw$2,
                            (state2$3, a$3) => more$2(v2$3 => {
                              const $6 = Parse$dParser.close(Parse$dParser.parseableChar)("]");
                              const $7 = $5._3 && !state2$3._3 ? Parsing.$ParseState(state2$3._1, state2$3._2, true) : state2$3;
                              return more$2(v1$4 => $6(
                                $7,
                                more$2,
                                lift1$2,
                                throw$2,
                                (state2$4, a$4) => more$2(v2$4 => done$2(
                                  $7._3 && !state2$4._3 ? Parsing.$ParseState(state2$4._1, state2$4._2, true) : state2$4,
                                  SExpr.$Expr("ListNonEmpty", undefined, a$1, Data$dList$dTypes.foldableList.foldr(SExpr.Next())(SExpr.$ListRest("End", undefined))(a$3))
                                ))
                              ));
                            })
                          ));
                        })
                      ));
                    })()),
                    (() => {
                      const $4 = Parse$dParser.close(Parse$dParser.parseableChar)("]");
                      return (state1$2, more$2, lift1$2, throw$2, done$2) => more$2(v1$2 => $4(
                        state1$2,
                        more$2,
                        lift1$2,
                        throw$2,
                        (state2$2, a$2) => more$2(v2$2 => done$2(
                          state1$2._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2,
                          SExpr.$Expr("ListNonEmpty", undefined, a$1, SExpr.$ListRest("End", undefined))
                        ))
                      ));
                    })(),
                    Parse$dParser.context("listEnum")((() => {
                      const $4 = Parse$dParser.delim(Parse$dParser.parseableString)("..");
                      return (state1$2, more$2, lift1$2, throw$2, done$2) => more$2(v1$2 => $4(
                        state1$2,
                        more$2,
                        lift1$2,
                        throw$2,
                        (state2$2, a$2) => more$2(v2$2 => {
                          const $5 = state1$2._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2;
                          return more$2(v1$3 => opTree$lazy()(
                            $5,
                            more$2,
                            lift1$2,
                            throw$2,
                            (state2$3, a$3) => more$2(v2$3 => {
                              const $6 = Parse$dParser.close(Parse$dParser.parseableChar)("]");
                              const $7 = $5._3 && !state2$3._3 ? Parsing.$ParseState(state2$3._1, state2$3._2, true) : state2$3;
                              return more$2(v1$4 => $6(
                                $7,
                                more$2,
                                lift1$2,
                                throw$2,
                                (state2$4, a$4) => more$2(v2$4 => done$2(
                                  $7._3 && !state2$4._3 ? Parsing.$ParseState(state2$4._1, state2$4._2, true) : state2$4,
                                  SExpr.$Expr("ListEnum", a$1, a$3)
                                ))
                              ));
                            })
                          ));
                        })
                      ));
                    })()),
                    Parse$dParser.context("listComp")((() => {
                      const $4 = Parsing$dCombinators.many1(choice([
                        Parse$dParser.context("listCompGuard")((() => {
                          const $4 = Parse$dParser.reserved("if");
                          return (state1$2, more$2, lift1$2, throw$2, done$2) => more$2(v1$2 => $4(
                            state1$2,
                            more$2,
                            lift1$2,
                            throw$2,
                            (state2$2, a$2) => more$2(v2$2 => {
                              const $5 = state1$2._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2;
                              return more$2(v1$3 => opTree$lazy()(
                                $5,
                                more$2,
                                lift1$2,
                                throw$2,
                                (state2$3, a$3) => more$2(v2$3 => done$2(
                                  $5._3 && !state2$3._3 ? Parsing.$ParseState(state2$3._1, state2$3._2, true) : state2$3,
                                  SExpr.$Qualifier("ListCompGuard", a$3)
                                ))
                              ));
                            })
                          ));
                        })()),
                        Parse$dParser.context("listCompDecl")((() => {
                          const $4 = Parse$dParser.reserved("def");
                          return (state1$2, more$2, lift1$2, throw$2, done$2) => more$2(v1$2 => $4(
                            state1$2,
                            more$2,
                            lift1$2,
                            throw$2,
                            (state2$2, a$2) => more$2(v2$2 => {
                              const $5 = state1$2._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2;
                              return more$2(v1$3 => pattern(
                                $5,
                                more$2,
                                lift1$2,
                                throw$2,
                                (state2$3, a$3) => more$2(v2$3 => {
                                  const $6 = Parse$dParser.delim(Parse$dParser.parseableChar)(":");
                                  const $7 = $5._3 && !state2$3._3 ? Parsing.$ParseState(state2$3._1, state2$3._2, true) : state2$3;
                                  return more$2(v1$4 => $6(
                                    $7,
                                    more$2,
                                    lift1$2,
                                    throw$2,
                                    (state2$4, a$4) => more$2(v2$4 => {
                                      const $8 = $7._3 && !state2$4._3 ? Parsing.$ParseState(state2$4._1, state2$4._2, true) : state2$4;
                                      return more$2(v1$5 => opTree$lazy()(
                                        $8,
                                        more$2,
                                        lift1$2,
                                        throw$2,
                                        (state2$5, a$5) => more$2(v2$5 => done$2(
                                          $8._3 && !state2$5._3 ? Parsing.$ParseState(state2$5._1, state2$5._2, true) : state2$5,
                                          SExpr.$Qualifier("ListCompDecl", SExpr.$VarDef(a$3, a$5))
                                        ))
                                      ));
                                    })
                                  ));
                                })
                              ));
                            })
                          ));
                        })()),
                        Parse$dParser.context("listCompGen")((() => {
                          const $4 = Parse$dParser.reserved("for");
                          return (state1$2, more$2, lift1$2, throw$2, done$2) => more$2(v1$2 => $4(
                            state1$2,
                            more$2,
                            lift1$2,
                            throw$2,
                            (state2$2, a$2) => more$2(v2$2 => {
                              const $5 = state1$2._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2;
                              return more$2(v1$3 => pattern(
                                $5,
                                more$2,
                                lift1$2,
                                throw$2,
                                (state2$3, a$3) => more$2(v2$3 => {
                                  const $6 = Parse$dParser.reserved("in");
                                  const $7 = $5._3 && !state2$3._3 ? Parsing.$ParseState(state2$3._1, state2$3._2, true) : state2$3;
                                  return more$2(v1$4 => $6(
                                    $7,
                                    more$2,
                                    lift1$2,
                                    throw$2,
                                    (state2$4, a$4) => more$2(v2$4 => {
                                      const $8 = $7._3 && !state2$4._3 ? Parsing.$ParseState(state2$4._1, state2$4._2, true) : state2$4;
                                      return more$2(v1$5 => opTree$lazy()(
                                        $8,
                                        more$2,
                                        lift1$2,
                                        throw$2,
                                        (state2$5, a$5) => more$2(v2$5 => done$2(
                                          $8._3 && !state2$5._3 ? Parsing.$ParseState(state2$5._1, state2$5._2, true) : state2$5,
                                          SExpr.$Qualifier("ListCompGen", a$3, a$5)
                                        ))
                                      ));
                                    })
                                  ));
                                })
                              ));
                            })
                          ));
                        })())
                      ]));
                      return (state1$2, more$2, lift1$2, throw$2, done$2) => more$2(v1$2 => $4(
                        state1$2,
                        more$2,
                        lift1$2,
                        throw$2,
                        (state2$2, a$2) => more$2(v2$2 => {
                          const $5 = Parse$dParser.close(Parse$dParser.parseableChar)("]");
                          const $6 = state1$2._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2;
                          return more$2(v1$3 => $5(
                            $6,
                            more$2,
                            lift1$2,
                            throw$2,
                            (state2$3, a$3) => more$2(v2$3 => done$2(
                              $6._3 && !state2$3._3 ? Parsing.$ParseState(state2$3._1, state2$3._2, true) : state2$3,
                              SExpr.$Expr("ListComp", undefined, a$1, Data$dList$dTypes.$List("Cons", a$2._1, a$2._2))
                            ))
                          ));
                        })
                      ));
                    })()),
                    Parsing.fail("Expected `]")
                  ])(state1$1._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1, more$1, lift1$1, throw$1, done$1))
                )),
                Parsing.fail("Expected `]` or a list expression after `[`")
              ])(state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2, more, lift1, $$throw, done))
            ));
          })());
          const $4 = Parse$dParser.context("lambda")((() => {
            const $4 = Parse$dParser.reserved("lambda");
            return (state1, more, lift1, $$throw, done) => more(v1 => $4(
              state1,
              more,
              lift1,
              $$throw,
              (state2, a) => more(v2 => {
                const $5 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
                return more(v1$1 => Parse$dParser.commas1(pattern)(
                  $5,
                  more,
                  lift1,
                  $$throw,
                  (state2$1, a$1) => more(v2$1 => {
                    const $6 = Parse$dParser.delim(Parse$dParser.parseableChar)(":");
                    const $7 = $5._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1;
                    return more(v1$2 => $6(
                      $7,
                      more,
                      lift1,
                      $$throw,
                      (state2$2, a$2) => more(v2$2 => {
                        const $8 = $7._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2;
                        return more(v1$3 => opTree$lazy()(
                          $8,
                          more,
                          lift1,
                          $$throw,
                          (state2$3, a$3) => more(v2$3 => done(
                            $8._3 && !state2$3._3 ? Parsing.$ParseState(state2$3._1, state2$3._2, true) : state2$3,
                            SExpr.$Expr("Lambda", Util.nonEmptyListNonEmptyList.nonEmpty(Data$dList$dTypes.$List("Cons", Data$dTuple.$Tuple(a$1, a$3), Data$dList$dTypes.Nil)))
                          ))
                        ));
                      })
                    ));
                  })
                ));
              })
            ));
          })());
          const $5 = SExpr.VarKey();
          const exprKey = Parsing.lazyParserT.defer(v => (state1, more, lift1, $$throw, done) => more(v1 => Parse$dParser.brackets(opTree$lazy())(
            state1,
            more,
            lift1,
            $$throw,
            (state2, a) => more(v2 => done(state2, SExpr.$DictEntry("ExprKey", a)))
          )));
          const $6 = Parse$dParser.context("dict")((() => {
            const $6 = Parse$dParser.delim(Parse$dParser.parseableChar)("{");
            return (state1, more, lift1, $$throw, done) => more(v1 => $6(
              state1,
              more,
              lift1,
              $$throw,
              (state2, a) => more(v2 => {
                const $7 = Parse$dParser.fields((v2$1, $7, $8, $9, $10) => {
                  const $11 = v2$1._1;
                  const $12 = v2$1._2;
                  return $7(v3 => exprKey(
                    Parsing.$ParseState($11, $12, false),
                    $7,
                    $8,
                    (v4, $13) => {
                      const $14 = v4._3;
                      return $7(v5 => {
                        if ($14) { return $9(v4, $13); }
                        return $7(v1$1 => Parse$dParser.variable(v2$1, $7, $8, $9, (state2$1, a$1) => $7(v2$2 => $10(state2$1, $5(a$1)))));
                      });
                    },
                    $10
                  ));
                })(expr$lazy());
                const $8 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
                return more(v1$1 => $7(
                  $8,
                  more,
                  lift1,
                  $$throw,
                  (state2$1, a$1) => more(v2$1 => {
                    const $9 = Parse$dParser.close(Parse$dParser.parseableChar)("}");
                    const $10 = $8._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1;
                    return more(v1$2 => $9(
                      $10,
                      more,
                      lift1,
                      $$throw,
                      (state2$2, a$2) => more(v2$2 => done(
                        $10._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2,
                        SExpr.$Expr("Dictionary", undefined, a$1)
                      ))
                    ));
                  })
                ));
              })
            ));
          })());
          const $7 = (() => {
            const $7 = Data$dArray.some(Parsing.alternativeParserT)(Parsing.lazyParserT)(Parsing$dString.satisfy(c => c !== "\"" && c !== "{" && !Data$dCodePoint$dUnicode.isSpace(Data$dEnum.toCharCode(c))));
            const $8 = (() => {
              const $8 = Parsing.lazyParserT.defer(v => (state1, more, lift1, $$throw, done) => more(v1 => Parse$dParser.braces(opTree$lazy())(
                state1,
                more,
                lift1,
                $$throw,
                (state2, a) => more(v2 => done(state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2, SExpr.$ParagraphElem("Unquote", a)))
              )));
              const $9 = (() => {
                const $9 = Parse$dParser.delim(Parse$dParser.parseableString)("f\"\"\"");
                const $10 = (() => {
                  const $10 = SExpr.Str();
                  const $11 = (() => {
                    const $11 = SExpr.Constr();
                    const $12 = Parse$dParser.context("parens")((() => {
                      const $12 = Parse$dParser.delim(Parse$dParser.parseableChar)("(");
                      return (state1, more, lift1, $$throw, done) => more(v1 => $12(
                        state1,
                        more,
                        lift1,
                        $$throw,
                        (state2, a) => more(v2 => choice([
                          (() => {
                            const $13 = Parse$dParser.close(Parse$dParser.parseableChar)(")");
                            return (state1$1, more$1, lift1$1, throw$1, done$1) => more$1(v1$1 => {
                              const $14 = state1$1._3;
                              return more$1(v2$1 => more$1(v1$2 => Parse$dParser.operator(
                                state1$1,
                                more$1,
                                lift1$1,
                                (v2$2, $15) => throw$1(Parsing.$ParseState(v2$2._1, v2$2._2, $14), $15),
                                (state2$1, a$1) => more$1(v2$2 => more$1(v3 => {
                                  const state2$p = state1$1._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1;
                                  return $13(
                                    state2$p,
                                    more$1,
                                    lift1$1,
                                    (v2$3, $15) => throw$1(Parsing.$ParseState(v2$3._1, v2$3._2, $14), $15),
                                    (state3, a$2) => more$1(v4 => {
                                      const $15 = state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3;
                                      return more$1(v2$3 => done$1(state1$1._3 && !$15._3 ? Parsing.$ParseState($15._1, $15._2, true) : $15, SExpr.$Expr("Op", a$1)));
                                    })
                                  );
                                }))
                              )));
                            });
                          })(),
                          (state1$1, more$1, lift1$1, throw$1, done$1) => more$1(v1$1 => opTree$lazy()(
                            state1$1,
                            more$1,
                            lift1$1,
                            throw$1,
                            (state2$1, a$1) => more$1(v2$1 => choice([
                              (() => {
                                const $13 = Parse$dParser.close(Parse$dParser.parseableChar)(")");
                                return (state1$2, more$2, lift1$2, throw$2, done$2) => more$2(v1$2 => $13(
                                  state1$2,
                                  more$2,
                                  lift1$2,
                                  throw$2,
                                  (state2$2, a$2) => more$2(v2$2 => done$2(state1$2._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2, a$1))
                                ));
                              })(),
                              (() => {
                                const $13 = Parse$dParser.delim(Parse$dParser.parseableChar)(",");
                                return (state1$2, more$2, lift1$2, throw$2, done$2) => more$2(v1$2 => $13(
                                  state1$2,
                                  more$2,
                                  lift1$2,
                                  throw$2,
                                  (state2$2, a$2) => more$2(v2$2 => {
                                    const $14 = state1$2._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2;
                                    return more$2(v1$3 => opTree$lazy()(
                                      $14,
                                      more$2,
                                      lift1$2,
                                      throw$2,
                                      (state2$3, a$3) => more$2(v2$3 => {
                                        const $15 = Parse$dParser.close(Parse$dParser.parseableChar)(")");
                                        const $16 = $14._3 && !state2$3._3 ? Parsing.$ParseState(state2$3._1, state2$3._2, true) : state2$3;
                                        return more$2(v1$4 => $15(
                                          $16,
                                          more$2,
                                          lift1$2,
                                          throw$2,
                                          (state2$4, a$4) => more$2(v2$4 => done$2(
                                            $16._3 && !state2$4._3 ? Parsing.$ParseState(state2$4._1, state2$4._2, true) : state2$4,
                                            SExpr.$Expr(
                                              "Constr",
                                              undefined,
                                              "Pair",
                                              Data$dList$dTypes.$List("Cons", a$1, Data$dList$dTypes.$List("Cons", a$3, Data$dList$dTypes.Nil))
                                            )
                                          ))
                                        ));
                                      })
                                    ));
                                  })
                                ));
                              })(),
                              Parsing.fail("Expected `)` or `,` after `(expr`")
                            ])(state1$1._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1, more$1, lift1$1, throw$1, done$1))
                          )),
                          Parsing.fail("Expected `op` or `expr` after `(`")
                        ])(state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2, more, lift1, $$throw, done))
                      ));
                    })());
                    const $13 = Parse$dParser.context("doc expr")((() => {
                      const $13 = Parse$dParser.delim(Parse$dParser.parseableString)("@doc");
                      return (state1, more, lift1, $$throw, done) => more(v1 => $13(
                        state1,
                        more,
                        lift1,
                        $$throw,
                        (state2, a) => more(v2 => {
                          const $14 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
                          return more(v1$1 => Parse$dParser.parens(opTree$lazy())(
                            $14,
                            more,
                            lift1,
                            $$throw,
                            (state2$1, a$1) => more(v2$1 => {
                              const $15 = $14._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1;
                              return more(v1$2 => opTree$lazy()(
                                $15,
                                more,
                                lift1,
                                $$throw,
                                (state2$2, a$2) => more(v2$2 => done(
                                  $15._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2,
                                  SExpr.$Expr("DocExpr", a$1, a$2)
                                ))
                              ));
                            })
                          ));
                        })
                      ));
                    })());
                    const $14 = (() => {
                      const $14 = Parsing$dCombinators.withErrorMessage((() => {
                        const $14 = SExpr.Float();
                        const $15 = SExpr.Int();
                        return (v2, $16, $17, $18, $19) => {
                          const $20 = v2._1;
                          const $21 = v2._2;
                          return $16(v3 => $16(v1 => Parse$dNumber.float(
                            Parsing.$ParseState($20, $21, false),
                            $16,
                            $17,
                            (v2$1, $22) => $16(v5 => $16(v1$1 => Parse$dNumber.integer(v2, $16, $17, $18, (state2, a) => $16(v2$2 => $19(state2, $15(a)))))),
                            (state2, a) => $16(v2$1 => $19(state2, $14(a)))
                          )));
                        };
                      })())("simple expression");
                      return (v2, $15, $16, $17, $18) => {
                        const $19 = v2._1;
                        const $20 = v2._2;
                        return $15(v3 => {
                          const $21 = (v4, $21) => {
                            const $22 = v4._3;
                            return $15(v5 => {
                              if ($22) { return $17(v4, $21); }
                              const $23 = v2._1;
                              const $24 = v2._2;
                              return $15(v3$1 => $15(v1 => Parse$dParser.stringLiteral(
                                Parsing.$ParseState($23, $24, false),
                                $15,
                                $16,
                                (v4$1, $25) => {
                                  const $26 = v4$1._3;
                                  return $15(v5$1 => {
                                    if ($26) { return $17(v4$1, $25); }
                                    const $27 = v2._1;
                                    const $28 = v2._2;
                                    return $15(v3$2 => $15(v1$1 => Parse$dParser.variable(
                                      Parsing.$ParseState($27, $28, false),
                                      $15,
                                      $16,
                                      (v4$2, $29) => {
                                        const $30 = v4$2._3;
                                        return $15(v5$2 => {
                                          if ($30) { return $17(v4$2, $29); }
                                          const $31 = v2._1;
                                          const $32 = v2._2;
                                          return $15(v3$3 => $15(v1$2 => Parse$dParser.constructor(
                                            Parsing.$ParseState($31, $32, false),
                                            $15,
                                            $16,
                                            (v4$3, $33) => {
                                              const $34 = v4$3._3;
                                              return $15(v5$3 => {
                                                if ($34) { return $17(v4$3, $33); }
                                                const $35 = v2._1;
                                                const $36 = v2._2;
                                                return $15(v3$4 => $12(
                                                  Parsing.$ParseState($35, $36, false),
                                                  $15,
                                                  $16,
                                                  (v4$4, $37) => {
                                                    const $38 = v4$4._3;
                                                    return $15(v5$4 => {
                                                      if ($38) { return $17(v4$4, $37); }
                                                      const $39 = v2._1;
                                                      const $40 = v2._2;
                                                      return $15(v3$5 => $13(
                                                        Parsing.$ParseState($39, $40, false),
                                                        $15,
                                                        $16,
                                                        (v4$5, $41) => {
                                                          const $42 = v4$5._3;
                                                          return $15(v5$5 => {
                                                            if ($42) { return $17(v4$5, $41); }
                                                            return $14(v2, $15, $16, $17, $18);
                                                          });
                                                        },
                                                        $18
                                                      ));
                                                    });
                                                  },
                                                  $18
                                                ));
                                              });
                                            },
                                            (state2, a) => $15(v2$1 => $18(state2, $11(a)(Data$dList$dTypes.Nil)))
                                          )));
                                        });
                                      },
                                      (state2, a) => $15(v2$1 => $18(state2, SExpr.$Expr("Var", a)))
                                    )));
                                  });
                                },
                                (state2, a) => $15(v2$1 => $18(state2, $10(a)))
                              )));
                            });
                          };
                          return $15(v1 => $9(
                            Parsing.$ParseState($19, $20, false),
                            $15,
                            $16,
                            $21,
                            (state2, a) => $15(v2$1 => {
                              const $22 = Parsing$dCombinators.manyRec(Parse$dParser.lexeme((v2$2, $22, $23, $24, $25) => {
                                const $26 = v2$2._1;
                                const $27 = v2$2._2;
                                return $22(v3$1 => $22(v1$1 => $7(
                                  Parsing.$ParseState($26, $27, false),
                                  $22,
                                  $23,
                                  (v4, $28) => {
                                    const $29 = v4._3;
                                    return $22(v5 => {
                                      if ($29) { return $24(v4, $28); }
                                      return $8(v2$2, $22, $23, $24, $25);
                                    });
                                  },
                                  (state2$1, a$1) => $22(v2$3 => $25(state2$1, SExpr.$ParagraphElem("Token", Data$dString$dCodeUnits.fromCharArray(a$1))))
                                )));
                              }));
                              return $15(v1$1 => $22(
                                state2,
                                $15,
                                $16,
                                $21,
                                (state2$1, a$1) => $15(v2$2 => {
                                  const $23 = Parse$dParser.delim(Parse$dParser.parseableString)("\"\"\"");
                                  const $24 = state2._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1;
                                  return $15(v1$2 => $23(
                                    $24,
                                    $15,
                                    $16,
                                    $21,
                                    (state2$2, a$2) => $15(v2$3 => $18(
                                      $24._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2,
                                      SExpr.$Expr("Paragraph", a$1)
                                    ))
                                  ));
                                })
                              ));
                            })
                          ));
                        });
                      };
                    })();
                    return (v2, $15, $16, $17, $18) => {
                      const $19 = v2._1;
                      const $20 = v2._2;
                      return $15(v3 => $6(
                        Parsing.$ParseState($19, $20, false),
                        $15,
                        $16,
                        (v4, $21) => {
                          const $22 = v4._3;
                          return $15(v5 => {
                            if ($22) { return $17(v4, $21); }
                            return $14(v2, $15, $16, $17, $18);
                          });
                        },
                        $18
                      ));
                    };
                  })();
                  return (v2, $12, $13, $14, $15) => {
                    const $16 = v2._1;
                    const $17 = v2._2;
                    return $12(v3 => $4(
                      Parsing.$ParseState($16, $17, false),
                      $12,
                      $13,
                      (v4, $18) => {
                        const $19 = v4._3;
                        return $12(v5 => {
                          if ($19) { return $14(v4, $18); }
                          return $11(v2, $12, $13, $14, $15);
                        });
                      },
                      $15
                    ));
                  };
                })();
                return (v2, $11, $12, $13, $14) => {
                  const $15 = v2._1;
                  const $16 = v2._2;
                  return $11(v3 => $3(
                    Parsing.$ParseState($15, $16, false),
                    $11,
                    $12,
                    (v4, $17) => {
                      const $18 = v4._3;
                      return $11(v5 => {
                        if ($18) { return $13(v4, $17); }
                        return $10(v2, $11, $12, $13, $14);
                      });
                    },
                    $14
                  ));
                };
              })();
              return (v2, $10, $11, $12, $13) => {
                const $14 = v2._1;
                const $15 = v2._2;
                return $10(v3 => $2(
                  Parsing.$ParseState($14, $15, false),
                  $10,
                  $11,
                  (v4, $16) => {
                    const $17 = v4._3;
                    return $10(v5 => {
                      if ($17) { return $12(v4, $16); }
                      return $9(v2, $10, $11, $12, $13);
                    });
                  },
                  $13
                ));
              };
            })();
            return (v2, $9, $10, $11, $12) => {
              const $13 = v2._1;
              const $14 = v2._2;
              return $9(v3 => $1(
                Parsing.$ParseState($13, $14, false),
                $9,
                $10,
                (v4, $15) => {
                  const $16 = v4._3;
                  return $9(v5 => {
                    if ($16) { return $11(v4, $15); }
                    return $8(v2, $9, $10, $11, $12);
                  });
                },
                $12
              ));
            };
          })();
          return (v2, $8, $9, $10, $11) => {
            const $12 = v2._1;
            const $13 = v2._2;
            return $8(v3 => $0(
              Parsing.$ParseState($12, $13, false),
              $8,
              $9,
              (v4, $14) => {
                const $15 = v4._3;
                return $8(v5 => {
                  if ($15) { return $10(v4, $14); }
                  return $7(v2, $8, $9, $10, $11);
                });
              },
              $11
            ));
          };
        })());
        return (state1, more, lift1, $$throw, done) => more(v1 => $0(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more(v2 => chain(a)(state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2, more, lift1, $$throw, done))
        ));
      })()))(opTable);
    })());
    return (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => $0(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more(v2$1 => more(v3 => {
        const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
        return Parsing.consume(
          state2$p,
          more,
          lift1,
          $$throw,
          (state3, a$1) => more(v4 => done(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a))
        );
      }))
    )));
  });
  const opTree = opTree$lazy();
  return Parse$dParser.context("expr")((() => {
    const $0 = Parse$dParser.reserved("case");
    const $1 = Parse$dParser.reserved("match");
    const clause = (state1, more, lift1, $$throw, done) => more(v1 => opTree(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more(v2 => {
        const $2 = Parse$dParser.block(expr$lazy());
        const $3 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
        return more(v1$1 => $2(
          $3,
          more,
          lift1,
          $$throw,
          (state2$1, a$1) => more(v2$1 => done($3._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1, Data$dTuple.$Tuple(a, a$1)))
        ));
      })
    ));
    const $2 = Parse$dParser.reserved("if");
    const $3 = Parse$dParser.context("def")((() => {
      const $3 = Parse$dParser.context("funDef")(Parsing$dIndent.withPos((state1, more, lift1, $$throw, done) => more(v1 => recDefs$lazy()(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more(v2 => {
          const $3 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
          return more(v1$1 => Parse$dParser.align(expr$lazy())(
            $3,
            more,
            lift1,
            $$throw,
            (state2$1, a$1) => more(v2$1 => done($3._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1, SExpr.$Expr("LetRec", a, a$1)))
          ));
        })
      ))));
      const $4 = Parse$dParser.context("valDef")(Parsing$dIndent.withPos((state1, more, lift1, $$throw, done) => more(v1 => varDefs$lazy()(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more(v2 => {
          const $4 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
          return more(v1$1 => Parse$dParser.align(expr$lazy())(
            $4,
            more,
            lift1,
            $$throw,
            (state2$1, a$1) => more(v2$1 => done($4._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1, SExpr.$Expr("Let", a, a$1)))
          ));
        })
      ))));
      return (v2, $5, $6, $7, $8) => {
        const $9 = v2._1;
        const $10 = v2._2;
        return $5(v3 => $3(
          Parsing.$ParseState($9, $10, false),
          $5,
          $6,
          (v4, $11) => {
            const $12 = v4._3;
            return $5(v5 => {
              if ($12) { return $7(v4, $11); }
              return $4(v2, $5, $6, $7, $8);
            });
          },
          $8
        ));
      };
    })());
    const $4 = Parsing$dCombinators.withErrorMessage(opTree)("expression");
    return (v2, $5, $6, $7, $8) => {
      const $9 = v2._1;
      const $10 = v2._2;
      return $5(v3 => {
        const $11 = (v4, $11) => {
          const $12 = v4._3;
          return $5(v5 => {
            if ($12) { return $7(v4, $11); }
            const $13 = v2._1;
            const $14 = v2._2;
            return $5(v3$1 => {
              const $15 = (v4$1, $15) => {
                const $16 = v4$1._3;
                return $5(v5$1 => {
                  if ($16) { return $7(v4$1, $15); }
                  const $17 = v2._1;
                  const $18 = v2._2;
                  return $5(v3$2 => $3(
                    Parsing.$ParseState($17, $18, false),
                    $5,
                    $6,
                    (v4$2, $19) => {
                      const $20 = v4$2._3;
                      return $5(v5$2 => {
                        if ($20) { return $7(v4$2, $19); }
                        return $4(v2, $5, $6, $7, $8);
                      });
                    },
                    $8
                  ));
                });
              };
              return $5(v1 => $2(
                Parsing.$ParseState($13, $14, false),
                $5,
                $6,
                $15,
                (state2, a) => $5(v2$1 => $5(v1$1 => clause(
                  state2,
                  $5,
                  $6,
                  $15,
                  (state2$1, a$1) => $5(v2$2 => {
                    const $16 = Parsing$dCombinators.manyRec(Parse$dParser.align((() => {
                      const $16 = Parse$dParser.reserved("elif");
                      return (state1, more, lift1, $$throw, done) => more(v2$3 => more(v1$2 => $16(
                        state1,
                        more,
                        lift1,
                        $$throw,
                        (state2$2, a$2) => more(v2$4 => more(v3$2 => {
                          const state2$p = state1._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2;
                          return clause(
                            state2$p,
                            more,
                            lift1,
                            $$throw,
                            (state3, a$3) => more(v4$1 => done(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a$3))
                          );
                        }))
                      )));
                    })()));
                    const $17 = state2._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1;
                    return $5(v1$2 => $16(
                      $17,
                      $5,
                      $6,
                      $15,
                      (state2$2, a$2) => $5(v2$3 => {
                        const $18 = Parse$dParser.align((() => {
                          const $18 = Parse$dParser.reserved("else");
                          const $19 = Parse$dParser.block(expr$lazy());
                          return (state1, more, lift1, $$throw, done) => more(v2$4 => more(v1$3 => $18(
                            state1,
                            more,
                            lift1,
                            $$throw,
                            (state2$3, a$3) => more(v2$5 => more(v3$2 => {
                              const state2$p = state1._3 && !state2$3._3 ? Parsing.$ParseState(state2$3._1, state2$3._2, true) : state2$3;
                              return $19(
                                state2$p,
                                more,
                                lift1,
                                $$throw,
                                (state3, a$4) => more(v4$1 => done(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a$4))
                              );
                            }))
                          )));
                        })());
                        const $19 = $17._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2;
                        return $5(v1$3 => $18(
                          $19,
                          $5,
                          $6,
                          $15,
                          (state2$3, a$3) => $5(v2$4 => $8(
                            $19._3 && !state2$3._3 ? Parsing.$ParseState(state2$3._1, state2$3._2, true) : state2$3,
                            SExpr.$Expr("IfElse", Util.nonEmptyListNonEmptyList.nonEmpty(Data$dList$dTypes.$List("Cons", a$1, a$2)), a$3)
                          ))
                        ));
                      })
                    ));
                  })
                )))
              ));
            });
          });
        };
        return $5(v1 => $1(
          Parsing.$ParseState($9, $10, false),
          $5,
          $6,
          $11,
          (state2, a) => $5(v2$1 => $5(v1$1 => opTree(
            state2,
            $5,
            $6,
            $11,
            (state2$1, a$1) => $5(v2$2 => {
              const $12 = Parse$dParser.block(Parsing$dCombinators.many1(Parse$dParser.align((state1, more, lift1, $$throw, done) => more(v1$2 => $0(
                state1,
                more,
                lift1,
                $$throw,
                (state2$2, a$2) => more(v2$3 => {
                  const $12 = state1._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2;
                  return more(v1$3 => pattern(
                    $12,
                    more,
                    lift1,
                    $$throw,
                    (state2$3, a$3) => more(v2$4 => {
                      const $13 = Parse$dParser.block(expr$lazy());
                      const $14 = $12._3 && !state2$3._3 ? Parsing.$ParseState(state2$3._1, state2$3._2, true) : state2$3;
                      return more(v1$4 => $13(
                        $14,
                        more,
                        lift1,
                        $$throw,
                        (state2$4, a$4) => more(v2$5 => done($14._3 && !state2$4._3 ? Parsing.$ParseState(state2$4._1, state2$4._2, true) : state2$4, Data$dTuple.$Tuple(a$3, a$4)))
                      ));
                    })
                  ));
                })
              )))));
              const $13 = state2._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1;
              return $5(v1$2 => $12(
                $13,
                $5,
                $6,
                $11,
                (state2$2, a$2) => $5(v2$3 => $8($13._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2, SExpr.$Expr("MatchAs", a$1, a$2)))
              ));
            })
          )))
        ));
      });
    };
  })());
});
const varDefs = /* #__PURE__ */ varDefs$lazy();
const recDefs = /* #__PURE__ */ recDefs$lazy();
const expr = /* #__PURE__ */ expr$lazy();
const parseProgram = /* #__PURE__ */ parse(/* #__PURE__ */ withImports(expr));
const defs = /* #__PURE__ */ Data$dEither.choose(Parsing.altParserT)(varDefs)(recDefs);
const module_ = /* #__PURE__ */ (() => {
  const $0 = Parsing$dCombinators.manyRec(defs);
  return (state1, more, lift1, $$throw, done) => more(v1 => $0(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2 => done(state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2, SExpr.$Module(a)))
  ));
})();
const parseModule = /* #__PURE__ */ parse(/* #__PURE__ */ withImports(module_));
export {
  
  
  
  expr,
  
  
  
  
  module_,
  
  
  parse,
  
  
  
  
  
  
  
  
  withImports
};
