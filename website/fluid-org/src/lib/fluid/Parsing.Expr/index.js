// | This module is a port of the Haskell
// | [__Text.Parsec.Expr__](https://hackage.haskell.org/package/parsec/docs/Text-Parsec-Expr.html)
// | module.
import * as $runtime from "../runtime.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Parsing$dCombinators from "../Parsing.Combinators/index.js";
const $Assoc = tag => tag;
const $Operator = (tag, _1, _2) => ({tag, _1, _2});
const choice = /* #__PURE__ */ Parsing$dCombinators.choice(Data$dList$dTypes.foldableList);
const identity = x => x;
const AssocNone = /* #__PURE__ */ $Assoc("AssocNone");
const AssocLeft = /* #__PURE__ */ $Assoc("AssocLeft");
const AssocRight = /* #__PURE__ */ $Assoc("AssocRight");
const Infix = value0 => value1 => $Operator("Infix", value0, value1);
const Prefix = value0 => $Operator("Prefix", value0);
const Postfix = value0 => $Operator("Postfix", value0);
const termP = prefixP => term => postfixP => (state1, more, lift1, $$throw, done) => more(v1 => prefixP(
  state1,
  more,
  lift1,
  $$throw,
  (state2, a) => more(v2 => {
    const $0 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
    return more(v1$1 => term(
      $0,
      more,
      lift1,
      $$throw,
      (state2$1, a$1) => more(v2$1 => {
        const $1 = $0._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1;
        return more(v1$2 => postfixP(
          $1,
          more,
          lift1,
          $$throw,
          (state2$2, a$2) => more(v2$2 => done($1._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2, a$2(a(a$1))))
        ));
      })
    ));
  })
));
const splitOp = v => v1 => {
  if (v.tag === "Infix") {
    if (v._2 === "AssocNone") { return {rassoc: v1.rassoc, lassoc: v1.lassoc, nassoc: Data$dList$dTypes.$List("Cons", v._1, v1.nassoc), prefix: v1.prefix, postfix: v1.postfix}; }
    if (v._2 === "AssocLeft") { return {rassoc: v1.rassoc, lassoc: Data$dList$dTypes.$List("Cons", v._1, v1.lassoc), nassoc: v1.nassoc, prefix: v1.prefix, postfix: v1.postfix}; }
    if (v._2 === "AssocRight") { return {rassoc: Data$dList$dTypes.$List("Cons", v._1, v1.rassoc), lassoc: v1.lassoc, nassoc: v1.nassoc, prefix: v1.prefix, postfix: v1.postfix}; }
    $runtime.fail();
  }
  if (v.tag === "Prefix") { return {rassoc: v1.rassoc, lassoc: v1.lassoc, nassoc: v1.nassoc, prefix: Data$dList$dTypes.$List("Cons", v._1, v1.prefix), postfix: v1.postfix}; }
  if (v.tag === "Postfix") { return {rassoc: v1.rassoc, lassoc: v1.lassoc, nassoc: v1.nassoc, prefix: v1.prefix, postfix: Data$dList$dTypes.$List("Cons", v._1, v1.postfix)}; }
  $runtime.fail();
};
const rassocP1 = x => rassocOp => prefixP => term => postfixP => {
  const $0 = rassocP(x)(rassocOp)(prefixP)(term)(postfixP);
  return (v2, $1, $2, $3, $4) => {
    const $5 = v2._1;
    const $6 = v2._2;
    return $1(v3 => $0(
      Parsing.$ParseState($5, $6, false),
      $1,
      $2,
      (v4, $7) => {
        const $8 = v4._3;
        return $1(v5 => {
          if ($8) { return $3(v4, $7); }
          return $4(v2, x);
        });
      },
      $4
    ));
  };
};
const rassocP = x => rassocOp => prefixP => term => postfixP => (state1, more, lift1, $$throw, done) => more(v1 => rassocOp(
  state1,
  more,
  lift1,
  $$throw,
  (state2, a) => more(v2 => {
    const $0 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
    return more(v1$1 => more(v1$2 => termP(prefixP)(term)(postfixP)(
      $0,
      more,
      lift1,
      $$throw,
      (state2$1, a$1) => more(v2$1 => rassocP1(a$1)(rassocOp)(prefixP)(term)(postfixP)(
        $0._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1,
        more,
        lift1,
        $$throw,
        (state2$2, a$2) => more(v2$2 => done($0._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2, a(x)(a$2)))
      ))
    )));
  })
));
const nassocP = x => nassocOp => prefixP => term => postfixP => (state1, more, lift1, $$throw, done) => more(v1 => nassocOp(
  state1,
  more,
  lift1,
  $$throw,
  (state2, a) => more(v2 => {
    const $0 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
    return more(v1$1 => termP(prefixP)(term)(postfixP)(
      $0,
      more,
      lift1,
      $$throw,
      (state2$1, a$1) => more(v2$1 => done($0._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1, a(x)(a$1)))
    ));
  })
));
const lassocP1 = x => lassocOp => prefixP => term => postfixP => {
  const $0 = lassocP(x)(lassocOp)(prefixP)(term)(postfixP);
  return (v2, $1, $2, $3, $4) => {
    const $5 = v2._1;
    const $6 = v2._2;
    return $1(v3 => $0(
      Parsing.$ParseState($5, $6, false),
      $1,
      $2,
      (v4, $7) => {
        const $8 = v4._3;
        return $1(v5 => {
          if ($8) { return $3(v4, $7); }
          return $4(v2, x);
        });
      },
      $4
    ));
  };
};
const lassocP = x => lassocOp => prefixP => term => postfixP => (state1, more, lift1, $$throw, done) => more(v1 => lassocOp(
  state1,
  more,
  lift1,
  $$throw,
  (state2, a) => more(v2 => {
    const $0 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
    return more(v1$1 => termP(prefixP)(term)(postfixP)(
      $0,
      more,
      lift1,
      $$throw,
      (state2$1, a$1) => more(v2$1 => lassocP1(a(x)(a$1))(lassocOp)(prefixP)(term)(postfixP)(
        $0._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1,
        more,
        lift1,
        $$throw,
        done
      ))
    ));
  })
));
const makeParser = term => ops => {
  const accum = Data$dFoldable.foldrArray(splitOp)({
    rassoc: Data$dList$dTypes.Nil,
    lassoc: Data$dList$dTypes.Nil,
    nassoc: Data$dList$dTypes.Nil,
    prefix: Data$dList$dTypes.Nil,
    postfix: Data$dList$dTypes.Nil
  })(ops);
  const lassocOp = choice(accum.lassoc);
  const nassocOp = choice(accum.nassoc);
  const postfixP$lazy = $runtime.binding(() => {
    const $0 = Parsing$dCombinators.withErrorMessage(choice(accum.postfix))("");
    return (v2, $1, $2, $3, $4) => {
      const $5 = v2._1;
      const $6 = v2._2;
      return $1(v3 => {
        const $7 = (v4, $7) => {
          const $8 = v4._3;
          return $1(v5 => {
            if ($8) { return $3(v4, $7); }
            return $4(v2, identity);
          });
        };
        return $1(v1 => $0(
          Parsing.$ParseState($5, $6, false),
          $1,
          $2,
          $7,
          (state2, a) => $1(v2$1 => $1(v1$1 => postfixP$lazy()(
            state2,
            $1,
            $2,
            $7,
            (state2$1, a$1) => $1(v2$2 => $4(state2._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1, x => a$1(a(x))))
          )))
        ));
      });
    };
  });
  const postfixP = postfixP$lazy();
  const prefixP$lazy = $runtime.binding(() => {
    const $0 = Parsing$dCombinators.withErrorMessage(choice(accum.prefix))("");
    return (v2, $1, $2, $3, $4) => {
      const $5 = v2._1;
      const $6 = v2._2;
      return $1(v3 => {
        const $7 = (v4, $7) => {
          const $8 = v4._3;
          return $1(v5 => {
            if ($8) { return $3(v4, $7); }
            return $4(v2, identity);
          });
        };
        return $1(v1 => $0(
          Parsing.$ParseState($5, $6, false),
          $1,
          $2,
          $7,
          (state2, a) => $1(v2$1 => $1(v1$1 => prefixP$lazy()(
            state2,
            $1,
            $2,
            $7,
            (state2$1, a$1) => $1(v2$2 => $4(state2._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1, x => a(a$1(x))))
          )))
        ));
      });
    };
  });
  const prefixP = prefixP$lazy();
  const rassocOp = choice(accum.rassoc);
  return (state1, more, lift1, $$throw, done) => more(v1 => termP(prefixP)(term)(postfixP)(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2 => {
      const $0 = Parsing$dCombinators.withErrorMessage((state1$1, v, v1$1, v2$1, done$1) => done$1(state1$1, a))("operator");
      const $1 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
      const $2 = $1._1;
      const $3 = $1._2;
      return more(v3 => rassocP(a)(rassocOp)(prefixP)(term)(postfixP)(
        Parsing.$ParseState($2, $3, false),
        more,
        lift1,
        (v4, $4) => {
          const $5 = v4._3;
          return more(v5 => {
            if ($5) { return $$throw(v4, $4); }
            const $6 = $1._1;
            const $7 = $1._2;
            return more(v3$1 => lassocP(a)(lassocOp)(prefixP)(term)(postfixP)(
              Parsing.$ParseState($6, $7, false),
              more,
              lift1,
              (v4$1, $8) => {
                const $9 = v4$1._3;
                return more(v5$1 => {
                  if ($9) { return $$throw(v4$1, $8); }
                  const $10 = $1._1;
                  const $11 = $1._2;
                  return more(v3$2 => nassocP(a)(nassocOp)(prefixP)(term)(postfixP)(
                    Parsing.$ParseState($10, $11, false),
                    more,
                    lift1,
                    (v4$2, $12) => {
                      const $13 = v4$2._3;
                      return more(v5$2 => {
                        if ($13) { return $$throw(v4$2, $12); }
                        return $0($1, more, lift1, $$throw, done);
                      });
                    },
                    done
                  ));
                });
              },
              done
            ));
          });
        },
        done
      ));
    })
  ));
};
const buildExprParser = operators => simpleExpr => Data$dFoldable.foldlArray(makeParser)(simpleExpr)(operators);
export {
  
  $Operator,
  AssocLeft,
  AssocNone,
  AssocRight,
  
  
  
  
  
  
  
  
  makeParser,
  
  
  
  
  
};
