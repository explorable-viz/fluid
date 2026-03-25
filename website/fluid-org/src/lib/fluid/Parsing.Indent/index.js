// | This module is a port of the Haskell
// | [__Text.Parsec.Indent__](https://hackage.haskell.org/package/indents-0.3.3/docs/Text-Parsec-Indent.html)
// | module from 2016-05-07.
// |
// | A module to construct indentation aware parsers. Many programming
// | language have indentation based syntax rules e.g. python and Haskell.
// | This module exports combinators to create such parsers.
// |
// | The input source can be thought of as a list of tokens. Abstractly
// | each token occurs at a line and a column and has a width. The column
// | number of a token measures is indentation. If t1 and t2 are two tokens
// | then we say that indentation of t1 is more than t2 if the column
// | number of occurrence of t1 is greater than that of t2.
// |
// | Currently this module supports two kind of indentation based syntactic
// | structures which we now describe:
// |
// | - **Block**
// |
// |   A block of indentation /c/ is a sequence of tokens with
// |   indentation at least /c/.  Examples for a block is a where clause of
// |   Haskell with no explicit braces.
// |
// | - **Line fold**
// |
// |   A line fold starting at line /l/ and indentation /c/ is a
// |   sequence of tokens that start at line /l/ and possibly continue to
// |   subsequent lines as long as the indentation is greater than /c/. Such
// |   a sequence of lines need to be /folded/ to a single line. An example
// |   is MIME headers. Line folding based binding separation is used in
// |   Haskell as well.
import * as $runtime from "../runtime.js";
import * as Control$dMonad$dState$dTrans from "../Control.Monad.State.Trans/index.js";
import * as Data$dIdentity from "../Data.Identity/index.js";
import * as Data$dList from "../Data.List/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Parsing$dString from "../Parsing.String/index.js";
import * as Parsing$dString$dBasic from "../Parsing.String.Basic/index.js";
const $Optional = (_1, _2) => ({tag: "Opt", _1, _2});
const lift = m => (state1, v, lift$p, v1, done) => lift$p(Control$dMonad$dState$dTrans.bindStateT(Data$dIdentity.monadIdentity).Apply0().Functor0().map(a => v2 => done(state1, a))(m));
const monadStateStateT = /* #__PURE__ */ Control$dMonad$dState$dTrans.monadStateStateT(Data$dIdentity.monadIdentity);
const identity = x => x;
const Opt = value0 => value1 => $Optional(value0, value1);
const symbol = name => {
  const $0 = Data$dList.many(Parsing.alternativeParserT)(Parsing.lazyParserT)(Parsing$dString$dBasic.oneOf([" ", "\t"]));
  return (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => $0(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2$1 => more(v3 => {
      const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
      return Parsing$dString.string(name)(
        state2$p,
        more,
        lift1,
        $$throw,
        (state3, a$1) => more(v4 => done(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a$1))
      );
    }))
  )));
};
const runIndent = a => a(Parsing.initialPos)._1;
const put$p = p => lift(monadStateStateT.state(v => Data$dTuple.$Tuple(undefined, p)));
const many1 = p => {
  const $0 = Data$dList.many(Parsing.alternativeParserT)(Parsing.lazyParserT)(p);
  return (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => p(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2$1 => {
      const $1 = Data$dList$dTypes.Cons(a);
      return more(v3 => {
        const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
        return $0(state2$p, more, lift1, $$throw, (state3, a$1) => more(v4 => done(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, $1(a$1))));
      });
    })
  )));
};
const get$p = /* #__PURE__ */ (() => {
  const $0 = lift(monadStateStateT.state(s => Data$dTuple.$Tuple(s, s)));
  return (state1, more, lift1, $$throw, done) => more(v1 => $0(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2 => done(state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2, a))
  ));
})();
const indented = (state1, more, lift1, $$throw, done) => more(v1 => Parsing.position(
  state1,
  more,
  lift1,
  $$throw,
  (state2, a) => more(v2 => {
    const $0 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
    return more(v1$1 => get$p(
      $0,
      more,
      lift1,
      $$throw,
      (state2$1, a$1) => more(v2$1 => (a.column <= a$1.column ? Parsing.fail("not indented") : put$p({index: 0, line: a.line, column: a$1.column}))(
        $0._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1,
        more,
        lift1,
        $$throw,
        done
      ))
    ));
  })
));
const indented$p = (state1, more, lift1, $$throw, done) => more(v1 => Parsing.position(
  state1,
  more,
  lift1,
  $$throw,
  (state2, a) => more(v2 => {
    const $0 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
    return more(v1$1 => get$p(
      $0,
      more,
      lift1,
      $$throw,
      (state2$1, a$1) => more(v2$1 => (a.column <= a$1.column ? Parsing.fail("not indented") : (state1$1, v, v1$2, v2$2, done$1) => done$1(state1$1, undefined))(
        $0._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1,
        more,
        lift1,
        $$throw,
        done
      ))
    ));
  })
));
const sameLine = (state1, more, lift1, $$throw, done) => more(v1 => Parsing.position(
  state1,
  more,
  lift1,
  $$throw,
  (state2, a) => more(v2 => {
    const $0 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
    return more(v1$1 => get$p(
      $0,
      more,
      lift1,
      $$throw,
      (state2$1, a$1) => more(v2$1 => (a.line === a$1.line ? ((state1$1, v, v1$2, v2$2, done$1) => done$1(state1$1, undefined)) : Parsing.fail("over one line"))(
        $0._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1,
        more,
        lift1,
        $$throw,
        done
      ))
    ));
  })
));
const sameOrIndented = (v2, $0, $1, $2, $3) => {
  const $4 = v2._1;
  const $5 = v2._2;
  return $0(v3 => sameLine(
    Parsing.$ParseState($4, $5, false),
    $0,
    $1,
    (v4, $6) => {
      const $7 = v4._3;
      return $0(v5 => {
        if ($7) { return $2(v4, $6); }
        return indented(v2, $0, $1, $2, $3);
      });
    },
    $3
  ));
};
const indentAp = a => b => (state1, more, lift1, $$throw, done) => more(v1 => a(
  state1,
  more,
  lift1,
  $$throw,
  (state2, a$1) => more(v2 => {
    const $0 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
    return more(v1$1 => more(v2$1 => more(v1$2 => sameOrIndented(
      $0,
      more,
      lift1,
      $$throw,
      (state2$1, a$2) => more(v2$2 => more(v3 => {
        const state2$p = $0._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1;
        return b(
          state2$p,
          more,
          lift1,
          $$throw,
          (state3, a$3) => more(v4 => {
            const $1 = state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3;
            return more(v2$3 => done($0._3 && !$1._3 ? Parsing.$ParseState($1._1, $1._2, true) : $1, a$1(a$3)));
          })
        );
      }))
    ))));
  })
));
const indentMany = a => b => {
  const $0 = Data$dList.many(Parsing.alternativeParserT)(Parsing.lazyParserT)((state1, more, lift1, $$throw, done) => more(v2 => more(v1 => sameOrIndented(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a$1) => more(v2$1 => more(v3 => {
      const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
      return b(state2$p, more, lift1, $$throw, (state3, a$2) => more(v4 => done(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a$2)));
    }))
  ))));
  return (state1, more, lift1, $$throw, done) => more(v1 => a(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a$1) => more(v2 => {
      const $1 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
      return more(v1$1 => $0(
        $1,
        more,
        lift1,
        $$throw,
        (state2$1, a$2) => more(v2$1 => done($1._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1, a$1(a$2)))
      ));
    })
  ));
};
const indentNoAp = a => b => (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => a(
  state1,
  more,
  lift1,
  $$throw,
  (state2, a$1) => more(v2$1 => more(v3 => {
    const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
    return more(v2$2 => more(v1$1 => sameOrIndented(
      state2$p,
      more,
      lift1,
      $$throw,
      (state2$1, a$2) => more(v2$3 => more(v3$1 => {
        const state2$p$1 = state2$p._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1;
        return b(
          state2$p$1,
          more,
          lift1,
          $$throw,
          (state3, a$3) => more(v4 => {
            const $0 = state2$p$1._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3;
            return more(v4$1 => done(state2$p._3 && !$0._3 ? Parsing.$ParseState($0._1, $0._2, true) : $0, a$1));
          })
        );
      }))
    )));
  }))
)));
const indentOp = a => v => {
  const $0 = v._1;
  return (state1, more, lift1, $$throw, done) => more(v1 => a(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a$1) => more(v2 => {
      const $1 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
      return more(v1$1 => {
        const $2 = (state2$1, a$2) => more(v2$1 => done($1._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1, a$1(a$2)));
        const $3 = $1._1;
        const $4 = $1._2;
        return more(v3 => {
          const $5 = (v4, $5) => {
            const $6 = v4._3;
            return more(v5 => {
              if ($6) { return $$throw(v4, $5); }
              return $2($1, $0);
            });
          };
          return more(v2$1 => more(v1$2 => sameOrIndented(
            Parsing.$ParseState($3, $4, false),
            more,
            lift1,
            $5,
            (state2$1, a$2) => more(v2$2 => more(v3$1 => v._2(
              state2$1,
              more,
              lift1,
              $5,
              (state3, a$3) => more(v4 => $2(state2$1._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a$3))
            )))
          )));
        });
      });
    })
  ));
};
const withPos = x => (state1, more, lift1, $$throw, done) => more(v1 => get$p(
  state1,
  more,
  lift1,
  $$throw,
  (state2, a) => more(v2 => {
    const $0 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
    return more(v1$1 => Parsing.position(
      $0,
      more,
      lift1,
      $$throw,
      (state2$1, a$1) => more(v2$1 => {
        const $1 = $0._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1;
        return more(v1$2 => more(v2$2 => more(v1$3 => put$p(a$1)(
          $1,
          more,
          lift1,
          $$throw,
          (state2$2, a$2) => more(v2$3 => more(v3 => {
            const state2$p = $1._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2;
            return x(
              state2$p,
              more,
              lift1,
              $$throw,
              (state3, a$3) => more(v4 => {
                const $2 = state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3;
                return more(v2$4 => {
                  const $3 = $1._3 && !$2._3 ? Parsing.$ParseState($2._1, $2._2, true) : $2;
                  return more(v2$5 => more(v1$4 => put$p(a)(
                    $3,
                    more,
                    lift1,
                    $$throw,
                    (state2$3, a$4) => more(v2$6 => more(v3$1 => {
                      const state2$p$1 = $3._3 && !state2$3._3 ? Parsing.$ParseState(state2$3._1, state2$3._2, true) : state2$3;
                      return more(v4$1 => done(state2$p$1._3 && !state2$p$1._3 ? Parsing.$ParseState(state2$p$1._1, state2$p$1._2, true) : state2$p$1, a$3));
                    }))
                  )));
                });
              })
            );
          }))
        ))));
      })
    ));
  })
));
const indentAngles = p => withPos(indentAp(indentNoAp((state1, v, v1, v2, done) => done(state1, identity))(symbol("<")))(indentNoAp(p)(symbol(">"))));
const indentBraces = p => withPos(indentAp(indentNoAp((state1, v, v1, v2, done) => done(state1, identity))(symbol("{")))(indentNoAp(p)(symbol("}"))));
const indentBrackets = p => withPos(indentAp(indentNoAp((state1, v, v1, v2, done) => done(state1, identity))(symbol("[")))(indentNoAp(p)(symbol("]"))));
const indentParens = p => withPos(indentAp(indentNoAp((state1, v, v1, v2, done) => done(state1, identity))(symbol("(")))(indentNoAp(p)(symbol(")"))));
const checkIndent = (state1, more, lift1, $$throw, done) => more(v1 => Parsing.position(
  state1,
  more,
  lift1,
  $$throw,
  (state2, a) => more(v2 => {
    const $0 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
    return more(v1$1 => get$p(
      $0,
      more,
      lift1,
      $$throw,
      (state2$1, a$1) => more(v2$1 => (a.column === a$1.column ? ((state1$1, v, v1$2, v2$2, done$1) => done$1(state1$1, undefined)) : Parsing.fail("indentation doesn't match"))(
        $0._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1,
        more,
        lift1,
        $$throw,
        done
      ))
    ));
  })
));
const block1 = p => withPos((() => {
  const $0 = many1((state1, more, lift1, $$throw, done) => more(v2 => more(v1 => checkIndent(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2$1 => more(v3 => {
      const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
      return p(state2$p, more, lift1, $$throw, (state3, a$1) => more(v4 => done(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a$1)));
    }))
  ))));
  return (state1, more, lift1, $$throw, done) => more(v1 => $0(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2 => done(state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2, a))
  ));
})());
const block = p => withPos((() => {
  const $0 = Data$dList.many(Parsing.alternativeParserT)(Parsing.lazyParserT)((state1, more, lift1, $$throw, done) => more(v2 => more(v1 => checkIndent(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2$1 => more(v3 => {
      const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
      return p(state2$p, more, lift1, $$throw, (state3, a$1) => more(v4 => done(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a$1)));
    }))
  ))));
  return (state1, more, lift1, $$throw, done) => more(v1 => $0(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2 => done(state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2, a))
  ));
})());
const withBlock = f => a => p => withPos((state1, more, lift1, $$throw, done) => more(v1 => a(
  state1,
  more,
  lift1,
  $$throw,
  (state2, a$1) => more(v2 => {
    const $0 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
    return more(v1$1 => {
      const $1 = (state2$1, a$2) => more(v2$1 => (() => {
        if (a$2.tag === "Nothing") {
          const $1 = f(a$1)(Data$dList$dTypes.Nil);
          return (state1$1, v, v1$2, v2$2, done$1) => done$1(state1$1, $1);
        }
        if (a$2.tag === "Just") {
          const $1 = f(a$1)(a$2._1);
          return (state1$1, v, v1$2, v2$2, done$1) => done$1(state1$1, $1);
        }
        $runtime.fail();
      })()($0._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1, more, lift1, $$throw, done));
      const $2 = $0._1;
      const $3 = $0._2;
      return more(v3 => more(v1$2 => {
        const $4 = (v4, $4) => {
          const $5 = v4._3;
          return more(v5 => {
            if ($5) { return $$throw(v4, $4); }
            return $1($0, Data$dMaybe.Nothing);
          });
        };
        return more(v2$1 => more(v1$3 => indented(
          Parsing.$ParseState($2, $3, false),
          more,
          lift1,
          $4,
          (state2$1, a$2) => more(v2$2 => more(v3$1 => block(p)(
            state2$1,
            more,
            lift1,
            $4,
            (state3, a$3) => more(v4 => {
              const $5 = state2$1._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3;
              return more(v2$3 => $1($5, Data$dMaybe.$Maybe("Just", a$3)));
            })
          )))
        )));
      }));
    });
  })
)));
const withBlock$p = /* #__PURE__ */ withBlock(b => a => a);
export {
  
  
  
  
  checkIndent,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  sameOrIndented,
  
  
  
  withPos
};
