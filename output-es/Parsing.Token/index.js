// | Primitive parsers for an input stream of type
// | `(`[__List__](https://pursuit.purescript.org/packages/purescript-lists/docs/Data.List.Types#t:List) `a)`
// | for working with streams of tokens.
// |
// | This module is a port of the Haskell
// | [__Text.Parsec.Token__](https://hackage.haskell.org/package/parsec/docs/Text-Parsec-Token.html)
// | module.
import * as $runtime from "../runtime.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dCodePoint$dUnicode from "../Data.CodePoint.Unicode/index.js";
import * as Data$dCodePoint$dUnicode$dInternal from "../Data.CodePoint.Unicode.Internal/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dEnum from "../Data.Enum/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dInt from "../Data.Int/index.js";
import * as Data$dList from "../Data.List/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNumber from "../Data.Number/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dString$dCodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data$dString$dCommon from "../Data.String.Common/index.js";
import * as Data$dString$dUnicode from "../Data.String.Unicode/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Data$dUnfoldable from "../Data.Unfoldable/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Parsing$dCombinators from "../Parsing.Combinators/index.js";
import * as Parsing$dString from "../Parsing.String/index.js";
import * as Parsing$dString$dBasic from "../Parsing.String.Basic/index.js";
const identity = x => x;
const choice = /* #__PURE__ */ Parsing$dCombinators.choice(Data$dFoldable.foldableArray);
const toUnfoldable = /* #__PURE__ */ (() => Data$dUnfoldable.unfoldableArray.unfoldr(xs => {
  if (xs.tag === "Nil") { return Data$dMaybe.Nothing; }
  if (xs.tag === "Cons") { return Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple(xs._1, xs._2)); }
  $runtime.fail();
}))();
const LanguageDef = x => x;
const unGenLanguageDef = v => v;
const token = tokpos => (state1, more, lift1, $$throw, done) => more(v1 => Parsing.getParserT(
  state1,
  more,
  lift1,
  $$throw,
  (state2, a) => more(v2 => (() => {
    if (a._1.tag === "Nil") { return Parsing.fail("Unexpected EOF"); }
    if (a._1.tag === "Cons") {
      const $0 = a._1._1;
      const $1 = a._1._2;
      return (state1$1, more$1, lift1$1, throw$1, done$1) => more$1(v1$1 => {
        const $2 = Parsing.$ParseState($1, tokpos($0), true);
        return more$1(v2$1 => done$1($2, $0));
      });
    }
    $runtime.fail();
  })()(state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2, more, lift1, $$throw, done))
));
const when = tokpos => f => (v1, $0, $1, $2, $3) => {
  const $4 = v1._3;
  const $5 = v1._2;
  return $0(v1$1 => token(tokpos)(
    v1,
    $0,
    $1,
    (v2, $6) => $2(Parsing.$ParseState(v2._1, v2._2, $4), Parsing.$ParseError($6._1, $5)),
    (state2, a) => $0(v2 => {
      const $6 = f(a) ? ((state1, v, v1$2, v2$1, done) => done(state1, undefined)) : Parsing.fail("No alternative");
      const $7 = v1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
      return $0(v1$2 => $6(
        $7,
        $0,
        $1,
        (v2$1, $8) => $2(Parsing.$ParseState(v2$1._1, v2$1._2, $4), Parsing.$ParseError($8._1, $5)),
        (state2$1, a$1) => $0(v2$1 => $3($7._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1, a))
      ));
    })
  ));
};
const theReservedNames = v => {
  if (v.caseSensitive) { return Data$dArray.sortBy(Data$dOrd.ordString.compare)(v.reservedNames); }
  return Data$dArray.sortBy(Data$dOrd.ordString.compare)(Data$dFunctor.arrayMap(Data$dString$dCommon.toLower)(v.reservedNames));
};
const simpleSpace = /* #__PURE__ */ Parsing$dCombinators.skipMany1(/* #__PURE__ */ Parsing$dString.satisfyCodePoint(Data$dCodePoint$dUnicode.isSpace));
const oneLineComment = v => {
  const $0 = Parsing$dCombinators.skipMany(Parsing$dString.satisfy(v1 => v1 !== "\n"));
  return (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => {
    const $1 = state1._3;
    return Parsing$dString.string(v.commentLine)(
      state1,
      more,
      lift1,
      (v2$1, $2) => $$throw(Parsing.$ParseState(v2$1._1, v2$1._2, $1), $2),
      (state2, a) => more(v2$1 => more(v3 => {
        const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
        return $0(state2$p, more, lift1, $$throw, (state3, a$1) => more(v4 => done(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a$1)));
      }))
    );
  }));
};
const match = dictEq => tokpos => tok => when(tokpos)(v => dictEq.eq(v)(tok));
const isReserved = isReserved$a0$copy => isReserved$a1$copy => {
  let isReserved$a0 = isReserved$a0$copy, isReserved$a1 = isReserved$a1$copy, isReserved$c = true, isReserved$r;
  while (isReserved$c) {
    const names = isReserved$a0, name = isReserved$a1;
    const v = Data$dArray.unconsImpl(v => Data$dMaybe.Nothing, x => xs => Data$dMaybe.$Maybe("Just", {head: x, tail: xs}), names);
    if (v.tag === "Nothing") {
      isReserved$c = false;
      isReserved$r = false;
      continue;
    }
    if (v.tag === "Just") {
      const v1 = Data$dOrd.ordString.compare(v._1.head)(name);
      if (v1 === "LT") {
        isReserved$a0 = v._1.tail;
        isReserved$a1 = name;
        continue;
      }
      if (v1 === "EQ") {
        isReserved$c = false;
        isReserved$r = true;
        continue;
      }
      if (v1 === "GT") {
        isReserved$c = false;
        isReserved$r = false;
        continue;
      }
    }
    $runtime.fail();
  }
  return isReserved$r;
};
const inCommentSingle = v => {
  const startEnd = [...Data$dString$dCodeUnits.toCharArray(v.commentEnd), ...Data$dString$dCodeUnits.toCharArray(v.commentStart)];
  const go$lazy = $runtime.binding(() => Parsing.lazyParserT.defer(v$1 => {
    const $0 = Parsing$dCombinators.skipMany1(Parsing$dString$dBasic.noneOf(startEnd));
    const $1 = Parsing$dCombinators.withErrorMessage((() => {
      const $1 = Parsing$dString$dBasic.oneOf(startEnd);
      return (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => $1(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more(v2$1 => more(v3 => {
          const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
          return go$lazy()(
            state2$p,
            more,
            lift1,
            $$throw,
            (state3, a$1) => more(v4 => done(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a$1))
          );
        }))
      )));
    })())("end of comment");
    return (v2, $2, $3, $4, $5) => {
      const $6 = v2._1;
      const $7 = v2._2;
      return $2(v3 => $2(v1 => Parsing$dString.string(v.commentEnd)(
        Parsing.$ParseState($6, $7, false),
        $2,
        $3,
        (v2$1, $8) => $2(v5 => {
          const $9 = v2._1;
          const $10 = v2._2;
          return $2(v3$1 => {
            const $11 = (v4, $11) => {
              const $12 = v4._3;
              return $2(v5$1 => {
                if ($12) { return $4(v4, $11); }
                return $1(v2, $2, $3, $4, $5);
              });
            };
            return $2(v2$2 => $2(v1$1 => $0(
              Parsing.$ParseState($9, $10, false),
              $2,
              $3,
              $11,
              (state2, a) => $2(v2$3 => $2(v3$2 => go$lazy()(
                state2,
                $2,
                $3,
                $11,
                (state3, a$1) => $2(v4 => $5(state2._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a$1))
              )))
            )));
          });
        }),
        (state2, a) => $2(v2$1 => $5(state2, undefined))
      )));
    };
  }));
  const go = go$lazy();
  return go;
};
const multiLineComment = v => {
  const $0 = inComment(v);
  return (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => {
    const $1 = state1._3;
    return Parsing$dString.string(v.commentStart)(
      state1,
      more,
      lift1,
      (v2$1, $2) => $$throw(Parsing.$ParseState(v2$1._1, v2$1._2, $1), $2),
      (state2, a) => more(v2$1 => more(v3 => {
        const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
        return $0(state2$p, more, lift1, $$throw, (state3, a$1) => more(v4 => done(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a$1)));
      }))
    );
  }));
};
const inCommentMulti = v => {
  const startEnd = [...Data$dString$dCodeUnits.toCharArray(v.commentEnd), ...Data$dString$dCodeUnits.toCharArray(v.commentStart)];
  const go$lazy = $runtime.binding(() => Parsing.lazyParserT.defer(v$1 => {
    const $0 = multiLineComment(v);
    const $1 = Parsing$dCombinators.skipMany1(Parsing$dString$dBasic.noneOf(startEnd));
    const $2 = Parsing$dCombinators.withErrorMessage((() => {
      const $2 = Parsing$dString$dBasic.oneOf(startEnd);
      return (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => $2(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more(v2$1 => more(v3 => {
          const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
          return go$lazy()(
            state2$p,
            more,
            lift1,
            $$throw,
            (state3, a$1) => more(v4 => done(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a$1))
          );
        }))
      )));
    })())("end of comment");
    return (v2, $3, $4, $5, $6) => {
      const $7 = v2._1;
      const $8 = v2._2;
      return $3(v3 => $3(v1 => Parsing$dString.string(v.commentEnd)(
        Parsing.$ParseState($7, $8, false),
        $3,
        $4,
        (v2$1, $9) => $3(v5 => {
          const $10 = v2._1;
          const $11 = v2._2;
          return $3(v3$1 => {
            const $12 = (v4, $12) => {
              const $13 = v4._3;
              return $3(v5$1 => {
                if ($13) { return $5(v4, $12); }
                const $14 = v2._1;
                const $15 = v2._2;
                return $3(v3$2 => {
                  const $16 = (v4$1, $16) => {
                    const $17 = v4$1._3;
                    return $3(v5$2 => {
                      if ($17) { return $5(v4$1, $16); }
                      return $2(v2, $3, $4, $5, $6);
                    });
                  };
                  return $3(v2$2 => $3(v1$1 => $1(
                    Parsing.$ParseState($14, $15, false),
                    $3,
                    $4,
                    $16,
                    (state2, a) => $3(v2$3 => $3(v3$3 => go$lazy()(
                      state2,
                      $3,
                      $4,
                      $16,
                      (state3, a$1) => $3(v4$1 => $6(state2._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a$1))
                    )))
                  )));
                });
              });
            };
            return $3(v2$2 => $3(v1$1 => $0(
              Parsing.$ParseState($10, $11, false),
              $3,
              $4,
              $12,
              (state2, a) => $3(v2$3 => $3(v3$2 => go$lazy()(
                state2,
                $3,
                $4,
                $12,
                (state3, a$1) => $3(v4 => $6(state2._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a$1))
              )))
            )));
          });
        }),
        (state2, a) => $3(v2$1 => $6(state2, undefined))
      )));
    };
  }));
  const go = go$lazy();
  return go;
};
const inComment = v => {
  if (v.nestedComments) { return inCommentMulti(v); }
  return inCommentSingle(v);
};
const whiteSpace$p = v => {
  if (v.commentLine === "" && v.commentStart === "") { return Parsing$dCombinators.skipMany(Parsing$dCombinators.withErrorMessage(simpleSpace)("")); }
  if (v.commentLine === "") {
    return Parsing$dCombinators.skipMany((() => {
      const $0 = Parsing$dCombinators.withErrorMessage(multiLineComment(v))("");
      return (v2, $1, $2, $3, $4) => {
        const $5 = v2._1;
        const $6 = v2._2;
        return $1(v3 => simpleSpace(
          Parsing.$ParseState($5, $6, false),
          $1,
          $2,
          (v4, $7) => {
            const $8 = v4._3;
            return $1(v5 => {
              if ($8) { return $3(v4, $7); }
              return $0(v2, $1, $2, $3, $4);
            });
          },
          $4
        ));
      };
    })());
  }
  if (v.commentStart === "") {
    return Parsing$dCombinators.skipMany((() => {
      const $0 = Parsing$dCombinators.withErrorMessage(oneLineComment(v))("");
      return (v2, $1, $2, $3, $4) => {
        const $5 = v2._1;
        const $6 = v2._2;
        return $1(v3 => simpleSpace(
          Parsing.$ParseState($5, $6, false),
          $1,
          $2,
          (v4, $7) => {
            const $8 = v4._3;
            return $1(v5 => {
              if ($8) { return $3(v4, $7); }
              return $0(v2, $1, $2, $3, $4);
            });
          },
          $4
        ));
      };
    })());
  }
  return Parsing$dCombinators.skipMany((() => {
    const $0 = oneLineComment(v);
    const $1 = Parsing$dCombinators.withErrorMessage(multiLineComment(v))("");
    return (v2, $2, $3, $4, $5) => {
      const $6 = v2._1;
      const $7 = v2._2;
      return $2(v3 => simpleSpace(
        Parsing.$ParseState($6, $7, false),
        $2,
        $3,
        (v4, $8) => {
          const $9 = v4._3;
          return $2(v5 => {
            if ($9) { return $4(v4, $8); }
            const $10 = v2._1;
            const $11 = v2._2;
            return $2(v3$1 => $0(
              Parsing.$ParseState($10, $11, false),
              $2,
              $3,
              (v4$1, $12) => {
                const $13 = v4$1._3;
                return $2(v5$1 => {
                  if ($13) { return $4(v4$1, $12); }
                  return $1(v2, $2, $3, $4, $5);
                });
              },
              $5
            ));
          });
        },
        $5
      ));
    };
  })());
};
const makeTokenParser = v => {
  const $0 = Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v$1 => v$1 === "-"))("'-'");
  const $1 = Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v$1 => v$1 === "+"))("'+'");
  const sign1 = (v2, $2, $3, $4, $5) => {
    const $6 = v2._1;
    const $7 = v2._2;
    return $2(v3 => $2(v1 => $0(
      Parsing.$ParseState($6, $7, false),
      $2,
      $3,
      (v4, $8) => {
        const $9 = v4._3;
        return $2(v5 => {
          if ($9) { return $4(v4, $8); }
          const $10 = v2._1;
          const $11 = v2._2;
          return $2(v3$1 => $2(v1$1 => $1(
            Parsing.$ParseState($10, $11, false),
            $2,
            $3,
            (v4$1, $12) => {
              const $13 = v4$1._3;
              return $2(v5$1 => {
                if ($13) { return $4(v4$1, $12); }
                return $5(v2, identity);
              });
            },
            (state2, a) => $2(v2$1 => $5(state2, identity))
          )));
        });
      },
      (state2, a) => $2(v2$1 => $5(state2, a$1 => -a$1))
    )));
  };
  const $2 = Parsing$dString$dBasic.oneOf(["o", "O"]);
  const $3 = Data$dArray.some(Parsing.alternativeParserT)(Parsing.lazyParserT)(Parsing$dString$dBasic.octDigit);
  const octal = (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => $2(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2$1 => more(v3 => {
      const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
      return more(v1$1 => $3(
        state2$p,
        more,
        lift1,
        $$throw,
        (state2$1, a$1) => more(v2$2 => {
          const $4 = Data$dFoldable.foldlArray(v1$2 => v2$3 => {
            if (v1$2.tag === "Nothing") { return Data$dMaybe.Nothing; }
            if (v1$2.tag === "Just") {
              const $4 = Data$dEnum.toCharCode(v2$3);
              const hexUpper = $4 - 65 | 0;
              const hexLower = $4 - 97 | 0;
              const dec = $4 - 48 | 0;
              if (dec <= 9 && dec >= 0) { return Data$dMaybe.$Maybe("Just", (8 * v1$2._1 | 0) + dec | 0); }
              if (hexLower <= 5 && hexLower >= 0) { return Data$dMaybe.$Maybe("Just", ((8 * v1$2._1 | 0) + hexLower | 0) + 10 | 0); }
              if (hexUpper <= 5 && hexUpper >= 0) { return Data$dMaybe.$Maybe("Just", ((8 * v1$2._1 | 0) + hexUpper | 0) + 10 | 0); }
              return Data$dMaybe.Nothing;
            }
            $runtime.fail();
          })(Data$dMaybe.$Maybe("Just", 0))(a$1);
          return (() => {
            if ($4.tag === "Nothing") { return Parsing.fail("not digits"); }
            if ($4.tag === "Just") {
              const $5 = $4._1;
              return (state1$1, v$1, v1$2, v2$3, done$1) => done$1(state1$1, $5);
            }
            $runtime.fail();
          })()(
            state2$p._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1,
            more,
            lift1,
            $$throw,
            (state3, a$2) => more(v4 => done(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a$2))
          );
        })
      ));
    }))
  )));
  const $4 = whiteSpace$p(v);
  const semi = (state1, more, lift1, $$throw, done) => more(v1 => more(v2 => more(v1$1 => Parsing$dString.string(";")(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2$1 => more(v3 => {
      const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
      return $4(
        state2$p,
        more,
        lift1,
        $$throw,
        (state3, a$1) => more(v4 => {
          const $5 = state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3;
          return more(v2$2 => done($5, ";"));
        })
      );
    }))
  ))));
  const $5 = Parsing$dString$dBasic.oneOf(["x", "X"]);
  const $6 = Data$dArray.some(Parsing.alternativeParserT)(Parsing.lazyParserT)(Parsing$dString$dBasic.hexDigit);
  const hexadecimal = (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => $5(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2$1 => more(v3 => {
      const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
      return more(v1$1 => $6(
        state2$p,
        more,
        lift1,
        $$throw,
        (state2$1, a$1) => more(v2$2 => {
          const $7 = Data$dFoldable.foldlArray(v1$2 => v2$3 => {
            if (v1$2.tag === "Nothing") { return Data$dMaybe.Nothing; }
            if (v1$2.tag === "Just") {
              const $7 = Data$dEnum.toCharCode(v2$3);
              const hexUpper = $7 - 65 | 0;
              const hexLower = $7 - 97 | 0;
              const dec = $7 - 48 | 0;
              if (dec <= 9 && dec >= 0) { return Data$dMaybe.$Maybe("Just", (16 * v1$2._1 | 0) + dec | 0); }
              if (hexLower <= 5 && hexLower >= 0) { return Data$dMaybe.$Maybe("Just", ((16 * v1$2._1 | 0) + hexLower | 0) + 10 | 0); }
              if (hexUpper <= 5 && hexUpper >= 0) { return Data$dMaybe.$Maybe("Just", ((16 * v1$2._1 | 0) + hexUpper | 0) + 10 | 0); }
              return Data$dMaybe.Nothing;
            }
            $runtime.fail();
          })(Data$dMaybe.$Maybe("Just", 0))(a$1);
          return (() => {
            if ($7.tag === "Nothing") { return Parsing.fail("not digits"); }
            if ($7.tag === "Just") {
              const $8 = $7._1;
              return (state1$1, v$1, v1$2, v2$3, done$1) => done$1(state1$1, $8);
            }
            $runtime.fail();
          })()(
            state2$p._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1,
            more,
            lift1,
            $$throw,
            (state3, a$2) => more(v4 => done(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a$2))
          );
        })
      ));
    }))
  )));
  const fraction = Parsing$dCombinators.withErrorMessage((() => {
    const $7 = Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v$1 => v$1 === "."))("'.'");
    return (state1, more, lift1, $$throw, done) => more(v1 => $7(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more(v2 => {
        const $8 = Parsing$dCombinators.withErrorMessage(Data$dArray.some(Parsing.alternativeParserT)(Parsing.lazyParserT)(Parsing$dString$dBasic.digit))("fraction");
        const $9 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
        return more(v1$1 => $8(
          $9,
          more,
          lift1,
          $$throw,
          (state2$1, a$1) => more(v2$1 => {
            const $10 = Data$dFoldable.foldrArray(v1$2 => v2$2 => {
              if (v2$2.tag === "Nothing") { return Data$dMaybe.Nothing; }
              if (v2$2.tag === "Just") {
                const $10 = Data$dEnum.toCharCode(v1$2);
                const hexUpper = $10 - 65 | 0;
                const hexLower = $10 - 97 | 0;
                const dec = $10 - 48 | 0;
                if (dec <= 9 && dec >= 0) { return Data$dMaybe.$Maybe("Just", (v2$2._1 + Data$dInt.toNumber(dec)) / 10.0); }
                if (hexLower <= 5 && hexLower >= 0) { return Data$dMaybe.$Maybe("Just", (v2$2._1 + Data$dInt.toNumber(hexLower + 10 | 0)) / 10.0); }
                if (hexUpper <= 5 && hexUpper >= 0) { return Data$dMaybe.$Maybe("Just", (v2$2._1 + Data$dInt.toNumber(hexUpper + 10 | 0)) / 10.0); }
                return Data$dMaybe.Nothing;
              }
              $runtime.fail();
            })(Data$dMaybe.$Maybe("Just", 0.0))(a$1);
            return (() => {
              if ($10.tag === "Nothing") { return Parsing.fail("not digit"); }
              if ($10.tag === "Just") {
                const $11 = $10._1;
                return (state1$1, v$1, v1$2, v2$2, done$1) => done$1(state1$1, $11);
              }
              $runtime.fail();
            })()($9._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1, more, lift1, $$throw, done);
          })
        ));
      })
    ));
  })())("fraction");
  const escapeGap = Parsing$dCombinators.withErrorMessage((() => {
    const $7 = Data$dArray.some(Parsing.alternativeParserT)(Parsing.lazyParserT)(Parsing$dString$dBasic.space);
    const $8 = Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v$1 => v$1 === "\\"))("'\\\\'");
    return (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => $7(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more(v2$1 => more(v3 => {
        const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
        return $8(state2$p, more, lift1, $$throw, (state3, a$1) => more(v4 => done(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a$1)));
      }))
    )));
  })())("end of string gap");
  const escapeEmpty = Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v$1 => v$1 === "&"))("'&'");
  const $7 = Data$dArray.some(Parsing.alternativeParserT)(Parsing.lazyParserT)(Parsing$dString$dBasic.digit);
  const decimal = (state1, more, lift1, $$throw, done) => more(v1 => $7(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2 => {
      const $8 = Data$dFoldable.foldlArray(v1$1 => v2$1 => {
        if (v1$1.tag === "Nothing") { return Data$dMaybe.Nothing; }
        if (v1$1.tag === "Just") {
          const $8 = Data$dEnum.toCharCode(v2$1);
          const hexUpper = $8 - 65 | 0;
          const hexLower = $8 - 97 | 0;
          const dec = $8 - 48 | 0;
          if (dec <= 9 && dec >= 0) { return Data$dMaybe.$Maybe("Just", (10 * v1$1._1 | 0) + dec | 0); }
          if (hexLower <= 5 && hexLower >= 0) { return Data$dMaybe.$Maybe("Just", ((10 * v1$1._1 | 0) + hexLower | 0) + 10 | 0); }
          if (hexUpper <= 5 && hexUpper >= 0) { return Data$dMaybe.$Maybe("Just", ((10 * v1$1._1 | 0) + hexUpper | 0) + 10 | 0); }
          return Data$dMaybe.Nothing;
        }
        $runtime.fail();
      })(Data$dMaybe.$Maybe("Just", 0))(a);
      return (() => {
        if ($8.tag === "Nothing") { return Parsing.fail("not digits"); }
        if ($8.tag === "Just") {
          const $9 = $8._1;
          return (state1$1, v$1, v1$1, v2$1, done$1) => done$1(state1$1, $9);
        }
        $runtime.fail();
      })()(state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2, more, lift1, $$throw, done);
    })
  ));
  const power = e => {
    if (e < 0) { return 1.0 / power(-e); }
    return Data$dNumber.pow(10.0)(Data$dInt.toNumber(e));
  };
  const $8 = Parsing$dString$dBasic.oneOf(["e", "E"]);
  const exponent$p = Parsing$dCombinators.withErrorMessage((state1, more, lift1, $$throw, done) => more(v1 => $8(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2 => {
      const $9 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
      return more(v1$1 => sign1(
        $9,
        more,
        lift1,
        $$throw,
        (state2$1, a$1) => more(v2$1 => {
          const $10 = Parsing$dCombinators.withErrorMessage(decimal)("exponent");
          const $11 = $9._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1;
          return more(v1$2 => $10(
            $11,
            more,
            lift1,
            $$throw,
            (state2$2, a$2) => more(v2$2 => done($11._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2, power(a$1(a$2))))
          ));
        })
      ));
    })
  )))("exponent");
  const fractExponent = n => (v2, $9, $10, $11, $12) => {
    const $13 = v2._1;
    const $14 = v2._2;
    return $9(v3 => {
      const $15 = (v4, $15) => {
        const $16 = v4._3;
        return $9(v5 => {
          if ($16) { return $11(v4, $15); }
          return $9(v1 => exponent$p(
            v2,
            $9,
            $10,
            $11,
            (state2, a) => $9(v2$1 => $12(v2._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2, Data$dInt.toNumber(n) * a))
          ));
        });
      };
      return $9(v1 => fraction(
        Parsing.$ParseState($13, $14, false),
        $9,
        $10,
        $15,
        (state2, a) => $9(v2$1 => $9(v1$1 => {
          const $16 = (state2$1, a$1) => $9(v2$2 => $12(
            state2._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1,
            (Data$dInt.toNumber(n) + a) * a$1
          ));
          const $17 = state2._1;
          const $18 = state2._2;
          return $9(v3$1 => exponent$p(
            Parsing.$ParseState($17, $18, false),
            $9,
            $10,
            (v4, $19) => {
              const $20 = v4._3;
              return $9(v5 => {
                if ($20) { return $15(v4, $19); }
                return $16(state2, 1.0);
              });
            },
            $16
          ));
        }))
      ));
    });
  };
  const decimalFloat = (state1, more, lift1, $$throw, done) => more(v1 => decimal(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2 => {
      const $9 = fractExponent(a);
      const $10 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
      const $11 = $10._1;
      const $12 = $10._2;
      return more(v3 => more(v1$1 => $9(
        Parsing.$ParseState($11, $12, false),
        more,
        lift1,
        (v4, $13) => {
          const $14 = v4._3;
          return more(v5 => {
            if ($14) { return $$throw(v4, $13); }
            return done($10, Data$dEither.$Either("Left", a));
          });
        },
        (state2$1, a$1) => more(v2$1 => done(state2$1, Data$dEither.$Either("Right", a$1)))
      )));
    })
  ));
  const zeroNumber = Parsing$dCombinators.withErrorMessage((() => {
    const $9 = Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v$1 => v$1 === "0"))("'0'");
    return (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => $9(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more(v2$1 => more(v3 => {
        const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
        const $10 = (state3, a$1) => more(v4 => done(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a$1));
        const $11 = state2$p._1;
        const $12 = state2$p._2;
        return more(v3$1 => hexadecimal(
          Parsing.$ParseState($11, $12, false),
          more,
          lift1,
          (v4, $13) => {
            const $14 = v4._3;
            return more(v5 => {
              if ($14) { return $$throw(v4, $13); }
              const $15 = state2$p._1;
              const $16 = state2$p._2;
              return more(v3$2 => octal(
                Parsing.$ParseState($15, $16, false),
                more,
                lift1,
                (v4$1, $17) => {
                  const $18 = v4$1._3;
                  return more(v5$1 => {
                    if ($18) { return $$throw(v4$1, $17); }
                    const $19 = state2$p._1;
                    const $20 = state2$p._2;
                    return more(v3$3 => decimal(
                      Parsing.$ParseState($19, $20, false),
                      more,
                      lift1,
                      (v4$2, $21) => {
                        const $22 = v4$2._3;
                        return more(v5$2 => {
                          if ($22) { return $$throw(v4$2, $21); }
                          return $10(state2$p, 0);
                        });
                      },
                      $10
                    ));
                  });
                },
                $10
              ));
            });
          },
          $10
        ));
      }))
    )));
  })())("");
  const $9 = whiteSpace$p(v);
  const comma = (state1, more, lift1, $$throw, done) => more(v1 => more(v2 => more(v1$1 => Parsing$dString.string(",")(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2$1 => more(v3 => {
      const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
      return $9(
        state2$p,
        more,
        lift1,
        $$throw,
        (state3, a$1) => more(v4 => {
          const $10 = state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3;
          return more(v2$2 => done($10, ","));
        })
      );
    }))
  ))));
  const $10 = choice(Data$dFunctor.arrayMap(v1 => {
    const $10 = v1._1;
    const $11 = v1._2;
    const $12 = Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v$1 => v$1 === $10))(Data$dShow.showCharImpl($10));
    return (state1, more, lift1, $$throw, done) => more(v1$1 => $12(state1, more, lift1, $$throw, (state2, a) => more(v2 => done(state2, $11))));
  })(Data$dArray.zipWithImpl(Data$dTuple.Tuple, ["a", "b", "f", "n", "r", "t", "v", "\\", "\"", "'"], ["\u0007", "\b", "\f", "\n", "\r", "\t", "\u000b", "\\", "\"", "'"])));
  const $11 = Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v$1 => v$1 === "o"))("'o'");
  const $12 = Data$dArray.some(Parsing.alternativeParserT)(Parsing.lazyParserT)(Parsing$dString$dBasic.octDigit);
  const $13 = Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v$1 => v$1 === "x"))("'x'");
  const $14 = Data$dArray.some(Parsing.alternativeParserT)(Parsing.lazyParserT)(Parsing$dString$dBasic.hexDigit);
  const $15 = choice(Data$dFunctor.arrayMap(v1 => {
    const $15 = v1._2;
    return (v1$1, $16, $17, $18, $19) => {
      const $20 = v1$1._3;
      return $16(v1$2 => Parsing$dString.string(v1._1)(v1$1, $16, $17, (v2, $21) => $18(Parsing.$ParseState(v2._1, v2._2, $20), $21), (state2, a) => $16(v2 => $19(state2, $15))));
    };
  })(Data$dArray.zipWithImpl(
    Data$dTuple.Tuple,
    [
      "NUL",
      "SOH",
      "STX",
      "ETX",
      "EOT",
      "ENQ",
      "ACK",
      "BEL",
      "DLE",
      "DC1",
      "DC2",
      "DC3",
      "DC4",
      "NAK",
      "SYN",
      "ETB",
      "CAN",
      "SUB",
      "ESC",
      "DEL",
      "BS",
      "HT",
      "LF",
      "VT",
      "FF",
      "CR",
      "SO",
      "SI",
      "EM",
      "FS",
      "GS",
      "RS",
      "US",
      "SP"
    ],
    [
      "\u0000",
      "\u0001",
      "\u0002",
      "\u0003",
      "\u0004",
      "\u0005",
      "\u0006",
      "\u0007",
      "\u0010",
      "\u0011",
      "\u0012",
      "\u0013",
      "\u0014",
      "\u0015",
      "\u0016",
      "\u0017",
      "\u0018",
      "\u001a",
      "\u001b",
      "",
      "\b",
      "\t",
      "\n",
      "\u000b",
      "\f",
      "\r",
      "\u000e",
      "\u000f",
      "\u0019",
      "\u001c",
      "\u001d",
      "\u001e",
      "\u001f",
      " "
    ]
  )));
  const $16 = Parsing$dCombinators.withErrorMessage((() => {
    const $16 = Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v$1 => v$1 === "^"))("'^'");
    return (state1, more, lift1, $$throw, done) => more(v1 => $16(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more(v2 => {
        const $17 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
        return more(v1$1 => Parsing$dString$dBasic.upper(
          $17,
          more,
          lift1,
          $$throw,
          (state2$1, a$1) => more(v2$1 => {
            const $18 = (Data$dEnum.toCharCode(a$1) - 65 | 0) + 1 | 0;
            if ($18 >= 0 && $18 <= 65535) { return done($17._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1, Data$dEnum.fromCharCode($18)); }
            return Parsing.fail("invalid character code (should not happen)")(
              $17._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1,
              more,
              lift1,
              $$throw,
              done
            );
          })
        ));
      })
    ));
  })())("escape code");
  const escapeCode = (v2, $17, $18, $19, $20) => {
    const $21 = v2._1;
    const $22 = v2._2;
    return $17(v3 => $10(
      Parsing.$ParseState($21, $22, false),
      $17,
      $18,
      (v4, $23) => {
        const $24 = v4._3;
        return $17(v5 => {
          if ($24) { return $19(v4, $23); }
          const $25 = v2._1;
          const $26 = v2._2;
          return $17(v3$1 => {
            const $27 = (v4$1, $27) => {
              const $28 = v4$1._3;
              return $17(v5$1 => {
                if ($28) { return $19(v4$1, $27); }
                const $29 = v2._1;
                const $30 = v2._2;
                return $17(v3$2 => $15(
                  Parsing.$ParseState($29, $30, false),
                  $17,
                  $18,
                  (v4$2, $31) => {
                    const $32 = v4$2._3;
                    return $17(v5$2 => {
                      if ($32) { return $19(v4$2, $31); }
                      return $16(v2, $17, $18, $19, $20);
                    });
                  },
                  $20
                ));
              });
            };
            return $17(v1 => {
              const $28 = (state2, a) => $17(v2$1 => {
                if (a > 1114111) { return Parsing.fail("invalid escape sequence")(state2, $17, $18, $27, $20); }
                if (a >= 0 && a <= 65535) { return $20(state2, Data$dEnum.fromCharCode(a)); }
                return Parsing.fail("invalid character code (should not happen)")(state2, $17, $18, $27, $20);
              });
              return $17(v3$2 => decimal(
                Parsing.$ParseState($25, $26, false),
                $17,
                $18,
                (v4$1, $29) => {
                  const $30 = v4$1._3;
                  return $17(v5$1 => {
                    if ($30) { return $27(v4$1, $29); }
                    return $17(v3$3 => {
                      const $31 = (v4$2, $31) => {
                        const $32 = v4$2._3;
                        return $17(v5$2 => {
                          if ($32) { return $27(v4$2, $31); }
                          return $17(v2$1 => $17(v1$1 => $13(
                            Parsing.$ParseState($25, $26, false),
                            $17,
                            $18,
                            $27,
                            (state2, a) => $17(v2$2 => $17(v3$4 => $17(v1$2 => $14(
                              state2,
                              $17,
                              $18,
                              $27,
                              (state2$1, a$1) => $17(v2$3 => {
                                const $33 = Data$dFoldable.foldlArray(v1$3 => v2$4 => {
                                  if (v1$3.tag === "Nothing") { return Data$dMaybe.Nothing; }
                                  if (v1$3.tag === "Just") {
                                    const $33 = Data$dEnum.toCharCode(v2$4);
                                    const hexUpper = $33 - 65 | 0;
                                    const hexLower = $33 - 97 | 0;
                                    const dec = $33 - 48 | 0;
                                    if (dec <= 9 && dec >= 0) { return Data$dMaybe.$Maybe("Just", (16 * v1$3._1 | 0) + dec | 0); }
                                    if (hexLower <= 5 && hexLower >= 0) { return Data$dMaybe.$Maybe("Just", ((16 * v1$3._1 | 0) + hexLower | 0) + 10 | 0); }
                                    if (hexUpper <= 5 && hexUpper >= 0) { return Data$dMaybe.$Maybe("Just", ((16 * v1$3._1 | 0) + hexUpper | 0) + 10 | 0); }
                                    return Data$dMaybe.Nothing;
                                  }
                                  $runtime.fail();
                                })(Data$dMaybe.$Maybe("Just", 0))(a$1);
                                return (() => {
                                  if ($33.tag === "Nothing") { return Parsing.fail("not digits"); }
                                  if ($33.tag === "Just") {
                                    const $34 = $33._1;
                                    return (state1, v$1, v1$3, v2$4, done) => done(state1, $34);
                                  }
                                  $runtime.fail();
                                })()(
                                  state2._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1,
                                  $17,
                                  $18,
                                  $27,
                                  (state3, a$2) => $17(v4$3 => $28(state2._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a$2))
                                );
                              })
                            ))))
                          )));
                        });
                      };
                      return $17(v2$1 => $17(v1$1 => $11(
                        Parsing.$ParseState($25, $26, false),
                        $17,
                        $18,
                        $31,
                        (state2, a) => $17(v2$2 => $17(v3$4 => $17(v1$2 => $12(
                          state2,
                          $17,
                          $18,
                          $31,
                          (state2$1, a$1) => $17(v2$3 => {
                            const $32 = Data$dFoldable.foldlArray(v1$3 => v2$4 => {
                              if (v1$3.tag === "Nothing") { return Data$dMaybe.Nothing; }
                              if (v1$3.tag === "Just") {
                                const $32 = Data$dEnum.toCharCode(v2$4);
                                const hexUpper = $32 - 65 | 0;
                                const hexLower = $32 - 97 | 0;
                                const dec = $32 - 48 | 0;
                                if (dec <= 9 && dec >= 0) { return Data$dMaybe.$Maybe("Just", (8 * v1$3._1 | 0) + dec | 0); }
                                if (hexLower <= 5 && hexLower >= 0) { return Data$dMaybe.$Maybe("Just", ((8 * v1$3._1 | 0) + hexLower | 0) + 10 | 0); }
                                if (hexUpper <= 5 && hexUpper >= 0) { return Data$dMaybe.$Maybe("Just", ((8 * v1$3._1 | 0) + hexUpper | 0) + 10 | 0); }
                                return Data$dMaybe.Nothing;
                              }
                              $runtime.fail();
                            })(Data$dMaybe.$Maybe("Just", 0))(a$1);
                            return (() => {
                              if ($32.tag === "Nothing") { return Parsing.fail("not digits"); }
                              if ($32.tag === "Just") {
                                const $33 = $32._1;
                                return (state1, v$1, v1$3, v2$4, done) => done(state1, $33);
                              }
                              $runtime.fail();
                            })()(
                              state2._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1,
                              $17,
                              $18,
                              $31,
                              (state3, a$2) => $17(v4$2 => $28(state2._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a$2))
                            );
                          })
                        ))))
                      )));
                    });
                  });
                },
                $28
              ));
            });
          });
        });
      },
      $20
    ));
  };
  return {
    identifier: (() => {
      const $17 = Parsing$dCombinators.withErrorMessage((state1, more, lift1, $$throw, done) => more(v1 => v.identStart(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more(v2 => {
          const $17 = Data$dArray.many(Parsing.alternativeParserT)(Parsing.lazyParserT)(v.identLetter);
          const $18 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
          return more(v1$1 => $17(
            $18,
            more,
            lift1,
            $$throw,
            (state2$1, a$1) => more(v2$1 => done(
              $18._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1,
              Data$dString$dCodeUnits.singleton(a) + Data$dString$dCodeUnits.fromCharArray(a$1)
            ))
          ));
        })
      )))("identifier");
      const $18 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => {
        const $19 = state1._3;
        return more(v1$1 => $17(
          state1,
          more,
          lift1,
          (v2$1, $20) => $$throw(Parsing.$ParseState(v2$1._1, v2$1._2, $19), $20),
          (state2, a) => more(v2$1 => (isReserved(theReservedNames(v))(v.caseSensitive ? a : Data$dString$dCommon.toLower(a))
            ? Parsing.fail("reserved word " + Data$dShow.showStringImpl(a))
            : (state1$1, v$1, v1$2, v2$2, done$1) => done$1(state1$1, a))(
            state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2,
            more,
            lift1,
            (v2$2, $20) => $$throw(Parsing.$ParseState(v2$2._1, v2$2._2, $19), $20),
            (state2$1, a$1) => more(v2$2 => more(v3 => {
              const state2$p = state1._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1;
              return $18(
                state2$p,
                more,
                lift1,
                $$throw,
                (state3, a$2) => more(v4 => done(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a$1))
              );
            }))
          ))
        ));
      }));
    })(),
    reserved: name => {
      const $17 = (() => {
        if (v.caseSensitive) {
          return (state1, more, lift1, $$throw, done) => more(v1 => Parsing$dString.string(name)(state1, more, lift1, $$throw, (state2, a) => more(v2 => done(state2, name))));
        }
        const msg = Data$dShow.showStringImpl(name);
        const walk = name$p => {
          const v1 = Data$dString$dCodeUnits.uncons(name$p);
          if (v1.tag === "Nothing") { return (state1, v$1, v1$1, v2, done) => done(state1, undefined); }
          if (v1.tag === "Just") {
            const $17 = Parsing$dCombinators.withErrorMessage((() => {
              if (Data$dCodePoint$dUnicode$dInternal.checkAttr([4096, 512, 524288, 1048576, 16384])(Data$dEnum.toCharCode(v1._1.head))) {
                const $17 = Data$dString$dCodeUnits.toChar(Data$dString$dUnicode.toLowerSimple(Data$dString$dCodeUnits.singleton(v1._1.head)));
                if ($17.tag === "Just") {
                  const $18 = Data$dString$dCodeUnits.toChar(Data$dString$dUnicode.toUpperSimple(Data$dString$dCodeUnits.singleton(v1._1.head)));
                  if ($18.tag === "Just") {
                    const $19 = $18._1;
                    const $20 = Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v$1 => v$1 === $17._1))(Data$dShow.showCharImpl($17._1));
                    const $21 = Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v$1 => v$1 === $19))(Data$dShow.showCharImpl($19));
                    return (v2, $22, $23, $24, $25) => {
                      const $26 = v2._1;
                      const $27 = v2._2;
                      return $22(v3 => $20(
                        Parsing.$ParseState($26, $27, false),
                        $22,
                        $23,
                        (v4, $28) => {
                          const $29 = v4._3;
                          return $22(v5 => {
                            if ($29) { return $24(v4, $28); }
                            return $21(v2, $22, $23, $24, $25);
                          });
                        },
                        $25
                      ));
                    };
                  }
                }
              }
              return Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v$1 => v$1 === v1._1.head))(Data$dShow.showCharImpl(v1._1.head));
            })())(msg);
            const $18 = walk(v1._1.tail);
            return (state1, more, lift1, $$throw, done) => more(v2 => more(v1$1 => $17(
              state1,
              more,
              lift1,
              $$throw,
              (state2, a) => more(v2$1 => more(v3 => {
                const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
                return $18(
                  state2$p,
                  more,
                  lift1,
                  $$throw,
                  (state3, a$1) => more(v4 => done(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a$1))
                );
              }))
            )));
          }
          $runtime.fail();
        };
        const $17 = walk(name);
        return (state1, more, lift1, $$throw, done) => more(v1 => $17(state1, more, lift1, $$throw, (state2, a) => more(v2 => done(state2, name))));
      })();
      const $18 = Parsing$dCombinators.withErrorMessage(Parsing$dCombinators.notFollowedBy(v.identLetter))("end of " + name);
      const $19 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => {
        const $20 = state1._3;
        return more(v2$1 => more(v1$1 => $17(
          state1,
          more,
          lift1,
          (v2$2, $21) => $$throw(Parsing.$ParseState(v2$2._1, v2$2._2, $20), $21),
          (state2, a) => more(v2$2 => more(v3 => {
            const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
            return $18(
              state2$p,
              more,
              lift1,
              (v2$3, $21) => $$throw(Parsing.$ParseState(v2$3._1, v2$3._2, $20), $21),
              (state3, a$1) => more(v4 => {
                const $21 = state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3;
                return more(v2$3 => more(v3$1 => {
                  const state2$p$1 = state1._3 && !$21._3 ? Parsing.$ParseState($21._1, $21._2, true) : $21;
                  return $19(
                    state2$p$1,
                    more,
                    lift1,
                    $$throw,
                    (state3$1, a$2) => more(v4$1 => done(state2$p$1._3 && !state3$1._3 ? Parsing.$ParseState(state3$1._1, state3$1._2, true) : state3$1, a$1))
                  );
                }));
              })
            );
          }))
        )));
      }));
    },
    operator: (() => {
      const $17 = Parsing$dCombinators.withErrorMessage((state1, more, lift1, $$throw, done) => more(v1 => v.opStart(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more(v2 => {
          const $17 = Data$dArray.many(Parsing.alternativeParserT)(Parsing.lazyParserT)(v.opLetter);
          const $18 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
          return more(v1$1 => $17(
            $18,
            more,
            lift1,
            $$throw,
            (state2$1, a$1) => more(v2$1 => done(
              $18._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1,
              Data$dString$dCodeUnits.singleton(a) + Data$dString$dCodeUnits.fromCharArray(a$1)
            ))
          ));
        })
      )))("operator");
      const $18 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => {
        const $19 = state1._3;
        return more(v1$1 => $17(
          state1,
          more,
          lift1,
          (v2$1, $20) => $$throw(Parsing.$ParseState(v2$1._1, v2$1._2, $19), $20),
          (state2, a) => more(v2$1 => (isReserved(Data$dArray.sortBy(Data$dOrd.ordString.compare)(v.reservedOpNames))(a)
            ? Parsing.fail("reserved operator " + a)
            : (state1$1, v$1, v1$2, v2$2, done$1) => done$1(state1$1, a))(
            state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2,
            more,
            lift1,
            (v2$2, $20) => $$throw(Parsing.$ParseState(v2$2._1, v2$2._2, $19), $20),
            (state2$1, a$1) => more(v2$2 => more(v3 => {
              const state2$p = state1._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1;
              return $18(
                state2$p,
                more,
                lift1,
                $$throw,
                (state3, a$2) => more(v4 => done(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a$1))
              );
            }))
          ))
        ));
      }));
    })(),
    reservedOp: name => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => {
        const $18 = state1._3;
        return more(v1$1 => Parsing$dString.string(name)(
          state1,
          more,
          lift1,
          (v2$1, $19) => $$throw(Parsing.$ParseState(v2$1._1, v2$1._2, $18), $19),
          (state2, a) => more(v2$1 => Parsing$dCombinators.withErrorMessage(Parsing$dCombinators.notFollowedBy(v.opLetter))("end of " + name)(
            state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2,
            more,
            lift1,
            (v2$2, $19) => $$throw(Parsing.$ParseState(v2$2._1, v2$2._2, $18), $19),
            (state2$1, a$1) => more(v2$2 => more(v3 => {
              const state2$p = state1._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1;
              return $17(
                state2$p,
                more,
                lift1,
                $$throw,
                (state3, a$2) => more(v4 => done(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a$1))
              );
            }))
          ))
        ));
      }));
    },
    charLiteral: Parsing$dCombinators.withErrorMessage((() => {
      const $17 = Parsing$dCombinators.between(Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v$1 => v$1 === "'"))("'\\''"))(Parsing$dCombinators.withErrorMessage(Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v$1 => v$1 === "'"))("'\\''"))("end of character"))((() => {
        const $17 = Parsing$dString.satisfy(c => c !== "'" && c !== "\\" && c > "\u001a");
        const $18 = Parsing$dCombinators.withErrorMessage((() => {
          const $18 = Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v$1 => v$1 === "\\"))("'\\\\'");
          return (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => $18(
            state1,
            more,
            lift1,
            $$throw,
            (state2, a) => more(v2$1 => more(v3 => {
              const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
              return escapeCode(
                state2$p,
                more,
                lift1,
                $$throw,
                (state3, a$1) => more(v4 => done(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a$1))
              );
            }))
          )));
        })())("literal character");
        return (v2, $19, $20, $21, $22) => {
          const $23 = v2._1;
          const $24 = v2._2;
          return $19(v3 => $17(
            Parsing.$ParseState($23, $24, false),
            $19,
            $20,
            (v4, $25) => {
              const $26 = v4._3;
              return $19(v5 => {
                if ($26) { return $21(v4, $25); }
                return $18(v2, $19, $20, $21, $22);
              });
            },
            $22
          ));
        };
      })());
      const $18 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => $17(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more(v2$1 => more(v3 => {
          const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
          return $18(state2$p, more, lift1, $$throw, (state3, a$1) => more(v4 => done(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a)));
        }))
      )));
    })())("character"),
    stringLiteral: (() => {
      const $17 = Parsing$dCombinators.withErrorMessage((() => {
        const $17 = Parsing$dCombinators.between(Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v$1 => v$1 === "\""))("'\"'"))(Parsing$dCombinators.withErrorMessage(Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v$1 => v$1 === "\""))("'\"'"))("end of string"))(Data$dList.many(Parsing.alternativeParserT)(Parsing.lazyParserT)((() => {
          const $17 = Parsing$dString.satisfy(c => c !== "\"" && c !== "\\" && c > "\u001a");
          const $18 = Parsing$dCombinators.withErrorMessage((() => {
            const $18 = Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v$1 => v$1 === "\\"))("'\\\\'");
            return (state1, more, lift1, $$throw, done) => more(v1 => $18(
              state1,
              more,
              lift1,
              $$throw,
              (state2, a) => more(v2 => {
                const $19 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
                const $20 = $19._1;
                const $21 = $19._2;
                return more(v3 => more(v1$1 => escapeGap(
                  Parsing.$ParseState($20, $21, false),
                  more,
                  lift1,
                  (v4, $22) => {
                    const $23 = v4._3;
                    return more(v5 => {
                      if ($23) { return $$throw(v4, $22); }
                      const $24 = $19._1;
                      const $25 = $19._2;
                      return more(v3$1 => more(v1$2 => escapeEmpty(
                        Parsing.$ParseState($24, $25, false),
                        more,
                        lift1,
                        (v4$1, $26) => {
                          const $27 = v4$1._3;
                          return more(v5$1 => {
                            if ($27) { return $$throw(v4$1, $26); }
                            return more(v1$3 => escapeCode($19, more, lift1, $$throw, (state2$1, a$1) => more(v2$1 => done(state2$1, Data$dMaybe.$Maybe("Just", a$1)))));
                          });
                        },
                        (state2$1, a$1) => more(v2$1 => done(state2$1, Data$dMaybe.Nothing))
                      )));
                    });
                  },
                  (state2$1, a$1) => more(v2$1 => done(state2$1, Data$dMaybe.Nothing))
                )));
              })
            ));
          })())("string character");
          return (v2, $19, $20, $21, $22) => {
            const $23 = v2._1;
            const $24 = v2._2;
            return $19(v3 => $19(v1 => $17(
              Parsing.$ParseState($23, $24, false),
              $19,
              $20,
              (v4, $25) => {
                const $26 = v4._3;
                return $19(v5 => {
                  if ($26) { return $21(v4, $25); }
                  return $18(v2, $19, $20, $21, $22);
                });
              },
              (state2, a) => $19(v2$1 => $22(state2, Data$dMaybe.$Maybe("Just", a)))
            )));
          };
        })()));
        return (state1, more, lift1, $$throw, done) => more(v1 => $17(
          state1,
          more,
          lift1,
          $$throw,
          (state2, a) => more(v2 => done(
            state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2,
            Data$dString$dCodeUnits.fromCharArray(toUnfoldable(Data$dList$dTypes.foldableList.foldr(v1$1 => v2$1 => {
              if (v1$1.tag === "Nothing") { return v2$1; }
              if (v1$1.tag === "Just") { return Data$dList$dTypes.$List("Cons", v1$1._1, v2$1); }
              $runtime.fail();
            })(Data$dList$dTypes.Nil)(a)))
          ))
        ));
      })())("literal string");
      const $18 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => $17(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more(v2$1 => more(v3 => {
          const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
          return $18(state2$p, more, lift1, $$throw, (state3, a$1) => more(v4 => done(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a)));
        }))
      )));
    })(),
    natural: Parsing$dCombinators.withErrorMessage((() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => {
        const $18 = (state2, a) => more(v2$1 => more(v3 => {
          const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
          return $17(state2$p, more, lift1, $$throw, (state3, a$1) => more(v4 => done(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a)));
        }));
        const $19 = state1._1;
        const $20 = state1._2;
        return more(v3 => zeroNumber(
          Parsing.$ParseState($19, $20, false),
          more,
          lift1,
          (v4, $21) => {
            const $22 = v4._3;
            return more(v5 => {
              if ($22) { return $$throw(v4, $21); }
              return decimal(state1, more, lift1, $$throw, $18);
            });
          },
          $18
        ));
      }));
    })())("natural"),
    integer: Parsing$dCombinators.withErrorMessage((() => {
      const $17 = whiteSpace$p(v);
      const $18 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => more(v1$1 => more(v2$1 => more(v1$2 => sign1(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more(v2$2 => more(v3 => {
          const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
          return $17(
            state2$p,
            more,
            lift1,
            $$throw,
            (state3, a$1) => more(v4 => {
              const $19 = state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3;
              return more(v2$3 => {
                const $20 = state1._3 && !$19._3 ? Parsing.$ParseState($19._1, $19._2, true) : $19;
                return more(v1$3 => {
                  const $21 = (state2$1, a$2) => more(v2$4 => {
                    const $21 = $20._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1;
                    const $22 = a(a$2);
                    return more(v2$5 => more(v3$1 => {
                      const state2$p$1 = state1._3 && !$21._3 ? Parsing.$ParseState($21._1, $21._2, true) : $21;
                      return $18(
                        state2$p$1,
                        more,
                        lift1,
                        $$throw,
                        (state3$1, a$3) => more(v4$1 => done(state2$p$1._3 && !state3$1._3 ? Parsing.$ParseState(state3$1._1, state3$1._2, true) : state3$1, $22))
                      );
                    }));
                  });
                  const $22 = $20._1;
                  const $23 = $20._2;
                  return more(v3$1 => zeroNumber(
                    Parsing.$ParseState($22, $23, false),
                    more,
                    lift1,
                    (v4$1, $24) => {
                      const $25 = v4$1._3;
                      return more(v5 => {
                        if ($25) { return $$throw(v4$1, $24); }
                        return decimal($20, more, lift1, $$throw, $21);
                      });
                    },
                    $21
                  ));
                });
              });
            })
          );
        }))
      ))))));
    })())("integer"),
    float: Parsing$dCombinators.withErrorMessage((() => {
      const $17 = Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v$1 => v$1 === "-"))("'-'");
      const $18 = Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v$1 => v$1 === "+"))("'+'");
      const $19 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => more(v1$1 => more(v1$2 => {
        const $20 = (state2, a) => more(v2$1 => {
          const $20 = (() => {
            if (a.tag === "Nothing") { return identity; }
            if (a.tag === "Just") { return a._1; }
            $runtime.fail();
          })();
          return more(v2$2 => {
            const $21 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
            return more(v1$3 => more(v1$4 => decimal(
              $21,
              more,
              lift1,
              $$throw,
              (state2$1, a$1) => more(v2$3 => fractExponent(a$1)(
                $21._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1,
                more,
                lift1,
                $$throw,
                (state2$2, a$2) => more(v2$4 => {
                  const $22 = $21._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2;
                  const $23 = $20(a$2);
                  return more(v2$5 => more(v3 => {
                    const state2$p = state1._3 && !$22._3 ? Parsing.$ParseState($22._1, $22._2, true) : $22;
                    return $19(
                      state2$p,
                      more,
                      lift1,
                      $$throw,
                      (state3, a$3) => more(v4 => done(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, $23))
                    );
                  }));
                })
              ))
            )));
          });
        });
        const $21 = state1._1;
        const $22 = state1._2;
        return more(v3 => more(v1$3 => {
          const $23 = (v4, $23) => {
            const $24 = v4._3;
            return more(v5 => {
              if ($24) { return $$throw(v4, $23); }
              return $20(state1, Data$dMaybe.Nothing);
            });
          };
          return more(v3$1 => more(v1$4 => $17(
            Parsing.$ParseState($21, $22, false),
            more,
            lift1,
            (v4, $24) => {
              const $25 = v4._3;
              return more(v5 => {
                if ($25) { return $23(v4, $24); }
                return more(v3$2 => more(v1$5 => $18(
                  Parsing.$ParseState($21, $22, false),
                  more,
                  lift1,
                  (v4$1, $26) => {
                    const $27 = v4$1._3;
                    return more(v5$1 => {
                      if ($27) { return $23(v4$1, $26); }
                      return more(v2$1 => $20(Parsing.$ParseState($21, $22, false), Data$dMaybe.$Maybe("Just", identity)));
                    });
                  },
                  (state2, a) => more(v2$1 => more(v2$2 => $20(state2, Data$dMaybe.$Maybe("Just", identity))))
                )));
              });
            },
            (state2, a) => more(v2$1 => more(v2$2 => $20(state2, Data$dMaybe.$Maybe("Just", a$1 => -a$1))))
          )));
        }));
      }))));
    })())("float"),
    naturalOrFloat: Parsing$dCombinators.withErrorMessage((() => {
      const $17 = Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v$1 => v$1 === "0"))("'0'");
      const $18 = fractExponent(0);
      const $19 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => {
        const $20 = (state2, a) => more(v2$1 => more(v3 => {
          const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
          return $19(state2$p, more, lift1, $$throw, (state3, a$1) => more(v4 => done(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a)));
        }));
        const $21 = state1._1;
        const $22 = state1._2;
        return more(v3 => {
          const $23 = (v4, $23) => {
            const $24 = v4._3;
            return more(v5 => {
              if ($24) { return $$throw(v4, $23); }
              return decimalFloat(state1, more, lift1, $$throw, $20);
            });
          };
          return more(v2$1 => more(v1$1 => $17(
            Parsing.$ParseState($21, $22, false),
            more,
            lift1,
            $23,
            (state2, a) => more(v2$2 => more(v3$1 => {
              const $24 = (state3, a$1) => more(v4 => $20(state2._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a$1));
              const $25 = state2._1;
              const $26 = state2._2;
              return more(v3$2 => {
                const $27 = (v4, $27) => {
                  const $28 = v4._3;
                  return more(v5 => {
                    if ($28) { return $23(v4, $27); }
                    const $29 = state2._1;
                    const $30 = state2._2;
                    return more(v3$3 => decimalFloat(
                      Parsing.$ParseState($29, $30, false),
                      more,
                      lift1,
                      (v4$1, $31) => {
                        const $32 = v4$1._3;
                        return more(v5$1 => {
                          if ($32) { return $23(v4$1, $31); }
                          const $33 = state2._1;
                          const $34 = state2._2;
                          return more(v3$4 => more(v1$2 => $18(
                            Parsing.$ParseState($33, $34, false),
                            more,
                            lift1,
                            (v4$2, $35) => {
                              const $36 = v4$2._3;
                              return more(v5$2 => {
                                if ($36) { return $23(v4$2, $35); }
                                return $24(state2, Data$dEither.$Either("Left", 0));
                              });
                            },
                            (state2$1, a$1) => more(v2$3 => $24(state2$1, Data$dEither.$Either("Right", a$1)))
                          )));
                        });
                      },
                      $24
                    ));
                  });
                };
                return more(v1$2 => more(v3$3 => hexadecimal(
                  Parsing.$ParseState($25, $26, false),
                  more,
                  lift1,
                  (v4, $28) => {
                    const $29 = v4._3;
                    return more(v5 => {
                      if ($29) { return $27(v4, $28); }
                      return octal(Parsing.$ParseState($25, $26, false), more, lift1, $27, (state2$1, a$1) => more(v2$3 => $24(state2$1, Data$dEither.$Either("Left", a$1))));
                    });
                  },
                  (state2$1, a$1) => more(v2$3 => $24(state2$1, Data$dEither.$Either("Left", a$1)))
                )));
              });
            }))
          )));
        });
      }));
    })())("number"),
    decimal,
    hexadecimal,
    octal,
    symbol: name => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw, done) => more(v1 => more(v2 => more(v1$1 => Parsing$dString.string(name)(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more(v2$1 => more(v3 => {
          const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
          return $17(
            state2$p,
            more,
            lift1,
            $$throw,
            (state3, a$1) => more(v4 => {
              const $18 = state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3;
              return more(v2$2 => done($18, name));
            })
          );
        }))
      ))));
    },
    lexeme: p => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => p(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more(v2$1 => more(v3 => {
          const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
          return $17(state2$p, more, lift1, $$throw, (state3, a$1) => more(v4 => done(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a)));
        }))
      )));
    },
    whiteSpace: whiteSpace$p(v),
    parens: p => Parsing$dCombinators.between((() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw, done) => more(v1 => more(v2 => more(v1$1 => Parsing$dString.string("(")(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more(v2$1 => more(v3 => {
          const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
          return $17(
            state2$p,
            more,
            lift1,
            $$throw,
            (state3, a$1) => more(v4 => {
              const $18 = state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3;
              return more(v2$2 => done($18, "("));
            })
          );
        }))
      ))));
    })())((() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw, done) => more(v1 => more(v2 => more(v1$1 => Parsing$dString.string(")")(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more(v2$1 => more(v3 => {
          const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
          return $17(
            state2$p,
            more,
            lift1,
            $$throw,
            (state3, a$1) => more(v4 => {
              const $18 = state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3;
              return more(v2$2 => done($18, ")"));
            })
          );
        }))
      ))));
    })())(p),
    braces: p => Parsing$dCombinators.between((() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw, done) => more(v1 => more(v2 => more(v1$1 => Parsing$dString.string("{")(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more(v2$1 => more(v3 => {
          const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
          return $17(
            state2$p,
            more,
            lift1,
            $$throw,
            (state3, a$1) => more(v4 => {
              const $18 = state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3;
              return more(v2$2 => done($18, "{"));
            })
          );
        }))
      ))));
    })())((() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw, done) => more(v1 => more(v2 => more(v1$1 => Parsing$dString.string("}")(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more(v2$1 => more(v3 => {
          const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
          return $17(
            state2$p,
            more,
            lift1,
            $$throw,
            (state3, a$1) => more(v4 => {
              const $18 = state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3;
              return more(v2$2 => done($18, "}"));
            })
          );
        }))
      ))));
    })())(p),
    angles: p => Parsing$dCombinators.between((() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw, done) => more(v1 => more(v2 => more(v1$1 => Parsing$dString.string("<")(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more(v2$1 => more(v3 => {
          const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
          return $17(
            state2$p,
            more,
            lift1,
            $$throw,
            (state3, a$1) => more(v4 => {
              const $18 = state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3;
              return more(v2$2 => done($18, "<"));
            })
          );
        }))
      ))));
    })())((() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw, done) => more(v1 => more(v2 => more(v1$1 => Parsing$dString.string(">")(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more(v2$1 => more(v3 => {
          const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
          return $17(
            state2$p,
            more,
            lift1,
            $$throw,
            (state3, a$1) => more(v4 => {
              const $18 = state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3;
              return more(v2$2 => done($18, ">"));
            })
          );
        }))
      ))));
    })())(p),
    brackets: p => Parsing$dCombinators.between((() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw, done) => more(v1 => more(v2 => more(v1$1 => Parsing$dString.string("[")(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more(v2$1 => more(v3 => {
          const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
          return $17(
            state2$p,
            more,
            lift1,
            $$throw,
            (state3, a$1) => more(v4 => {
              const $18 = state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3;
              return more(v2$2 => done($18, "["));
            })
          );
        }))
      ))));
    })())((() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw, done) => more(v1 => more(v2 => more(v1$1 => Parsing$dString.string("]")(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more(v2$1 => more(v3 => {
          const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
          return $17(
            state2$p,
            more,
            lift1,
            $$throw,
            (state3, a$1) => more(v4 => {
              const $18 = state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3;
              return more(v2$2 => done($18, "]"));
            })
          );
        }))
      ))));
    })())(p),
    semi,
    comma,
    colon: (() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw, done) => more(v1 => more(v2 => more(v1$1 => Parsing$dString.string(":")(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more(v2$1 => more(v3 => {
          const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
          return $17(
            state2$p,
            more,
            lift1,
            $$throw,
            (state3, a$1) => more(v4 => {
              const $18 = state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3;
              return more(v2$2 => done($18, ":"));
            })
          );
        }))
      ))));
    })(),
    dot: (() => {
      const $17 = whiteSpace$p(v);
      return (state1, more, lift1, $$throw, done) => more(v1 => more(v2 => more(v1$1 => Parsing$dString.string(".")(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more(v2$1 => more(v3 => {
          const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
          return $17(
            state2$p,
            more,
            lift1,
            $$throw,
            (state3, a$1) => more(v4 => {
              const $18 = state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3;
              return more(v2$2 => done($18, "."));
            })
          );
        }))
      ))));
    })(),
    semiSep: p => Parsing$dCombinators.sepBy(p)(semi),
    semiSep1: p => Parsing$dCombinators.sepBy1(p)(semi),
    commaSep: p => Parsing$dCombinators.sepBy(p)(comma),
    commaSep1: p => Parsing$dCombinators.sepBy1(p)(comma)
  };
};
const eof = (state1, more, lift1, $$throw, done) => more(v1 => Parsing.getParserT(
  state1,
  more,
  lift1,
  $$throw,
  (state2, a) => more(v2 => (a._1.tag === "Nil" ? Parsing.consume : Parsing.fail("Expected EOF"))(
    state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2,
    more,
    lift1,
    $$throw,
    done
  ))
));
export {
  LanguageDef,
  choice,
  eof,
  identity,
  inComment,
  inCommentMulti,
  inCommentSingle,
  isReserved,
  makeTokenParser,
  match,
  multiLineComment,
  oneLineComment,
  simpleSpace,
  theReservedNames,
  toUnfoldable,
  token,
  unGenLanguageDef,
  when,
  whiteSpace$p
};
