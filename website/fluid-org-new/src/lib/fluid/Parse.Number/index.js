import * as $runtime from "../runtime.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dEnum from "../Data.Enum/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dInt from "../Data.Int/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNumber from "../Data.Number/index.js";
import * as Data$dRing from "../Data.Ring/index.js";
import * as Parse$dParser from "../Parse.Parser/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Parsing$dCombinators from "../Parsing.Combinators/index.js";
import * as Parsing$dString from "../Parsing.String/index.js";
import * as Parsing$dString$dBasic from "../Parsing.String.Basic/index.js";
const identity = x => x;
const sign = dictRing => {
  const $0 = Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v => v === "-"))("'-'");
  const zero = dictRing.Semiring0().zero;
  const $1 = Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v => v === "+"))("'+'");
  return (v2, $2, $3, $4, $5) => {
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
      (state2, a) => $2(v2$1 => $5(state2, a$1 => dictRing.sub(zero)(a$1)))
    )));
  };
};
const sign1 = /* #__PURE__ */ sign(Data$dRing.ringInt);
const number = base => baseDigit => {
  const $0 = Data$dArray.some(Parsing.alternativeParserT)(Parsing.lazyParserT)(baseDigit);
  return (state1, more, lift1, $$throw, done) => more(v1 => $0(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2 => {
      const $1 = Data$dFoldable.foldlArray(v => v1$1 => {
        if (v.tag === "Nothing") { return Data$dMaybe.Nothing; }
        if (v.tag === "Just") {
          const $1 = Data$dEnum.toCharCode(v1$1);
          const hexUpper = $1 - 65 | 0;
          const hexLower = $1 - 97 | 0;
          const $2 = (() => {
            const dec = $1 - 48 | 0;
            if (dec <= 9 && dec >= 0) { return Data$dMaybe.$Maybe("Just", dec); }
            if (hexLower <= 5 && hexLower >= 0) { return Data$dMaybe.$Maybe("Just", hexLower + 10 | 0); }
            if (hexUpper <= 5 && hexUpper >= 0) { return Data$dMaybe.$Maybe("Just", hexUpper + 10 | 0); }
            return Data$dMaybe.Nothing;
          })();
          if ($2.tag === "Just") { return Data$dMaybe.$Maybe("Just", (base * v._1 | 0) + $2._1 | 0); }
          return Data$dMaybe.Nothing;
        }
        $runtime.fail();
      })(Data$dMaybe.$Maybe("Just", 0))(a);
      return (() => {
        if ($1.tag === "Nothing") { return Parsing.fail("not digits"); }
        if ($1.tag === "Just") {
          const $2 = $1._1;
          return (state1$1, v, v1$1, v2$1, done$1) => done$1(state1$1, $2);
        }
        $runtime.fail();
      })()(state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2, more, lift1, $$throw, done);
    })
  ));
};
const octal = /* #__PURE__ */ (() => {
  const $0 = Parsing$dString$dBasic.oneOf(["o", "O"]);
  const $1 = number(8)(Parsing$dString$dBasic.octDigit);
  return (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => $0(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2$1 => more(v3 => {
      const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
      return $1(state2$p, more, lift1, $$throw, (state3, a$1) => more(v4 => done(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a$1)));
    }))
  )));
})();
const hexadecimal = /* #__PURE__ */ (() => {
  const $0 = Parsing$dString$dBasic.oneOf(["x", "X"]);
  const $1 = number(16)(Parsing$dString$dBasic.hexDigit);
  return (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => $0(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2$1 => more(v3 => {
      const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
      return $1(state2$p, more, lift1, $$throw, (state3, a$1) => more(v4 => done(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a$1)));
    }))
  )));
})();
const fraction = /* #__PURE__ */ Parsing$dCombinators.withErrorMessage(/* #__PURE__ */ (() => {
  const $0 = Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v => v === "."))("'.'");
  return (state1, more, lift1, $$throw, done) => more(v1 => $0(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2 => {
      const $1 = Parsing$dCombinators.withErrorMessage(Data$dArray.some(Parsing.alternativeParserT)(Parsing.lazyParserT)(Parsing$dString$dBasic.digit))("fraction");
      const $2 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
      return more(v1$1 => $1(
        $2,
        more,
        lift1,
        $$throw,
        (state2$1, a$1) => more(v2$1 => {
          const $3 = Data$dFoldable.foldrArray(v => v1$2 => {
            if (v1$2.tag === "Nothing") { return Data$dMaybe.Nothing; }
            if (v1$2.tag === "Just") {
              const $3 = Data$dEnum.toCharCode(v);
              const hexUpper = $3 - 65 | 0;
              const hexLower = $3 - 97 | 0;
              const dec = $3 - 48 | 0;
              if (dec <= 9 && dec >= 0) { return Data$dMaybe.$Maybe("Just", (v1$2._1 + Data$dInt.toNumber(dec)) / 10.0); }
              if (hexLower <= 5 && hexLower >= 0) { return Data$dMaybe.$Maybe("Just", (v1$2._1 + Data$dInt.toNumber(hexLower + 10 | 0)) / 10.0); }
              if (hexUpper <= 5 && hexUpper >= 0) { return Data$dMaybe.$Maybe("Just", (v1$2._1 + Data$dInt.toNumber(hexUpper + 10 | 0)) / 10.0); }
              return Data$dMaybe.Nothing;
            }
            $runtime.fail();
          })(Data$dMaybe.$Maybe("Just", 0.0))(a$1);
          return (() => {
            if ($3.tag === "Nothing") { return Parsing.fail("not digit"); }
            if ($3.tag === "Just") {
              const $4 = $3._1;
              return (state1$1, v, v1$2, v2$2, done$1) => done$1(state1$1, $4);
            }
            $runtime.fail();
          })()($2._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1, more, lift1, $$throw, done);
        })
      ));
    })
  ));
})())("fraction");
const decimal = /* #__PURE__ */ number(10)(Parsing$dString$dBasic.digit);
const exponent$p = /* #__PURE__ */ (() => {
  const power = e => {
    if (e < 0) { return 1.0 / power(-e); }
    return Data$dNumber.pow(10.0)(Data$dInt.toNumber(e));
  };
  const $0 = Parsing$dString$dBasic.oneOf(["e", "E"]);
  return Parsing$dCombinators.withErrorMessage((state1, more, lift1, $$throw, done) => more(v1 => $0(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2 => {
      const $1 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
      return more(v1$1 => sign1(
        $1,
        more,
        lift1,
        $$throw,
        (state2$1, a$1) => more(v2$1 => {
          const $2 = Parsing$dCombinators.withErrorMessage(decimal)("exponent");
          const $3 = $1._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1;
          return more(v1$2 => $2(
            $3,
            more,
            lift1,
            $$throw,
            (state2$2, a$2) => more(v2$2 => done($3._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2, power(a$1(a$2))))
          ));
        })
      ));
    })
  )))("exponent");
})();
const fractExponent = n => (v2, $0, $1, $2, $3) => {
  const $4 = v2._1;
  const $5 = v2._2;
  return $0(v3 => {
    const $6 = (v4, $6) => {
      const $7 = v4._3;
      return $0(v5 => {
        if ($7) { return $2(v4, $6); }
        return $0(v1 => exponent$p(
          v2,
          $0,
          $1,
          $2,
          (state2, a) => $0(v2$1 => $3(v2._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2, Data$dInt.toNumber(n) * a))
        ));
      });
    };
    return $0(v1 => fraction(
      Parsing.$ParseState($4, $5, false),
      $0,
      $1,
      $6,
      (state2, a) => $0(v2$1 => $0(v1$1 => {
        const $7 = (state2$1, a$1) => $0(v2$2 => $3(state2._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1, (Data$dInt.toNumber(n) + a) * a$1));
        const $8 = state2._1;
        const $9 = state2._2;
        return $0(v3$1 => exponent$p(
          Parsing.$ParseState($8, $9, false),
          $0,
          $1,
          (v4, $10) => {
            const $11 = v4._3;
            return $0(v5 => {
              if ($11) { return $6(v4, $10); }
              return $7(state2, 1.0);
            });
          },
          $7
        ));
      }))
    ));
  });
};
const floating = /* #__PURE__ */ (() => {
  const $0 = sign(Data$dRing.ringNumber);
  return (state1, more, lift1, $$throw, done) => more(v1 => more(v1$1 => {
    const $1 = (state2, a) => more(v2 => {
      const $1 = (() => {
        if (a.tag === "Nothing") { return identity; }
        if (a.tag === "Just") { return a._1; }
        $runtime.fail();
      })();
      return more(v2$1 => {
        const $2 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
        return more(v1$2 => more(v1$3 => decimal(
          $2,
          more,
          lift1,
          $$throw,
          (state2$1, a$1) => more(v2$2 => fractExponent(a$1)(
            $2._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1,
            more,
            lift1,
            $$throw,
            (state2$2, a$2) => more(v2$3 => done($2._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2, $1(a$2)))
          ))
        )));
      });
    });
    const $2 = state1._1;
    const $3 = state1._2;
    return more(v3 => more(v1$2 => $0(
      Parsing.$ParseState($2, $3, false),
      more,
      lift1,
      (v4, $4) => {
        const $5 = v4._3;
        return more(v5 => {
          if ($5) { return $$throw(v4, $4); }
          return $1(state1, Data$dMaybe.Nothing);
        });
      },
      (state2, a) => more(v2 => $1(state2, Data$dMaybe.$Maybe("Just", a)))
    )));
  }));
})();
const $$float = /* #__PURE__ */ Parsing$dCombinators.withErrorMessage(/* #__PURE__ */ Parse$dParser.lexeme(floating))("float");
const zeroNumber = /* #__PURE__ */ Parsing$dCombinators.withErrorMessage(/* #__PURE__ */ (() => {
  const $0 = Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v => v === "0"))("'0'");
  return (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => $0(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2$1 => more(v3 => {
      const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
      const $1 = (state3, a$1) => more(v4 => done(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a$1));
      const $2 = state2$p._1;
      const $3 = state2$p._2;
      return more(v3$1 => hexadecimal(
        Parsing.$ParseState($2, $3, false),
        more,
        lift1,
        (v4, $4) => {
          const $5 = v4._3;
          return more(v5 => {
            if ($5) { return $$throw(v4, $4); }
            const $6 = state2$p._1;
            const $7 = state2$p._2;
            return more(v3$2 => octal(
              Parsing.$ParseState($6, $7, false),
              more,
              lift1,
              (v4$1, $8) => {
                const $9 = v4$1._3;
                return more(v5$1 => {
                  if ($9) { return $$throw(v4$1, $8); }
                  const $10 = state2$p._1;
                  const $11 = state2$p._2;
                  return more(v3$3 => decimal(
                    Parsing.$ParseState($10, $11, false),
                    more,
                    lift1,
                    (v4$2, $12) => {
                      const $13 = v4$2._3;
                      return more(v5$2 => {
                        if ($13) { return $$throw(v4$2, $12); }
                        return $1(state2$p, 0);
                      });
                    },
                    $1
                  ));
                });
              },
              $1
            ));
          });
        },
        $1
      ));
    }))
  )));
})())("");
const nat = (v2, $0, $1, $2, $3) => {
  const $4 = v2._1;
  const $5 = v2._2;
  return $0(v3 => zeroNumber(
    Parsing.$ParseState($4, $5, false),
    $0,
    $1,
    (v4, $6) => {
      const $7 = v4._3;
      return $0(v5 => {
        if ($7) { return $2(v4, $6); }
        return decimal(v2, $0, $1, $2, $3);
      });
    },
    $3
  ));
};
const $$int = (state1, more, lift1, $$throw, done) => more(v1 => Parse$dParser.lexeme(sign1)(
  state1,
  more,
  lift1,
  $$throw,
  (state2, a) => more(v2 => {
    const $0 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
    return more(v1$1 => nat(
      $0,
      more,
      lift1,
      $$throw,
      (state2$1, a$1) => more(v2$1 => done($0._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1, a(a$1)))
    ));
  })
));
const integer = /* #__PURE__ */ Parsing$dCombinators.withErrorMessage(/* #__PURE__ */ Parse$dParser.lexeme($$int))("integer");
const natural = /* #__PURE__ */ Parsing$dCombinators.withErrorMessage(/* #__PURE__ */ Parse$dParser.lexeme(nat))("natural");
export {
  
  
  $$float as float,
  
  
  
  
  
  
  integer,
  
  
  
  
  
  
  
};
