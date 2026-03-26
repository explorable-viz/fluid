import * as $runtime from "../runtime.js";
import * as Control$dMonad$dState$dTrans from "../Control.Monad.State.Trans/index.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dIdentity from "../Data.Identity/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dString$dCodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Parsing$dCombinators from "../Parsing.Combinators/index.js";
import * as Parsing$dCombinators$dArray from "../Parsing.Combinators.Array/index.js";
import * as Parsing$dIndent from "../Parsing.Indent/index.js";
import * as Parsing$dString from "../Parsing.String/index.js";
import * as Parsing$dString$dBasic from "../Parsing.String.Basic/index.js";
const lift = m => (state1, v, lift$p, v1, done) => lift$p(Control$dMonad$dState$dTrans.bindStateT(Data$dIdentity.monadIdentity).Apply0().Functor0().map(a => v2 => done(state1, a))(m));
const put = /* #__PURE__ */ (() => {
  const $0 = Control$dMonad$dState$dTrans.monadStateStateT(Data$dIdentity.monadIdentity);
  return s => $0.state(v => Data$dTuple.$Tuple(undefined, s));
})();
const parseableString = {parse: Parsing$dString.string};
const parseableChar = {parse: Parsing$dString.char};
const whitespace = /* #__PURE__ */ Parsing$dCombinators.skipMany(/* #__PURE__ */ (() => {
  const $0 = Parsing$dString$dBasic.oneOf([" ", "\t", "\n"]);
  const $1 = Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v => v === "#"))("'#'");
  const $2 = Parsing$dCombinators.skipMany(Parsing$dString.satisfy(v => v !== "\n"));
  return (v2, $3, $4, $5, $6) => {
    const $7 = v2._1;
    const $8 = v2._2;
    return $3(v3 => $3(v1 => $0(
      Parsing.$ParseState($7, $8, false),
      $3,
      $4,
      (v4, $9) => {
        const $10 = v4._3;
        return $3(v5 => {
          if ($10) { return $5(v4, $9); }
          return $3(v2$1 => $3(v1$1 => $1(
            v2,
            $3,
            $4,
            $5,
            (state2, a) => $3(v2$2 => $3(v3$1 => {
              const state2$p = v2._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
              return $2(state2$p, $3, $4, $5, (state3, a$1) => $3(v4$1 => $6(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a$1)));
            }))
          )));
        });
      },
      (state2, a) => $3(v2$1 => $6(state2, undefined))
    )));
  };
})());
const stringLetter = /* #__PURE__ */ Parsing$dString.satisfy(c => c !== "\"" && c !== "\\" && c > "\u001a");
const stringChar = /* #__PURE__ */ Parsing$dCombinators.withErrorMessage((state1, more, lift1, $$throw, done) => more(v1 => stringLetter(
  state1,
  more,
  lift1,
  $$throw,
  (state2, a) => more(v2 => done(state2, Data$dMaybe.$Maybe("Just", a)))
)))("string character");
const parse = dict => dict.parse;
const opChars = [":", "!", "#", "$", "%", "&", "*", "+", ".", "/", "<", "=", ">", "?", "\\", "^", "|", "-", "~"];
const lexeme = v => (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => v(
  state1,
  more,
  lift1,
  $$throw,
  (state2, a) => more(v2$1 => more(v3 => {
    const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
    return whitespace(state2$p, more, lift1, $$throw, (state3, a$1) => more(v4 => done(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a)));
  }))
)));
const operator = /* #__PURE__ */ lexeme(/* #__PURE__ */ (() => {
  const $0 = Data$dArray.some(Parsing.alternativeParserT)(Parsing.lazyParserT)(Parsing$dString$dBasic.oneOf(opChars));
  return (state1, more, lift1, $$throw, done) => more(v1 => $0(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2 => done(state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2, Data$dString$dCodeUnits.fromCharArray(a)))
  ));
})());
const reservedOperator = expected => (v1, $0, $1, $2, $3) => {
  const $4 = v1._3;
  return $0(v1$1 => operator(
    v1,
    $0,
    $1,
    (v2, $5) => $2(Parsing.$ParseState(v2._1, v2._2, $4), $5),
    (state2, a) => $0(v2 => (expected !== a ? Parsing.fail("Expected `" + expected + "`, received `" + a + "`") : (state1, v, v1$2, v2$1, done) => done(state1, undefined))(
      v1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2,
      $0,
      $1,
      (v2$1, $5) => $2(Parsing.$ParseState(v2$1._1, v2$1._2, $4), $5),
      $3
    ))
  ));
};
const stringLiteral = /* #__PURE__ */ lexeme(/* #__PURE__ */ Parsing$dCombinators.withErrorMessage(/* #__PURE__ */ (() => {
  const $0 = Parsing$dCombinators.between(Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v => v === "\""))("'\"'"))(Parsing$dCombinators.withErrorMessage(Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v => v === "\""))("'\"'"))("end of string"))(Parsing$dCombinators$dArray.many(stringChar));
  return (state1, more, lift1, $$throw, done) => more(v1 => $0(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2 => done(
      state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2,
      Data$dString$dCodeUnits.fromCharArray(Data$dFoldable.foldrArray(v => v1$1 => {
        if (v.tag === "Nothing") { return v1$1; }
        if (v.tag === "Just") { return [v._1, ...v1$1]; }
        $runtime.fail();
      })([])(a))
    ))
  ));
})())("literal string"));
const keywords = ["case", "def", "else", "for", "if", "import", "in", "lambda", "match"];
const unreserved = p => (state1, more, lift1, $$throw, done) => more(v1 => p(
  state1,
  more,
  lift1,
  $$throw,
  (state2, a) => more(v2 => (Data$dArray.elem(Data$dEq.eqString)(a)(keywords) ? Parsing.fail("Reserved identifier: " + a) : (state1$1, v, v1$1, v2$1, done$1) => done$1(state1$1, a))(
    state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2,
    more,
    lift1,
    $$throw,
    done
  ))
));
const identifier = start => letter => lexeme((state1, more, lift1, $$throw, done) => more(v1 => start(
  state1,
  more,
  lift1,
  $$throw,
  (state2, a) => more(v2 => {
    const $0 = Data$dArray.many(Parsing.alternativeParserT)(Parsing.lazyParserT)(letter);
    const $1 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
    return more(v1$1 => $0(
      $1,
      more,
      lift1,
      $$throw,
      (state2$1, a$1) => more(v2$1 => done(
        $1._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1,
        Data$dString$dCodeUnits.singleton(a) + Data$dString$dCodeUnits.fromCharArray(a$1)
      ))
    ));
  })
)));
const reserved = expected => {
  const $0 = identifier((() => {
    const $0 = Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v => v === "_"))("'_'");
    return (v2, $1, $2, $3, $4) => {
      const $5 = v2._1;
      const $6 = v2._2;
      return $1(v3 => Parsing$dString$dBasic.letter(
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
  })())((() => {
    const $0 = Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v => v === "_"))("'_'");
    return (v2, $1, $2, $3, $4) => {
      const $5 = v2._1;
      const $6 = v2._2;
      return $1(v3 => Parsing$dString$dBasic.alphaNum(
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
  return (v1, $1, $2, $3, $4) => {
    const $5 = v1._3;
    return $1(v1$1 => $0(
      v1,
      $1,
      $2,
      (v2, $6) => $3(Parsing.$ParseState(v2._1, v2._2, $5), $6),
      (state2, a) => $1(v2 => (expected !== a ? Parsing.fail("Expected `" + expected + "`, received `" + a + "`") : (state1, v, v1$2, v2$1, done) => done(state1, undefined))(
        v1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2,
        $1,
        $2,
        (v2$1, $6) => $3(Parsing.$ParseState(v2$1._1, v2$1._2, $5), $6),
        $4
      ))
    ));
  };
};
const variable = /* #__PURE__ */ unreserved(/* #__PURE__ */ identifier(/* #__PURE__ */ (() => {
  const $0 = Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v => v === "_"))("'_'");
  return (v2, $1, $2, $3, $4) => {
    const $5 = v2._1;
    const $6 = v2._2;
    return $1(v3 => Parsing$dString$dBasic.lower(
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
})())(/* #__PURE__ */ (() => {
  const $0 = Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v => v === "_"))("'_'");
  return (v2, $1, $2, $3, $4) => {
    const $5 = v2._1;
    const $6 = v2._2;
    return $1(v3 => Parsing$dString$dBasic.alphaNum(
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
})()));
const delim = dictParseable => a => {
  const $0 = lexeme(dictParseable.parse(a));
  return (state1, more, lift1, $$throw, done) => more(v1 => $0(state1, more, lift1, $$throw, (state2, a$1) => more(v2 => done(state2, undefined))));
};
const parens = /* #__PURE__ */ Parsing$dCombinators.between(/* #__PURE__ */ delim(parseableChar)("("))(/* #__PURE__ */ delim(parseableChar)(")"));
const trailingCommas = p => Parsing$dCombinators.sepEndBy(p)(delim(parseableChar)(","));
const fields = key => val => trailingCommas((state1, more, lift1, $$throw, done) => more(v1 => key(
  state1,
  more,
  lift1,
  $$throw,
  (state2, a) => more(v2 => {
    const $0 = delim(parseableChar)(":");
    const $1 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
    return more(v1$1 => $0(
      $1,
      more,
      lift1,
      $$throw,
      (state2$1, a$1) => more(v2$1 => {
        const $2 = $1._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1;
        return more(v1$2 => val(
          $2,
          more,
          lift1,
          $$throw,
          (state2$2, a$2) => more(v2$2 => done($2._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2, Data$dTuple.$Tuple(a, a$2)))
        ));
      })
    ));
  })
)));
const context = s => p => (state1, more, lift1, $$throw, done) => more(v1 => Parsing.position(
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
      (state2$1, err) => $$throw(
        state2$1,
        Parsing.$ParseError(
          Data$dString$dCodeUnits.take(200)(err._1 + "\n " + s + " on line " + Data$dShow.showIntImpl(a.line) + ", column " + Data$dShow.showIntImpl(a.column)),
          err._2
        )
      ),
      done
    ));
  })
));
const constructor = /* #__PURE__ */ unreserved(/* #__PURE__ */ identifier(Parsing$dString$dBasic.upper)(/* #__PURE__ */ (() => {
  const $0 = Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v => v === "_"))("'_'");
  return (v2, $1, $2, $3, $4) => {
    const $5 = v2._1;
    const $6 = v2._2;
    return $1(v3 => Parsing$dString$dBasic.alphaNum(
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
})()));
const commas1 = p => Parsing$dCombinators.sepBy1(p)(delim(parseableChar)(","));
const commas = p => Parsing$dCombinators.sepBy(p)(delim(parseableChar)(","));
const close = dictParseable => a => {
  const $0 = dictParseable.parse(a);
  return (state1, more, lift1, $$throw, done) => more(v1 => $0(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a$1) => more(v2 => {
      const $1 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
      return more(v1$1 => Parsing.position(
        $1,
        more,
        lift1,
        $$throw,
        (state2$1, a$2) => more(v2$1 => {
          const $2 = lift(put(a$2));
          const $3 = $1._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1;
          return more(v1$2 => $2(
            $3,
            more,
            lift1,
            $$throw,
            (state2$2, a$3) => more(v2$2 => whitespace($3._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2, more, lift1, $$throw, done))
          ));
        })
      ));
    })
  ));
};
const brackets = /* #__PURE__ */ Parsing$dCombinators.between(/* #__PURE__ */ delim(parseableChar)("["))(/* #__PURE__ */ delim(parseableChar)("]"));
const braces = /* #__PURE__ */ Parsing$dCombinators.between(/* #__PURE__ */ delim(parseableChar)("{"))(/* #__PURE__ */ delim(parseableChar)("}"));
const block = e => {
  const $0 = delim(parseableChar)(":");
  return (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => more(v2$1 => more(v1$1 => $0(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2$2 => more(v3 => {
      const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
      return Parsing$dIndent.sameOrIndented(
        state2$p,
        more,
        lift1,
        $$throw,
        (state3, a$1) => more(v4 => {
          const $1 = state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3;
          return more(v2$3 => more(v3$1 => {
            const state2$p$1 = state1._3 && !$1._3 ? Parsing.$ParseState($1._1, $1._2, true) : $1;
            return Parsing$dIndent.withPos(e)(
              state2$p$1,
              more,
              lift1,
              $$throw,
              (state3$1, a$2) => more(v4$1 => done(state2$p$1._3 && !state3$1._3 ? Parsing.$ParseState(state3$1._1, state3$1._2, true) : state3$1, a$2))
            );
          }));
        })
      );
    }))
  )))));
};
const align = p => (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => Parsing$dIndent.checkIndent(
  state1,
  more,
  lift1,
  $$throw,
  (state2, a) => more(v2$1 => more(v3 => {
    const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
    return p(state2$p, more, lift1, $$throw, (state3, a$1) => more(v4 => done(state2$p._3 && !state3._3 ? Parsing.$ParseState(state3._1, state3._2, true) : state3, a$1)));
  }))
)));
export {
  align,
  block,
  braces,
  brackets,
  close,
  commas,
  commas1,
  constructor,
  context,
  delim,
  fields,
  
  
  lexeme,
  
  
  operator,
  parens,
  
  parseableChar,
  parseableString,
  
  reserved,
  reservedOperator,
  
  
  stringLiteral,
  trailingCommas,
  
  variable,
  whitespace
};
