// | Basic `String` parsers derived from primitive `String` parsers.
// |
// | #### unicode dependency
// |
// | Some of the parsers in this module depend on the
// | [__unicode__](https://pursuit.purescript.org/packages/purescript-unicode)
// | package.
// | The __unicode__ package is large; about half a megabyte unminified.
// | If code which depends on __parsing__ is “tree-shaken”
// | “dead-code-eliminated,” then
// | all of the __unicode__ package will be eliminated.
// |
// | The __unicode__-dependent parsers in this module will call functions
// | which use large lookup tables from the __unicode__ package.
// | Using any of these __unicode__-dependent parsers
// | may result in a minified, dead-code-eliminated bundle size increase
// | of over 100 kilobytes.
import * as $runtime from "../runtime.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dCodePoint$dUnicode from "../Data.CodePoint.Unicode/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dEnum from "../Data.Enum/index.js";
import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dInt from "../Data.Int/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNumber from "../Data.Number/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dString$dCodePoints from "../Data.String.CodePoints/index.js";
import * as Data$dString$dCodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data$dString$dRegex$dFlags from "../Data.String.Regex.Flags/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Parsing$dCombinators from "../Parsing.Combinators/index.js";
import * as Parsing$dString from "../Parsing.String/index.js";
import * as Partial from "../Partial/index.js";
const show = /* #__PURE__ */ Data$dShow.showArrayImpl(Data$dShow.showStringImpl);
const show1 = /* #__PURE__ */ Data$dShow.showArrayImpl(Data$dShow.showCharImpl);
const takeWhile1 = predicate => Parsing$dString.consumeWith(s => {
  const value = Data$dString$dCodePoints.take(Data$dString$dCodePoints.countPrefix(predicate)(s))(s);
  if (Data$dString$dCodeUnits.length(value) > 0) {
    return Data$dEither.$Either("Right", {consumed: value, remainder: Data$dString$dCodeUnits.drop(Data$dString$dCodeUnits.length(value))(s), value});
  }
  return Data$dEither.$Either("Left", "Expected character satisfying predicate");
});
const takeWhile = predicate => Parsing$dString.consumeWith(s => {
  const value = Data$dString$dCodePoints.take(Data$dString$dCodePoints.countPrefix(predicate)(s))(s);
  return Data$dEither.$Either("Right", {consumed: value, remainder: Data$dString$dCodeUnits.drop(Data$dString$dCodeUnits.length(value))(s), value});
});
const whiteSpace = /* #__PURE__ */ takeWhile(Data$dCodePoint$dUnicode.isSpace);
const skipSpaces = (state1, more, lift1, $$throw, done) => more(v1 => takeWhile(Data$dCodePoint$dUnicode.isSpace)(
  state1,
  more,
  lift1,
  $$throw,
  (state2, a) => more(v2 => done(state2, undefined))
));
const satisfyCP = p => Parsing$dString.satisfy(x => p(Data$dEnum.toCharCode(x)));
const space = /* #__PURE__ */ Parsing$dCombinators.withErrorMessage(/* #__PURE__ */ satisfyCP(Data$dCodePoint$dUnicode.isSpace))("space");
const upper = /* #__PURE__ */ Parsing$dCombinators.withErrorMessage(/* #__PURE__ */ satisfyCP(Data$dCodePoint$dUnicode.isUpper))("uppercase letter");
const oneOfCodePoints = ss => Parsing$dCombinators.withLazyErrorMessage(Parsing$dString.satisfyCodePoint(a => Data$dArray.elem(Data$dString$dCodePoints.eqCodePoint)(a)(ss)))(v => "one of " + show(Data$dFunctor.arrayMap(Data$dString$dCodePoints.singleton)(ss)));
const oneOf = ss => Parsing$dCombinators.withLazyErrorMessage(Parsing$dString.satisfy(a => Data$dArray.elem(Data$dEq.eqChar)(a)(ss)))(v => "one of " + show1(ss));
const octDigit = /* #__PURE__ */ Parsing$dCombinators.withErrorMessage(/* #__PURE__ */ satisfyCP(Data$dCodePoint$dUnicode.isOctDigit))("oct digit");
const numberRegex = /* #__PURE__ */ (() => {
  const $0 = Parsing$dString.regex("[+-]?[0-9]*(\\.[0-9]*)?([eE][+-]?[0-9]*(\\.[0-9]*)?)?")(Data$dString$dRegex$dFlags.noFlags);
  if ($0.tag === "Left") { return Partial._crashWith($0._1); }
  if ($0.tag === "Right") { return $0._1; }
  $runtime.fail();
})();
const number = /* #__PURE__ */ (() => {
  const $0 = Parsing$dCombinators.choice(Data$dFoldable.foldableArray)([
    (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => Parsing$dString.string("Infinity")(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more(v2$1 => more(v3 => {
        const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
        return more(v4 => done(state2$p._3 && !state2$p._3 ? Parsing.$ParseState(state2$p._1, state2$p._2, true) : state2$p, Data$dNumber.infinity));
      }))
    ))),
    (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => Parsing$dString.string("+Infinity")(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more(v2$1 => more(v3 => {
        const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
        return more(v4 => done(state2$p._3 && !state2$p._3 ? Parsing.$ParseState(state2$p._1, state2$p._2, true) : state2$p, Data$dNumber.infinity));
      }))
    ))),
    (() => {
      const $0 = -Data$dNumber.infinity;
      return (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => Parsing$dString.string("-Infinity")(
        state1,
        more,
        lift1,
        $$throw,
        (state2, a) => more(v2$1 => more(v3 => {
          const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
          return more(v4 => done(state2$p._3 && !state2$p._3 ? Parsing.$ParseState(state2$p._1, state2$p._2, true) : state2$p, $0));
        }))
      )));
    })(),
    (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => Parsing$dString.string("NaN")(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a) => more(v2$1 => more(v3 => {
        const state2$p = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
        return more(v4 => done(state2$p._3 && !state2$p._3 ? Parsing.$ParseState(state2$p._1, state2$p._2, true) : state2$p, Data$dNumber.nan));
      }))
    ))),
    (v1, $0, $1, $2, $3) => {
      const $4 = v1._3;
      const $5 = v1._2;
      return $0(v1$1 => numberRegex(
        v1,
        $0,
        $1,
        (v2, $6) => $2(Parsing.$ParseState(v2._1, v2._2, $4), Parsing.$ParseError($6._1, $5)),
        (state2, a) => $0(v2 => {
          const v = Data$dNumber.fromStringImpl(a, Data$dNumber.isFinite, Data$dMaybe.Just, Data$dMaybe.Nothing);
          return (() => {
            if (v.tag === "Nothing") { return Parsing.fail("Expected Number"); }
            if (v.tag === "Just") {
              const $6 = v._1;
              return (state1, v$1, v1$2, v2$1, done) => done(state1, $6);
            }
            $runtime.fail();
          })()(
            v1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2,
            $0,
            $1,
            (v2$1, $6) => $2(Parsing.$ParseState(v2$1._1, v2$1._2, $4), Parsing.$ParseError($6._1, $5)),
            $3
          );
        })
      ));
    }
  ]);
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
          return Parsing.fail("Expected Number")(v2, $1, $2, $3, $4);
        });
      },
      $4
    ));
  };
})();
const noneOfCodePoints = ss => Parsing$dCombinators.withLazyErrorMessage(Parsing$dString.satisfyCodePoint(a => Data$dArray.notElem(Data$dString$dCodePoints.eqCodePoint)(a)(ss)))(v => "none of " + show(Data$dFunctor.arrayMap(Data$dString$dCodePoints.singleton)(ss)));
const noneOf = ss => Parsing$dCombinators.withLazyErrorMessage(Parsing$dString.satisfy(a => Data$dArray.notElem(Data$dEq.eqChar)(a)(ss)))(v => "none of " + show1(ss));
const lower = /* #__PURE__ */ Parsing$dCombinators.withErrorMessage(/* #__PURE__ */ satisfyCP(Data$dCodePoint$dUnicode.isLower))("lowercase letter");
const letter = /* #__PURE__ */ Parsing$dCombinators.withErrorMessage(/* #__PURE__ */ satisfyCP(Data$dCodePoint$dUnicode.isAlpha))("letter");
const intDecimalRegex = /* #__PURE__ */ (() => {
  const $0 = Parsing$dString.regex("[+-]?[0-9]+")(Data$dString$dRegex$dFlags.noFlags);
  if ($0.tag === "Left") { return Partial._crashWith($0._1); }
  if ($0.tag === "Right") { return $0._1; }
  $runtime.fail();
})();
const intDecimal = (v1, $0, $1, $2, $3) => {
  const $4 = v1._3;
  const $5 = v1._2;
  return $0(v1$1 => {
    const $6 = (state2, a) => $0(v2 => {
      const v = Data$dInt.fromString(a);
      return (() => {
        if (v.tag === "Nothing") { return Parsing.fail("Expected Int"); }
        if (v.tag === "Just") {
          const $6 = v._1;
          return (state1, v$1, v1$2, v2$1, done) => done(state1, $6);
        }
        $runtime.fail();
      })()(
        v1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2,
        $0,
        $1,
        (v2$1, $6) => $2(Parsing.$ParseState(v2$1._1, v2$1._2, $4), Parsing.$ParseError($6._1, $5)),
        $3
      );
    });
    const $7 = v1._1;
    const $8 = v1._2;
    return $0(v3 => intDecimalRegex(
      Parsing.$ParseState($7, $8, false),
      $0,
      $1,
      (v4, $9) => {
        const $10 = v4._3;
        return $0(v5 => {
          if ($10) { return $2(Parsing.$ParseState(v4._1, v4._2, $4), Parsing.$ParseError($9._1, $5)); }
          return Parsing.fail("Expected Int")(v1, $0, $1, (v2, $11) => $2(Parsing.$ParseState(v2._1, v2._2, $4), Parsing.$ParseError($11._1, $5)), $6);
        });
      },
      $6
    ));
  });
};
const hexDigit = /* #__PURE__ */ Parsing$dCombinators.withErrorMessage(/* #__PURE__ */ satisfyCP(Data$dCodePoint$dUnicode.isHexDigit))("hex digit");
const digit = /* #__PURE__ */ Parsing$dCombinators.withErrorMessage(/* #__PURE__ */ satisfyCP(Data$dCodePoint$dUnicode.isDecDigit))("digit");
const alphaNum = /* #__PURE__ */ Parsing$dCombinators.withErrorMessage(/* #__PURE__ */ satisfyCP(Data$dCodePoint$dUnicode.isAlphaNum))("letter or digit");
export {
  alphaNum,
  digit,
  hexDigit,
  
  
  letter,
  lower,
  
  
  
  
  octDigit,
  oneOf,
  
  
  
  
  
  
  
  
  upper,
  
};
