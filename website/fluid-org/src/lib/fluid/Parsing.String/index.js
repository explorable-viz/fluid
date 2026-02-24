// | Primitive parsers, combinators and functions for working with an input
// | stream of type `String`.
// |
// | All of these primitive parsers will consume when they succeed.
// |
// | All of these primitive parsers will not consume and will automatically
// | backtrack when they fail.
// |
// | The behavior of these primitive parsers is based on the behavior of the
// | `Data.String` module in the __strings__ package.
// | In most JavaScript runtime environments, the `String`
// | is little-endian [UTF-16](https://en.wikipedia.org/wiki/UTF-16).
// |
// | The primitive parsers which return `Char` will only succeed when the character
// | being parsed is a code point in the
// | [Basic Multilingual Plane](https://en.wikipedia.org/wiki/Plane_(Unicode)#Basic_Multilingual_Plane)
// | (the “BMP”). These parsers can be convenient because of the good support
// | that PureScript has for writing `Char` literals like `'あ'`, `'β'`, `'C'`.
// |
// | The other primitive parsers, which return `CodePoint` and `String` types,
// | can parse the full Unicode character set. All of the primitive parsers
// | in this module can be used together.
// |
// | ### Position
// |
// | In a `String` parser, the `Position {index}` counts the number of
// | unicode `CodePoint`s since the beginning of the input string.
// |
// | Each tab character (`0x09`) encountered in a `String` parser will advance
// | the `Position {column}` by 8.
// |
// | These patterns will advance the `Position {line}` by 1 and reset
// | the `Position {column}` to 1:
// | - newline (`0x0A`)
// | - carriage-return (`0x0D`)
// | - carriage-return-newline (`0x0D 0x0A`)
import * as $runtime from "../runtime.js";
import * as Control$dMonad$dRec$dClass from "../Control.Monad.Rec.Class/index.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dEnum from "../Data.Enum/index.js";
import * as Data$dEuclideanRing from "../Data.EuclideanRing/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dString$dCodePoints from "../Data.String.CodePoints/index.js";
import * as Data$dString$dCodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data$dString$dCommon from "../Data.String.Common/index.js";
import * as Data$dString$dRegex from "../Data.String.Regex/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Parsing$dCombinators from "../Parsing.Combinators/index.js";
const max = x => y => {
  const v = Data$dOrd.ordInt.compare(x)(y);
  if (v === "LT") { return y; }
  if (v === "EQ") { return x; }
  if (v === "GT") { return x; }
  $runtime.fail();
};
const min = x => y => {
  const v = Data$dOrd.ordInt.compare(x)(y);
  if (v === "LT") { return x; }
  if (v === "EQ") { return x; }
  if (v === "GT") { return y; }
  $runtime.fail();
};
const updatePosSingle = v => cp => after => {
  if (cp === 10) { return {index: v.index + 1 | 0, line: v.line + 1 | 0, column: 1}; }
  if (cp === 13) {
    const v2 = Data$dString$dCodePoints.codePointAt(0)(after);
    if (v2.tag === "Just" && v2._1 === 10) { return {index: v.index + 1 | 0, line: v.line, column: v.column}; }
    return {index: v.index + 1 | 0, line: v.line + 1 | 0, column: 1};
  }
  if (cp === 9) { return {index: v.index + 1 | 0, line: v.line, column: (v.column + 8 | 0) - Data$dEuclideanRing.intMod(v.column - 1 | 0)(8) | 0}; }
  return {index: v.index + 1 | 0, line: v.line, column: v.column + 1 | 0};
};
const updatePosString = updatePosString$a0$copy => updatePosString$a1$copy => updatePosString$a2$copy => {
  let updatePosString$a0 = updatePosString$a0$copy;
  let updatePosString$a1 = updatePosString$a1$copy;
  let updatePosString$a2 = updatePosString$a2$copy;
  let updatePosString$c = true;
  let updatePosString$r;
  while (updatePosString$c) {
    const pos = updatePosString$a0, before = updatePosString$a1, after = updatePosString$a2;
    const v = Data$dString$dCodePoints.uncons(before);
    if (v.tag === "Nothing") {
      updatePosString$c = false;
      updatePosString$r = pos;
      continue;
    }
    if (v.tag === "Just") {
      updatePosString$a0 = v._1.tail === "" ? updatePosSingle(pos)(v._1.head)(after) : updatePosSingle(pos)(v._1.head)(v._1.tail);
      updatePosString$a1 = v._1.tail;
      updatePosString$a2 = after;
      continue;
    }
    $runtime.fail();
  }
  return updatePosString$r;
};
const satisfyCodePoint = f => (v, $0, $1, $2, $3) => {
  const v3 = Data$dString$dCodePoints.uncons(v._1);
  if (v3.tag === "Nothing") { return $2(v, Parsing.$ParseError("Unexpected EOF", v._2)); }
  if (v3.tag === "Just") {
    if (f(v3._1.head)) { return $3(Parsing.$ParseState(v3._1.tail, updatePosSingle(v._2)(v3._1.head)(v3._1.tail), true), v3._1.head); }
    return $2(v, Parsing.$ParseError("Predicate unsatisfied", v._2));
  }
  $runtime.fail();
};
const satisfy = f => (v, $0, $1, $2, $3) => {
  const v3 = Data$dString$dCodePoints.uncons(v._1);
  if (v3.tag === "Nothing") { return $2(v, Parsing.$ParseError("Unexpected EOF", v._2)); }
  if (v3.tag === "Just") {
    if (v3._1.head < 0 || v3._1.head > 65535) { return $2(v, Parsing.$ParseError("Expected Char", v._2)); }
    if (v3._1.head >= 0 && v3._1.head <= 65535) {
      const ch = Data$dEnum.fromCharCode(v3._1.head);
      if (f(ch)) { return $3(Parsing.$ParseState(v3._1.tail, updatePosSingle(v._2)(v3._1.head)(v3._1.tail), true), ch); }
      return $2(v, Parsing.$ParseError("Predicate unsatisfied", v._2));
    }
  }
  $runtime.fail();
};
const parseErrorHuman = input => contextSize => v => {
  const $0 = v._2.index;
  const go = go$a0$copy => go$a1$copy => go$a2$copy => go$a3$copy => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$a2 = go$a2$copy, go$a3 = go$a3$copy, go$c = true, go$r;
    while (go$c) {
      const posBegin = go$a0, lineBegin = go$a1, posEnd = go$a2, lineEnd = go$a3;
      const v2 = Data$dString$dCodePoints.uncons(lineEnd);
      if (v2.tag === "Just") {
        if (v2._1.head === 10) {
          if (posEnd === $0) {
            go$c = false;
            go$r = {posBegin, posEnd: posEnd + 1 | 0, lineBegin};
            continue;
          }
          if (posEnd > $0) {
            go$c = false;
            go$r = {posBegin, posEnd, lineBegin};
            continue;
          }
          go$a0 = posEnd + 1 | 0;
          go$a1 = v2._1.tail;
          go$a2 = posEnd + 1 | 0;
          go$a3 = v2._1.tail;
          continue;
        }
        if (v2._1.head === 13) {
          if (posEnd === $0) {
            go$c = false;
            go$r = {posBegin, posEnd: posEnd + 1 | 0, lineBegin};
            continue;
          }
          if (posEnd > $0) {
            go$c = false;
            go$r = {posBegin, posEnd, lineBegin};
            continue;
          }
          go$a0 = posEnd + 1 | 0;
          go$a1 = v2._1.tail;
          go$a2 = posEnd + 1 | 0;
          go$a3 = v2._1.tail;
          continue;
        }
        go$a0 = posBegin;
        go$a1 = lineBegin;
        go$a2 = posEnd + 1 | 0;
        go$a3 = v2._1.tail;
        continue;
      }
      go$c = false;
      go$r = {posBegin, posEnd, lineBegin};
    }
    return go$r;
  };
  const v1 = go(0)(input)(0)(input);
  const lineSelect = Data$dString$dCodePoints.take(v1.posEnd - v1.posBegin | 0)(v1.lineBegin);
  const lineLength = Data$dString$dCodePoints.toCodePointArray(lineSelect).length;
  const lineIndex = $0 - v1.posBegin | 0;
  const bestPosBefore = lineIndex - $runtime.intDiv(contextSize, 2) | 0;
  const bestPosAfter = (lineIndex + $runtime.intDiv(contextSize, 2) | 0) + ((contextSize & 1) !== 0 ? 1 : 0) | 0;
  if (bestPosBefore >= 0) {
    if (bestPosAfter <= lineLength) {
      return [
        v._1 + " at position index:" + Data$dShow.showIntImpl($0) + " (line:" + Data$dShow.showIntImpl(v._2.line) + ", column:" + Data$dShow.showIntImpl(v._2.column) + ")",
        Data$dString$dCommon.joinWith("")(Data$dArray.replicateImpl(lineIndex - bestPosBefore | 0, " ")) + "▼",
        Data$dString$dCodePoints.take(bestPosAfter - bestPosBefore | 0)(Data$dString$dCodeUnits.drop(Data$dString$dCodeUnits.length(Data$dString$dCodePoints.take(bestPosBefore)(lineSelect)))(lineSelect))
      ];
    }
    return [
      v._1 + " at position index:" + Data$dShow.showIntImpl($0) + " (line:" + Data$dShow.showIntImpl(v._2.line) + ", column:" + Data$dShow.showIntImpl(v._2.column) + ")",
      Data$dString$dCommon.joinWith("")(Data$dArray.replicateImpl(lineIndex - max(0)(lineLength - contextSize | 0) | 0, " ")) + "▼",
      Data$dString$dCodePoints.take(lineLength - max(0)(lineLength - contextSize | 0) | 0)(Data$dString$dCodeUnits.drop(Data$dString$dCodeUnits.length(Data$dString$dCodePoints.take(max(0)(lineLength - contextSize | 0))(lineSelect)))(lineSelect))
    ];
  }
  return [
    v._1 + " at position index:" + Data$dShow.showIntImpl($0) + " (line:" + Data$dShow.showIntImpl(v._2.line) + ", column:" + Data$dShow.showIntImpl(v._2.column) + ")",
    Data$dString$dCommon.joinWith("")(Data$dArray.replicateImpl(lineIndex - 0 | 0, " ")) + "▼",
    Data$dString$dCodePoints.take(min(lineLength)(contextSize) - 0 | 0)(Data$dString$dCodeUnits.drop(Data$dString$dCodeUnits.length(Data$dString$dCodePoints.take(0)(lineSelect)))(lineSelect))
  ];
};
const match = p => (state1, more, lift1, $$throw, done) => more(v1 => Parsing.getParserT(
  state1,
  more,
  lift1,
  $$throw,
  (state2, a) => more(v2 => {
    const $0 = a._1;
    const $1 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
    return more(v1$1 => p(
      $1,
      more,
      lift1,
      $$throw,
      (state2$1, a$1) => more(v2$1 => {
        const $2 = $1._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1;
        return more(v1$2 => Parsing.getParserT(
          $2,
          more,
          lift1,
          $$throw,
          (state2$2, a$2) => more(v2$2 => done(
            $2._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2,
            Data$dTuple.$Tuple(Data$dString$dCodeUnits.take(Data$dString$dCodeUnits.length($0) - Data$dString$dCodeUnits.length(a$2._1) | 0)($0), a$1)
          ))
        ));
      })
    ));
  })
));
const eof = (v, $0, $1, $2, $3) => {
  if (v._1 === "") { return $3(Parsing.$ParseState(v._1, v._2, true), undefined); }
  return $2(v, Parsing.$ParseError("Expected EOF", v._2));
};
const consumeWith = f => (v, $0, $1, $2, $3) => {
  const v3 = f(v._1);
  if (v3.tag === "Left") { return $2(v, Parsing.$ParseError(v3._1, v._2)); }
  if (v3.tag === "Right") { return $3(Parsing.$ParseState(v3._1.remainder, updatePosString(v._2)(v3._1.consumed)(v3._1.remainder), v3._1.consumed !== ""), v3._1.value); }
  $runtime.fail();
};
const regex = pattern => flags => {
  const $0 = Data$dString$dRegex.regex("^(" + pattern + ")")(flags);
  if ($0.tag === "Left") { return Data$dEither.$Either("Left", $0._1); }
  if ($0.tag === "Right") {
    return Data$dEither.$Either(
      "Right",
      (() => {
        const $1 = $0._1;
        return consumeWith(input => {
          const $2 = Data$dString$dRegex.match($1)(input);
          if (
            $2.tag === "Just" && (() => {
              if (0 < $2._1.length) { return $2._1[0].tag === "Just"; }
              $runtime.fail();
            })()
          ) {
            return Data$dEither.$Either(
              "Right",
              {
                value: (() => {
                  if (0 < $2._1.length) { return $2._1[0]._1; }
                  $runtime.fail();
                })(),
                consumed: (() => {
                  if (0 < $2._1.length) { return $2._1[0]._1; }
                  $runtime.fail();
                })(),
                remainder: Data$dString$dCodeUnits.drop(Data$dString$dCodeUnits.length((() => {
                  if (0 < $2._1.length) { return $2._1[0]._1; }
                  $runtime.fail();
                })()))(input)
              }
            );
          }
          return Data$dEither.$Either("Left", "No Regex pattern match");
        });
      })()
    );
  }
  $runtime.fail();
};
const rest = /* #__PURE__ */ consumeWith(consumed => Data$dEither.$Either("Right", {value: consumed, consumed, remainder: ""}));
const string = str => consumeWith(input => {
  const v = Data$dString$dCodeUnits.stripPrefix(str)(input);
  if (v.tag === "Just") { return Data$dEither.$Either("Right", {value: str, consumed: str, remainder: v._1}); }
  return Data$dEither.$Either("Left", "Expected " + Data$dShow.showStringImpl(str));
});
const takeN = n => consumeWith(input => {
  const v = Data$dString$dCodePoints.splitAt(n)(input);
  if (Data$dString$dCodePoints.toCodePointArray(v.before).length === n) { return Data$dEither.$Either("Right", {value: v.before, consumed: v.before, remainder: v.after}); }
  return Data$dEither.$Either("Left", "Could not take " + Data$dShow.showIntImpl(n) + " characters");
});
const $$char = c => Parsing$dCombinators.withErrorMessage(satisfy(v => v === c))(Data$dShow.showCharImpl(c));
const anyCodePoint = /* #__PURE__ */ satisfyCodePoint(v => true);
const anyTill = dictMonad => p => (state1, more, lift1, $$throw, done) => more(v1 => Parsing.getParserT(
  state1,
  more,
  lift1,
  $$throw,
  (state2, a) => more(v2 => {
    const $0 = a._1;
    const $1 = Parsing.monadRecParserT.tailRecM(unit => (v2$1, $1, $2, $3, $4) => {
      const $5 = v2$1._1;
      const $6 = v2$1._2;
      return $1(v3 => {
        const $7 = (v4, $7) => {
          const $8 = v4._3;
          return $1(v5 => {
            if ($8) { return $3(v4, $7); }
            return $1(v1$1 => anyCodePoint(
              v2$1,
              $1,
              $2,
              $3,
              (state2$1, a$1) => $1(v2$2 => $4(
                v2$1._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1,
                Control$dMonad$dRec$dClass.$Step("Loop", unit)
              ))
            ));
          });
        };
        return $1(v1$1 => Parsing.getParserT(
          Parsing.$ParseState($5, $6, false),
          $1,
          $2,
          $7,
          (state2$1, a$1) => $1(v2$2 => {
            const $8 = a$1._1;
            return $1(v1$2 => {
              const $9 = state2$1._3;
              return p(
                state2$1,
                $1,
                $2,
                (v2$3, $10) => $7(Parsing.$ParseState(v2$3._1, v2$3._2, $9), $10),
                (state2$2, a$2) => $1(v2$3 => $4(
                  state2$1._3 && !state2$2._3 ? Parsing.$ParseState(state2$2._1, state2$2._2, true) : state2$2,
                  Control$dMonad$dRec$dClass.$Step("Done", Data$dTuple.$Tuple($8, a$2))
                ))
              );
            });
          })
        ));
      });
    })();
    const $2 = state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2;
    return more(v1$1 => $1(
      $2,
      more,
      lift1,
      $$throw,
      (state2$1, a$1) => more(v2$1 => done(
        $2._3 && !state2$1._3 ? Parsing.$ParseState(state2$1._1, state2$1._2, true) : state2$1,
        Data$dTuple.$Tuple(Data$dString$dCodeUnits.take(Data$dString$dCodeUnits.length($0) - Data$dString$dCodeUnits.length(a$1._1) | 0)($0), a$1._2)
      ))
    ));
  })
));
const anyChar = /* #__PURE__ */ satisfy(v => true);
export {
  
  
  
  $$char as char,
  consumeWith,
  eof,
  
  
  
  
  regex,
  
  satisfy,
  satisfyCodePoint,
  string,
  
  
  
};
