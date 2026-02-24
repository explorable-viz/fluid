// | These combinators will produce `Array`s, as opposed to the other combinators
// | of the same names in the __Parsing.Combinators__ module
// | which mostly produce `List`s. These `Array` combinators will run in a bit
// | less time (*~85% runtime*) than the similar `List` combinators, and they will run in a
// | lot less time (*~10% runtime*) than the similar combinators in __Data.Array__.
// |
// | If there is some other combinator which returns
// | a `List` but we want an `Array`, and there is no `Array` version of the
// | combinator in this module, then we can rely on the
// | [__`Data.Array.fromFoldable`__](https://pursuit.purescript.org/packages/purescript-arrays/docs/Data.Array#v:fromFoldable)
// | function for a pretty fast transformation from `List` to `Array`.
import * as Control$dMonad$dRec$dClass from "../Control.Monad.Rec.Class/index.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Parsing from "../Parsing/index.js";
const manyTill_ = p => end => {
  const $0 = Parsing.monadRecParserT.tailRecM(xs => (v2, $0, $1, $2, $3) => {
    const $4 = v2._1;
    const $5 = v2._2;
    return $0(v3 => $0(v1 => end(
      Parsing.$ParseState($4, $5, false),
      $0,
      $1,
      (v4, $6) => {
        const $7 = v4._3;
        return $0(v5 => {
          if ($7) { return $2(v4, $6); }
          return $0(v1$1 => p(
            v2,
            $0,
            $1,
            $2,
            (state2, a) => $0(v2$1 => $3(
              v2._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2,
              Control$dMonad$dRec$dClass.$Step("Loop", Data$dList$dTypes.$List("Cons", a, xs))
            ))
          ));
        });
      },
      (state2, a) => $0(v2$1 => $3(state2, Control$dMonad$dRec$dClass.$Step("Done", Data$dTuple.$Tuple(xs, a))))
    )));
  })(Data$dList$dTypes.Nil);
  return (state1, more, lift1, $$throw, done) => more(v1 => $0(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2 => done(
      state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2,
      Data$dTuple.$Tuple(Data$dArray.reverse(Data$dArray.fromFoldableImpl(Data$dList$dTypes.foldableList.foldr, a._1)), a._2)
    ))
  ));
};
const manyIndex = from => to => p => {
  if (from > to || from < 0) { return (state1, v, v1, v2, done) => done(state1, Data$dTuple.$Tuple(0, [])); }
  const $0 = Parsing.monadRecParserT.tailRecM(v => {
    if (v._1 >= to) { return (state1, v$1, v1, v2, done) => done(state1, Control$dMonad$dRec$dClass.$Step("Done", Data$dTuple.$Tuple(v._1, v._2))); }
    const $0 = p(v._1);
    return (state1, more, lift1, $$throw, done) => more(v1 => more(v1$1 => $0(
      state1,
      more,
      lift1,
      (state2, err) => {
        if (v._1 >= from) { return done(state2, Control$dMonad$dRec$dClass.$Step("Done", Data$dTuple.$Tuple(v._1, v._2))); }
        return $$throw(state2, Parsing.$ParseError(err._1 + " (at least " + Data$dShow.showIntImpl(from) + ", but only parsed " + Data$dShow.showIntImpl(v._1) + ")", err._2));
      },
      (state2, a) => more(v2 => done(
        state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2,
        Control$dMonad$dRec$dClass.$Step("Loop", Data$dTuple.$Tuple(v._1 + 1 | 0, Data$dList$dTypes.$List("Cons", a, v._2)))
      ))
    )));
  })(Data$dTuple.$Tuple(0, Data$dList$dTypes.Nil));
  return (state1, more, lift1, $$throw, done) => more(v1 => $0(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2 => done(
      state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2,
      Data$dTuple.$Tuple(a._1, Data$dArray.reverse(Data$dArray.fromFoldableImpl(Data$dList$dTypes.foldableList.foldr, a._2)))
    ))
  ));
};
const many = p => {
  const $0 = Parsing.monadRecParserT.tailRecM(xs => (v2, $0, $1, $2, $3) => {
    const $4 = v2._1;
    const $5 = v2._2;
    return $0(v3 => $0(v1 => p(
      Parsing.$ParseState($4, $5, false),
      $0,
      $1,
      (v4, $6) => {
        const $7 = v4._3;
        return $0(v5 => {
          if ($7) { return $2(v4, $6); }
          return $3(v2, Control$dMonad$dRec$dClass.$Step("Done", xs));
        });
      },
      (state2, a) => $0(v2$1 => $3(state2, Control$dMonad$dRec$dClass.$Step("Loop", Data$dList$dTypes.$List("Cons", a, xs))))
    )));
  })(Data$dList$dTypes.Nil);
  return (state1, more, lift1, $$throw, done) => more(v1 => $0(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2 => done(
      state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2,
      Data$dArray.reverse(Data$dArray.fromFoldableImpl(Data$dList$dTypes.foldableList.foldr, a))
    ))
  ));
};
const many1 = p => {
  const $0 = many(p);
  return (state1, more, lift1, $$throw, done) => more(v1 => $0(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2 => (a.length > 0 ? ((state1$1, v, v1$1, v2$1, done$1) => done$1(state1$1, a)) : Parsing.fail("Expected at least 1"))(
      state1._3 && !state2._3 ? Parsing.$ParseState(state2._1, state2._2, true) : state2,
      more,
      lift1,
      $$throw,
      done
    ))
  ));
};
export {many,   };
