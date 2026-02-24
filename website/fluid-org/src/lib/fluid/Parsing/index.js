// | Types and operations for monadic parsing.
// |
// | Combinators are in the `Parsing.Combinators` module.
// |
// | Primitive parsers for `String` input streams are in the `Parsing.String`
// | module.
import * as $runtime from "../runtime.js";
import * as Control$dMonad$dRec$dClass from "../Control.Monad.Rec.Class/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dLazy from "../Data.Lazy/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dShow$dGeneric from "../Data.Show.Generic/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const $ParseError = (_1, _2) => ({tag: "ParseError", _1, _2});
const $ParseState = (_1, _2, _3) => ({tag: "ParseState", _1, _2, _3});
const $RunParser = (tag, _1, _2) => ({tag, _1, _2});
const Position = x => x;
const ParseState = value0 => value1 => value2 => $ParseState(value0, value1, value2);
const ParseError = value0 => value1 => $ParseError(value0, value1);
const ParserT = x => x;
const More = value0 => $RunParser("More", value0);
const Lift = value0 => $RunParser("Lift", value0);
const Stop = value0 => value1 => $RunParser("Stop", value0, value1);
const monadTransParserT = {lift: dictMonad => m => (state1, v, lift$p, v1, done) => lift$p(dictMonad.Bind1().Apply0().Functor0().map(a => v2 => done(state1, a))(m))};
const lazyParserT = {
  defer: f => {
    const m = Data$dLazy.defer(f);
    return (state1, more, lift1, $$throw, done) => Data$dLazy.force(m)(state1, more, lift1, $$throw, done);
  }
};
const genericPosition_ = {to: x => x, from: x => x};
const genericShow = /* #__PURE__ */ (() => {
  const $0 = Data$dShow$dGeneric.genericShowConstructor({
    genericShowArgs: v => ["{ column: " + Data$dShow.showIntImpl(v.column) + ", index: " + Data$dShow.showIntImpl(v.index) + ", line: " + Data$dShow.showIntImpl(v.line) + " }"]
  })({reflectSymbol: () => "Position"});
  return x => $0["genericShow'"](x);
})();
const showPosition = {show: x => genericShow(x)};
const functorParserT = {map: f => v => (state1, more, lift1, $$throw, done) => more(v1 => v(state1, more, lift1, $$throw, (state2, a) => more(v2 => done(state2, f(a)))))};
const eqPosition = {eq: v => v1 => v.index === v1.index};
const ordPosition = {compare: v => v1 => Data$dOrd.ordInt.compare(v.index)(v1.index), Eq0: () => eqPosition};
const altParserT = {
  alt: v => v1 => (v2, $0, $1, $2, $3) => {
    const $4 = v2._1;
    const $5 = v2._2;
    return $0(v3 => v(
      $ParseState($4, $5, false),
      $0,
      $1,
      (v4, $6) => {
        const $7 = v4._3;
        return $0(v5 => {
          if ($7) { return $2(v4, $6); }
          return v1(v2, $0, $1, $2, $3);
        });
      },
      $3
    ));
  },
  Functor0: () => functorParserT
};
const stateParserT = k => (state1, v, v1, v2, done) => {
  const v3 = k(state1);
  return done(v3._2, v3._1);
};
const showParseError = {show: v => "(ParseError " + Data$dShow.showStringImpl(v._1) + " " + genericShow(v._2) + ")"};
const runParserT$p = dictMonadRec => {
  const Monad0 = dictMonadRec.Monad0();
  return state1 => v => {
    const go = go$a0$copy => {
      let go$a0 = go$a0$copy, go$c = true, go$r;
      while (go$c) {
        const step = go$a0;
        const v1 = step();
        if (v1.tag === "More") {
          go$a0 = v1._1;
          continue;
        }
        if (v1.tag === "Lift") {
          go$c = false;
          go$r = Monad0.Bind1().Apply0().Functor0().map(Control$dMonad$dRec$dClass.Loop)(v1._1);
          continue;
        }
        if (v1.tag === "Stop") {
          go$c = false;
          go$r = Monad0.Applicative0().pure(Control$dMonad$dRec$dClass.$Step("Done", Data$dTuple.$Tuple(v1._2, v1._1)));
          continue;
        }
        $runtime.fail();
      }
      return go$r;
    };
    return dictMonadRec.tailRecM(go)(v1 => v(
      state1,
      More,
      Lift,
      (state2, err) => $RunParser("Stop", state2, Data$dEither.$Either("Left", err)),
      (state2, res) => $RunParser("Stop", state2, Data$dEither.$Either("Right", res))
    ));
  };
};
const position = (state1, v, v1, v2, done) => done(state1, state1._2);
const parseErrorPosition = v => v._2;
const parseErrorMessage = v => v._1;
const mapParserT = dictMonadRec => {
  const runParserT$p1 = runParserT$p(dictMonadRec);
  return dictFunctor => f => p => (state1, v, lift1, $$throw, done) => lift1(dictFunctor.map(v1 => {
    const $0 = v1._1;
    const $1 = v1._2;
    return v2 => {
      if ($0.tag === "Left") { return $$throw($1, $0._1); }
      if ($0.tag === "Right") { return done($1, $0._1); }
      $runtime.fail();
    };
  })(f(runParserT$p1(state1)(p))));
};
const initialPos = {index: 0, line: 1, column: 1};
const runParserT = dictMonadRec => {
  const runParserT$p1 = runParserT$p(dictMonadRec);
  return s => p => dictMonadRec.Monad0().Bind1().Apply0().Functor0().map(Data$dTuple.fst)(runParserT$p1($ParseState(s, initialPos, false))(p));
};
const runParserT1 = /* #__PURE__ */ runParserT(Control$dMonad$dRec$dClass.monadRecIdentity);
const runParser = s => runParserT1(s);
const hoistParserT = f => v => (state1, more, lift1, $$throw, done) => v(state1, more, x => lift1(f(x)), $$throw, done);
const getParserT = (state1, v, v1, v2, done) => done(state1, state1);
const eqPositionFull = v => v1 => v.index === v1.index && v.line === v1.line && v.column === v1.column;
const eqParseError = {eq: x => y => x._1 === y._1 && x._2.index === y._2.index};
const ordParseError = {
  compare: x => y => {
    const v = Data$dOrd.ordString.compare(x._1)(y._1);
    if (v === "LT") { return Data$dOrdering.LT; }
    if (v === "GT") { return Data$dOrdering.GT; }
    return Data$dOrd.ordInt.compare(x._2.index)(y._2.index);
  },
  Eq0: () => eqParseError
};
const consume = (state1, v, v1, v2, done) => done($ParseState(state1._1, state1._2, true), undefined);
const applyParserT = {
  apply: v => v1 => (state1, more, lift1, $$throw, done) => more(v2 => v(
    state1,
    more,
    lift1,
    $$throw,
    (state2, f) => more(v3 => {
      const state2$p = state1._3 && !state2._3 ? $ParseState(state2._1, state2._2, true) : state2;
      return v1(state2$p, more, lift1, $$throw, (state3, a) => more(v4 => done(state2$p._3 && !state3._3 ? $ParseState(state3._1, state3._2, true) : state3, f(a))));
    })
  )),
  Functor0: () => functorParserT
};
const applicativeParserT = {pure: a => (state1, v, v1, v2, done) => done(state1, a), Apply0: () => applyParserT};
const semigroupParserT = dictSemigroup => (
  {
    append: a => b => (state1, more, lift1, $$throw, done) => more(v2 => more(v1 => a(
      state1,
      more,
      lift1,
      $$throw,
      (state2, a$1) => more(v2$1 => {
        const $0 = dictSemigroup.append(a$1);
        return more(v3 => {
          const state2$p = state1._3 && !state2._3 ? $ParseState(state2._1, state2._2, true) : state2;
          return b(state2$p, more, lift1, $$throw, (state3, a$2) => more(v4 => done(state2$p._3 && !state3._3 ? $ParseState(state3._1, state3._2, true) : state3, $0(a$2))));
        });
      })
    )))
  }
);
const monoidParserT = dictMonoid => {
  const semigroupParserT1 = semigroupParserT(dictMonoid.Semigroup0());
  return {
    mempty: (() => {
      const $0 = dictMonoid.mempty;
      return (state1, v, v1, v2, done) => done(state1, $0);
    })(),
    Semigroup0: () => semigroupParserT1
  };
};
const bindParserT = {
  bind: v => next => (state1, more, lift1, $$throw, done) => more(v1 => v(
    state1,
    more,
    lift1,
    $$throw,
    (state2, a) => more(v2 => next(a)(state1._3 && !state2._3 ? $ParseState(state2._1, state2._2, true) : state2, more, lift1, $$throw, done))
  )),
  Apply0: () => applyParserT
};
const monadParserT = {Applicative0: () => applicativeParserT, Bind1: () => bindParserT};
const monadAskParserT = dictMonadAsk => (
  {
    ask: (() => {
      const $0 = dictMonadAsk.Monad0();
      const $1 = dictMonadAsk.ask;
      return (state1, v, lift$p, v1, done) => lift$p($0.Bind1().Apply0().Functor0().map(a => v2 => done(state1, a))($1));
    })(),
    Monad0: () => monadParserT
  }
);
const monadReaderParserT = dictMonadReader => {
  const monadAskParserT1 = monadAskParserT(dictMonadReader.MonadAsk0());
  return {
    local: f => v => (state1, more, lift1, $$throw, done) => {
      const $0 = dictMonadReader.local(f);
      return v(state1, more, x => lift1($0(x)), $$throw, done);
    },
    MonadAsk0: () => monadAskParserT1
  };
};
const monadStateParserT = dictMonadState => {
  const $0 = dictMonadState.Monad0();
  return {
    state: k => {
      const $1 = dictMonadState.state(k);
      return (state1, v, lift$p, v1, done) => lift$p($0.Bind1().Apply0().Functor0().map(a => v2 => done(state1, a))($1));
    },
    Monad0: () => monadParserT
  };
};
const monadThrowParseErrorParse = {throwError: err => (state1, v, v1, $$throw, v2) => $$throw(state1, err), Monad0: () => monadParserT};
const failWithPosition = message => pos => (state1, v, v1, $$throw, v2) => $$throw(state1, $ParseError(message, pos));
const fail = message => (state1, more, lift1, $$throw, done) => more(v1 => position(
  state1,
  more,
  lift1,
  $$throw,
  (state2, a) => more(v2 => $$throw(state1._3 && !state2._3 ? $ParseState(state2._1, state2._2, true) : state2, $ParseError(message, a)))
));
const liftEither = dictMonad => f => {
  if (f.tag === "Left") { return fail(f._1); }
  if (f.tag === "Right") {
    const $0 = f._1;
    return (state1, v, v1, v2, done) => done(state1, $0);
  }
  $runtime.fail();
};
const liftExceptT = dictMonad => f => (state1, more, lift1, $$throw, done) => more(v1 => lift1(dictMonad.Bind1().Apply0().Functor0().map(a => v2 => more(v2$1 => (() => {
  if (a.tag === "Left") { return fail(a._1); }
  if (a.tag === "Right") {
    const $0 = a._1;
    return (state1$1, v, v1$1, v2$2, done$1) => done$1(state1$1, $0);
  }
  $runtime.fail();
})()(state1._3 && !state1._3 ? $ParseState(state1._1, state1._2, true) : state1, more, lift1, $$throw, done)))(f)));
const liftMaybe = dictMonad => message => f => {
  if (f.tag === "Nothing") { return fail(message()); }
  if (f.tag === "Just") {
    const $0 = f._1;
    return (state1, v, v1, v2, done) => done(state1, $0);
  }
  $runtime.fail();
};
const plusParserT = {empty: /* #__PURE__ */ fail("No alternative"), Alt0: () => altParserT};
const alternativeParserT = {Applicative0: () => applicativeParserT, Plus1: () => plusParserT};
const monadPlusParserT = {Monad0: () => monadParserT, Alternative1: () => alternativeParserT};
const monadErrorParseErrorParse = {
  catchError: v => next => (state1, more, lift1, $$throw, done) => more(v1 => v(state1, more, lift1, (state2, err) => next(err)(state2, more, lift1, $$throw, done), done)),
  MonadThrow0: () => monadThrowParseErrorParse
};
const region = context => p => (state1, more, lift1, $$throw, done) => more(v1 => p(state1, more, lift1, (state2, err) => $$throw(state2, context(err)), done));
const monadRecParserT = {
  tailRecM: next => initArg => (state1, more, lift1, $$throw, done) => {
    const loop = (state2, arg, gas) => next(arg)(
      state2,
      more,
      lift1,
      $$throw,
      (state3, step) => {
        const state3$p = state2._3 && !state3._3 ? $ParseState(state3._1, state3._2, true) : state3;
        if (step.tag === "Loop") {
          if (gas === 0) { return more(v1 => loop(state3$p, step._1, 30)); }
          return loop(state3$p, step._1, gas - 1 | 0);
        }
        if (step.tag === "Done") { return done(state3$p, step._1); }
        $runtime.fail();
      }
    );
    return loop(state1, initArg, 30);
  },
  Monad0: () => monadParserT
};
export {
  $ParseError,
  $ParseState,
  
  
  
  
  
  
  
  
  altParserT,
  alternativeParserT,
  
  
  
  consume,
  
  
  
  fail,
  
  
  
  
  getParserT,
  
  initialPos,
  lazyParserT,
  
  
  
  
  
  
  
  
  
  monadRecParserT,
  
  
  
  
  
  
  
  
  
  position,
  
  
  runParserT,
  
  
  
  
  
  
};
