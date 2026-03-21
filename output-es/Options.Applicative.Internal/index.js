import * as $runtime from "../runtime.js";
import * as Control$dMonad$dExcept$dTrans from "../Control.Monad.Except.Trans/index.js";
import * as Control$dMonad$dReader$dTrans from "../Control.Monad.Reader.Trans/index.js";
import * as Control$dMonad$dState$dTrans from "../Control.Monad.State.Trans/index.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dIdentity from "../Data.Identity/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Options$dApplicative$dTypes from "../Options.Applicative.Types/index.js";
const $ComplResult = (tag, _1, _2) => ({tag, _1, _2});
const $TStep = (tag, _1, _2) => ({tag, _1, _2});
const monadReaderT = /* #__PURE__ */ Control$dMonad$dReader$dTrans.monadReaderT(Data$dIdentity.monadIdentity);
const monadStateT = {Applicative0: () => Control$dMonad$dState$dTrans.applicativeStateT(monadReaderT), Bind1: () => Control$dMonad$dState$dTrans.bindStateT(monadReaderT)};
const apply = /* #__PURE__ */ (() => Control$dMonad$dExcept$dTrans.applyExceptT(monadStateT).apply)();
const bind = /* #__PURE__ */ (() => Control$dMonad$dExcept$dTrans.bindExceptT(monadStateT).bind)();
const pure = /* #__PURE__ */ (() => Control$dMonad$dExcept$dTrans.applicativeExceptT(monadStateT).pure)();
const alt = /* #__PURE__ */ (() => Control$dMonad$dExcept$dTrans.altExceptT(Options$dApplicative$dTypes.parseErrorSemigroup)(monadStateT).alt)();
const lift1 = m => Control$dMonad$dState$dTrans.bindStateT(monadReaderT).bind(m)(a => Control$dMonad$dState$dTrans.applicativeStateT(monadReaderT).pure(Data$dEither.$Either(
  "Right",
  a
)));
const modify_ = /* #__PURE__ */ (() => {
  const $0 = Control$dMonad$dState$dTrans.monadStateStateT(monadReaderT);
  return f => $0.state(s => Data$dTuple.$Tuple(undefined, f(s)));
})();
const throwError = /* #__PURE__ */ (() => Control$dMonad$dExcept$dTrans.monadThrowExceptT(monadStateT).throwError)();
const TNil = /* #__PURE__ */ $TStep("TNil");
const TCons = value0 => value1 => $TStep("TCons", value0, value1);
const ComplParser = value0 => value1 => $ComplResult("ComplParser", value0, value1);
const ComplOption = value0 => $ComplResult("ComplOption", value0);
const ComplResult = value0 => $ComplResult("ComplResult", value0);
const withReadM = f => x => x$1 => {
  const $0 = x(x$1);
  if ($0.tag === "Right") { return Data$dEither.$Either("Right", $0._1); }
  if ($0.tag === "Left") { return Data$dEither.$Either("Left", $0._1.tag === "ErrorMsg" ? Options$dApplicative$dTypes.$ParseError("ErrorMsg", f($0._1._1)) : $0._1); }
  $runtime.fail();
};
const runP = v => v([]);
const runListT = dictMonad => xs => dictMonad.Bind1().bind(xs)(s => {
  if (s.tag === "TNil") { return dictMonad.Applicative0().pure(Data$dList$dTypes.Nil); }
  if (s.tag === "TCons") {
    const $0 = Data$dList$dTypes.Cons(s._1);
    return dictMonad.Bind1().bind(runListT(dictMonad)(s._2))(a$p => dictMonad.Applicative0().pure($0(a$p)));
  }
  $runtime.fail();
});
const runCompletion = v => prefs => {
  const v1 = v(prefs);
  if (v1.tag === "ComplResult") { return Data$dMaybe.Nothing; }
  if (v1.tag === "ComplParser") { return Data$dMaybe.$Maybe("Just", Data$dEither.$Either("Left", Data$dTuple.$Tuple(v1._1, v1._2))); }
  if (v1.tag === "ComplOption") { return Data$dMaybe.$Maybe("Just", Data$dEither.$Either("Right", v1._1)); }
  $runtime.fail();
};
const pFunctor = {
  map: f => v => s => {
    const $0 = v(s);
    return x => {
      const $1 = $0(x);
      return Data$dTuple.$Tuple(
        (() => {
          if ($1._1.tag === "Left") { return Data$dEither.$Either("Left", $1._1._1); }
          if ($1._1.tag === "Right") { return Data$dEither.$Either("Right", f($1._1._1)); }
          $runtime.fail();
        })(),
        $1._2
      );
    };
  }
};
const pApply = {apply: v => v1 => apply(v)(v1), Functor0: () => pFunctor};
const pBind = {bind: v => k => bind(v)(a => k(a)), Apply0: () => pApply};
const pApplicative = {pure: a => pure(a), Apply0: () => pApply};
const pMonad = {Applicative0: () => pApplicative, Bind1: () => pBind};
const pAlt = {alt: v => v1 => alt(v)(v1), Functor0: () => pFunctor};
const missingArgP = dict => dict.missingArgP;
const getPrefs = dict => dict.getPrefs;
const exitP = dict => dict.exitP;
const exitContext = dict => dict.exitContext;
const errorP = dict => dict.errorP;
const hoistEither = dictMonadP => {
  const $0 = dictMonadP.Monad0().Applicative0().pure;
  return v2 => {
    if (v2.tag === "Left") { return dictMonadP.errorP(v2._1); }
    if (v2.tag === "Right") { return $0(v2._1); }
    $runtime.fail();
  };
};
const runReadM = dictMonadP => {
  const $0 = dictMonadP.Monad0().Applicative0().pure;
  return v => s => {
    const $1 = v(s);
    if ($1.tag === "Left") { return dictMonadP.errorP($1._1); }
    if ($1.tag === "Right") { return $0($1._1); }
    $runtime.fail();
  };
};
const hoistMaybe = dictMonadP => {
  const pure4 = dictMonadP.Monad0().Applicative0().pure;
  return err => {
    const $0 = dictMonadP.errorP(err);
    return v2 => {
      if (v2.tag === "Nothing") { return $0; }
      if (v2.tag === "Just") { return pure4(v2._1); }
      $runtime.fail();
    };
  };
};
const pMonadP$lazy = /* #__PURE__ */ $runtime.binding(() => (
  {
    enterContext: name => pinfo => lift1(modify_(Data$dArray.cons(Options$dApplicative$dTypes.$Context(name, pinfo)))),
    exitContext: lift1(modify_(Data$dArray.drop(1))),
    getPrefs: lift1(s => monadReaderT.Bind1().bind(Data$dIdentity.Identity)(x => monadReaderT.Applicative0().pure(Data$dTuple.$Tuple(x, s)))),
    missingArgP: e => v => pMonadP$lazy().errorP(e),
    exitP: i => v => p => {
      const $0 = throwError(Options$dApplicative$dTypes.$ParseError("MissingError", i, Options$dApplicative$dTypes.$SomeParser(p)));
      return x => {
        if (x.tag === "Nothing") { return $0; }
        if (x.tag === "Just") { return pure(x._1); }
        $runtime.fail();
      };
    },
    errorP: x => throwError(x),
    Monad0: () => pMonad,
    Alt1: () => pAlt
  }
));
const pMonadP = /* #__PURE__ */ pMonadP$lazy();
const enterContext = dict => dict.enterContext;
const contextNames = ns => Data$dArray.reverse(Data$dFunctor.arrayMap(v => v._1)(ns));
const complResultMonad = {Applicative0: () => complResultApplicative, Bind1: () => complResultBind};
const complResultFunctor = {map: f => a => complResultBind.bind(a)(a$p => complResultApplicative.pure(f(a$p)))};
const complResultBind = {
  bind: m => f => {
    if (m.tag === "ComplResult") { return f(m._1); }
    if (m.tag === "ComplParser") { return $ComplResult("ComplParser", m._1, m._2); }
    if (m.tag === "ComplOption") { return $ComplResult("ComplOption", m._1); }
    $runtime.fail();
  },
  Apply0: () => complResultApply
};
const complResultApply = {
  apply: f => a => {
    if (f.tag === "ComplResult") {
      if (a.tag === "ComplResult") { return complResultApplicative.pure(f._1(a._1)); }
      if (a.tag === "ComplParser") { return $ComplResult("ComplParser", a._1, a._2); }
      if (a.tag === "ComplOption") { return $ComplResult("ComplOption", a._1); }
      $runtime.fail();
    }
    if (f.tag === "ComplParser") { return $ComplResult("ComplParser", f._1, f._2); }
    if (f.tag === "ComplOption") { return $ComplResult("ComplOption", f._1); }
    $runtime.fail();
  },
  Functor0: () => complResultFunctor
};
const complResultApplicative = {pure: ComplResult, Apply0: () => complResultApply};
const monadReaderT1 = /* #__PURE__ */ Control$dMonad$dReader$dTrans.monadReaderT(complResultMonad);
const alt1 = /* #__PURE__ */ (() => Control$dMonad$dExcept$dTrans.altExceptT(Options$dApplicative$dTypes.parseErrorSemigroup)(monadReaderT1).alt)();
const apply1 = /* #__PURE__ */ (() => Control$dMonad$dExcept$dTrans.applyExceptT(monadReaderT1).apply)();
const pure2 = /* #__PURE__ */ (() => Control$dMonad$dExcept$dTrans.applicativeExceptT(monadReaderT1).pure)();
const bind1 = /* #__PURE__ */ (() => Control$dMonad$dExcept$dTrans.bindExceptT(monadReaderT1).bind)();
const lift3 = m => monadReaderT1.Bind1().bind(m)(a => monadReaderT1.Applicative0().pure(Data$dEither.$Either("Right", a)));
const completionFunctor = {
  map: f => v => x => {
    const $0 = v(x);
    if ($0.tag === "ComplResult") {
      return $ComplResult(
        "ComplResult",
        (() => {
          if ($0._1.tag === "Left") { return Data$dEither.$Either("Left", $0._1._1); }
          if ($0._1.tag === "Right") { return Data$dEither.$Either("Right", f($0._1._1)); }
          $runtime.fail();
        })()
      );
    }
    if ($0.tag === "ComplParser") { return $ComplResult("ComplParser", $0._1, $0._2); }
    if ($0.tag === "ComplOption") { return $ComplResult("ComplOption", $0._1); }
    $runtime.fail();
  }
};
const completionAlt = {alt: v => v1 => alt1(v)(v1), Functor0: () => completionFunctor};
const completionApply = {apply: v => v1 => apply1(v)(v1), Functor0: () => completionFunctor};
const completionApplicative = {pure: a => pure2(a), Apply0: () => completionApply};
const completionBind = {bind: v => k => bind1(v)(a => k(a)), Apply0: () => completionApply};
const completionMonad = {Applicative0: () => completionApplicative, Bind1: () => completionBind};
const completionMonadP = {
  enterContext: v => v1 => pure2(),
  exitContext: /* #__PURE__ */ pure2(),
  getPrefs: /* #__PURE__ */ lift3(ComplResult),
  missingArgP: v => x => lift3(v$1 => $ComplResult("ComplOption", x)),
  exitP: v => a => p => v1 => lift3(v$1 => $ComplResult("ComplParser", Options$dApplicative$dTypes.$SomeParser(p), a)),
  errorP: x => Control$dMonad$dExcept$dTrans.monadThrowExceptT(monadReaderT1).throwError(x),
  Monad0: () => completionMonad,
  Alt1: () => completionAlt
};
const listTFunctor = dictMonad => (
  {
    map: f => v => {
      const $0 = listTFunctor(dictMonad).map(f);
      return dictMonad.Bind1().bind(v)(a$p => dictMonad.Applicative0().pure((() => {
        if (a$p.tag === "TNil") { return TNil; }
        if (a$p.tag === "TCons") { return $TStep("TCons", f(a$p._1), $0(a$p._2)); }
        $runtime.fail();
      })()));
    }
  }
);
const listTAlt = dictMonad => {
  const listTFunctor1 = listTFunctor(dictMonad);
  return {
    alt: xs => ys => dictMonad.Bind1().bind(xs)(s => {
      if (s.tag === "TNil") { return ys; }
      if (s.tag === "TCons") { return dictMonad.Applicative0().pure($TStep("TCons", s._1, listTAlt(dictMonad).alt(s._2)(ys))); }
      $runtime.fail();
    }),
    Functor0: () => listTFunctor1
  };
};
const listTPlus = dictMonad => {
  const listTAlt1 = listTAlt(dictMonad);
  return {empty: dictMonad.Applicative0().pure(TNil), Alt0: () => listTAlt1};
};
const hoistList = dictMonad => Data$dFoldable.foldrArray(x => xt => dictMonad.Applicative0().pure($TStep("TCons", x, xt)))(listTPlus(dictMonad).empty);
const listTMonadTrans = {
  lift: dictMonad => {
    const empty = listTPlus(dictMonad).empty;
    return x => dictMonad.Bind1().bind(x)(a$p => dictMonad.Applicative0().pure($TStep("TCons", a$p, empty)));
  }
};
const cut = dictMonad => listTMonadTrans.lift({
  Applicative0: () => Control$dMonad$dState$dTrans.applicativeStateT(dictMonad),
  Bind1: () => Control$dMonad$dState$dTrans.bindStateT(dictMonad)
})(Control$dMonad$dState$dTrans.monadStateStateT(dictMonad).state(v => Data$dTuple.$Tuple(undefined, true)));
const nondetTMonadTrans = {
  lift: dictMonad => {
    const $0 = listTMonadTrans.lift({Applicative0: () => Control$dMonad$dState$dTrans.applicativeStateT(dictMonad), Bind1: () => Control$dMonad$dState$dTrans.bindStateT(dictMonad)});
    return x => $0(s => dictMonad.Bind1().bind(x)(x$1 => dictMonad.Applicative0().pure(Data$dTuple.$Tuple(x$1, s))));
  }
};
const listTMonad = dictMonad => ({Applicative0: () => listTApplicative(dictMonad), Bind1: () => listTBind(dictMonad)});
const listTBind = dictMonad => (
  {
    bind: xs => f => dictMonad.Bind1().bind(xs)(s => {
      if (s.tag === "TNil") { return dictMonad.Applicative0().pure(TNil); }
      if (s.tag === "TCons") { return listTAlt(dictMonad).alt(f(s._1))(listTBind(dictMonad).bind(s._2)(f)); }
      $runtime.fail();
    }),
    Apply0: () => listTApply(dictMonad)
  }
);
const listTApply = dictMonad => {
  const listTFunctor1 = listTFunctor(dictMonad);
  return {
    apply: (() => {
      const $0 = listTBind(dictMonad);
      return f => a => $0.bind(f)(f$p => $0.bind(a)(a$p => listTApplicative(dictMonad).pure(f$p(a$p))));
    })(),
    Functor0: () => listTFunctor1
  };
};
const listTApplicative = dictMonad => (
  {
    pure: (() => {
      const $0 = hoistList(dictMonad);
      return x => $0([x]);
    })(),
    Apply0: () => listTApply(dictMonad)
  }
);
const listTAlternative = dictMonad => {
  const listTApplicative1 = listTApplicative(dictMonad);
  const listTPlus1 = listTPlus(dictMonad);
  return {Applicative0: () => listTApplicative1, Plus1: () => listTPlus1};
};
const listTMonadPlus = dictMonad => {
  const listTMonad1 = {Applicative0: () => listTApplicative(dictMonad), Bind1: () => listTBind(dictMonad)};
  const listTAlternative1 = listTAlternative(dictMonad);
  return {Monad0: () => listTMonad1, Alternative1: () => listTAlternative1};
};
const nondetTAltOp = dictMonad => {
  const monadStateT1 = {Applicative0: () => Control$dMonad$dState$dTrans.applicativeStateT(dictMonad), Bind1: () => Control$dMonad$dState$dTrans.bindStateT(dictMonad)};
  const listTBind1 = listTBind(monadStateT1);
  const lift6 = listTMonadTrans.lift(monadStateT1);
  const $$get = Control$dMonad$dState$dTrans.monadStateStateT(dictMonad).state(s => Data$dTuple.$Tuple(s, s));
  const $0 = listTAlternative(monadStateT1);
  const empty = $0.Plus1().empty;
  return m1 => m2 => listTAlt(monadStateT1).alt(m1)(listTBind1.bind(lift6($$get))(s => listTBind1.bind(!s ? $0.Applicative0().pure() : empty)(() => m2)));
};
const nondetTFunctor = dictMonad => (
  {map: f => listTFunctor({Applicative0: () => Control$dMonad$dState$dTrans.applicativeStateT(dictMonad), Bind1: () => Control$dMonad$dState$dTrans.bindStateT(dictMonad)}).map(f)}
);
const nondetTAlt = dictMonad => {
  const nondetTFunctor1 = nondetTFunctor(dictMonad);
  return {
    alt: v => v1 => listTAlt({Applicative0: () => Control$dMonad$dState$dTrans.applicativeStateT(dictMonad), Bind1: () => Control$dMonad$dState$dTrans.bindStateT(dictMonad)}).alt(v)(v1),
    Functor0: () => nondetTFunctor1
  };
};
const nondetTPlus = dictMonad => {
  const nondetTAlt1 = nondetTAlt(dictMonad);
  return {
    empty: listTPlus({Applicative0: () => Control$dMonad$dState$dTrans.applicativeStateT(dictMonad), Bind1: () => Control$dMonad$dState$dTrans.bindStateT(dictMonad)}).empty,
    Alt0: () => nondetTAlt1
  };
};
const nondetTApply = dictMonad => {
  const nondetTFunctor1 = nondetTFunctor(dictMonad);
  return {
    apply: v => v1 => listTApply({Applicative0: () => Control$dMonad$dState$dTrans.applicativeStateT(dictMonad), Bind1: () => Control$dMonad$dState$dTrans.bindStateT(dictMonad)}).apply(v)(v1),
    Functor0: () => nondetTFunctor1
  };
};
const nondetTApplicative = dictMonad => {
  const nondetTApply1 = nondetTApply(dictMonad);
  return {
    pure: x => listTApplicative({Applicative0: () => Control$dMonad$dState$dTrans.applicativeStateT(dictMonad), Bind1: () => Control$dMonad$dState$dTrans.bindStateT(dictMonad)}).pure(x),
    Apply0: () => nondetTApply1
  };
};
const nondetTAlternative = dictMonad => {
  const nondetTApplicative1 = nondetTApplicative(dictMonad);
  const nondetTPlus1 = nondetTPlus(dictMonad);
  return {Applicative0: () => nondetTApplicative1, Plus1: () => nondetTPlus1};
};
const nondetTBind = dictMonad => {
  const nondetTApply1 = nondetTApply(dictMonad);
  return {
    bind: v => f => listTBind({Applicative0: () => Control$dMonad$dState$dTrans.applicativeStateT(dictMonad), Bind1: () => Control$dMonad$dState$dTrans.bindStateT(dictMonad)}).bind(v)(x => f(x)),
    Apply0: () => nondetTApply1
  };
};
const nondetTMonad = dictMonad => {
  const nondetTApplicative1 = nondetTApplicative(dictMonad);
  const nondetTBind1 = nondetTBind(dictMonad);
  return {Applicative0: () => nondetTApplicative1, Bind1: () => nondetTBind1};
};
const nondetTMonadPlus = dictMonad => {
  const nondetTMonad1 = nondetTMonad(dictMonad);
  const nondetTAlternative1 = nondetTAlternative(dictMonad);
  return {Monad0: () => nondetTMonad1, Alternative1: () => nondetTAlternative1};
};
const takeListT = dictMonad => {
  const empty = listTPlus(dictMonad).empty;
  return v => {
    if (v === 0) { return v$1 => empty; }
    const $0 = takeListT(dictMonad)(v - 1 | 0);
    return x => dictMonad.Bind1().bind(x)(a$p => dictMonad.Applicative0().pure((() => {
      if (a$p.tag === "TNil") { return TNil; }
      if (a$p.tag === "TCons") { return $TStep("TCons", a$p._1, $0(a$p._2)); }
      $runtime.fail();
    })()));
  };
};
const disamb = dictMonad => {
  const Bind1 = dictMonad.Bind1();
  const evalStateT = Control$dMonad$dState$dTrans.evalStateT(Bind1.Apply0().Functor0());
  const monadStateT1 = {Applicative0: () => Control$dMonad$dState$dTrans.applicativeStateT(dictMonad), Bind1: () => Control$dMonad$dState$dTrans.bindStateT(dictMonad)};
  const takeListT1 = takeListT(monadStateT1);
  return allow_amb => xs => Bind1.bind(evalStateT(runListT(monadStateT1)(takeListT1(allow_amb ? 1 : 2)(xs)))(false))(xs$p => dictMonad.Applicative0().pure(xs$p.tag === "Cons" && xs$p._2.tag === "Nil"
    ? Data$dMaybe.$Maybe("Just", xs$p._1)
    : Data$dMaybe.Nothing));
};
export {
  $ComplResult,
  $TStep,
  ComplOption,
  ComplParser,
  ComplResult,
  TCons,
  TNil,
  alt,
  alt1,
  apply,
  apply1,
  bind,
  bind1,
  complResultApplicative,
  complResultApply,
  complResultBind,
  complResultFunctor,
  complResultMonad,
  completionAlt,
  completionApplicative,
  completionApply,
  completionBind,
  completionFunctor,
  completionMonad,
  completionMonadP,
  contextNames,
  cut,
  disamb,
  enterContext,
  errorP,
  exitContext,
  exitP,
  getPrefs,
  hoistEither,
  hoistList,
  hoistMaybe,
  lift1,
  lift3,
  listTAlt,
  listTAlternative,
  listTApplicative,
  listTApply,
  listTBind,
  listTFunctor,
  listTMonad,
  listTMonadPlus,
  listTMonadTrans,
  listTPlus,
  missingArgP,
  modify_,
  monadReaderT,
  monadReaderT1,
  monadStateT,
  nondetTAlt,
  nondetTAltOp,
  nondetTAlternative,
  nondetTApplicative,
  nondetTApply,
  nondetTBind,
  nondetTFunctor,
  nondetTMonad,
  nondetTMonadPlus,
  nondetTMonadTrans,
  nondetTPlus,
  pAlt,
  pApplicative,
  pApply,
  pBind,
  pFunctor,
  pMonad,
  pMonadP,
  pure,
  pure2,
  runCompletion,
  runListT,
  runP,
  runReadM,
  takeListT,
  throwError,
  withReadM
};
