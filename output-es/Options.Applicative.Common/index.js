import * as $runtime from "../runtime.js";
import * as Control$dApply from "../Control.Apply/index.js";
import * as Control$dBind from "../Control.Bind/index.js";
import * as Control$dMonad$dFree from "../Control.Monad.Free/index.js";
import * as Control$dMonad$dState$dTrans from "../Control.Monad.State.Trans/index.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dCatList from "../Data.CatList/index.js";
import * as Data$dCatQueue from "../Data.CatQueue/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunction from "../Data.Function/index.js";
import * as Data$dList from "../Data.List/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dString$dCodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Options$dApplicative$dInternal from "../Options.Applicative.Internal/index.js";
import * as Options$dApplicative$dInternal$dUtils from "../Options.Applicative.Internal.Utils/index.js";
import * as Options$dApplicative$dTypes from "../Options.Applicative.Types/index.js";
const $OptWord = (_1, _2) => ({tag: "OptWord", _1, _2});
const any = /* #__PURE__ */ (() => Data$dFoldable.foldableArray.foldMap((() => {
  const semigroupDisj1 = {append: v => v1 => v || v1};
  return {mempty: false, Semigroup0: () => semigroupDisj1};
})()))();
const elem = /* #__PURE__ */ (() => {
  const any1 = Data$dFoldable.foldableArray.foldMap((() => {
    const semigroupDisj1 = {append: v => v1 => v || v1};
    return {mempty: false, Semigroup0: () => semigroupDisj1};
  })());
  return x => any1(y => {
    if (x.tag === "OptShort") { return y.tag === "OptShort" && x._1 === y._1; }
    return x.tag === "OptLong" && y.tag === "OptLong" && x._1 === y._1;
  });
})();
const OptWord = value0 => value1 => $OptWord(value0, value1);
const simplify = v => {
  if (v.tag === "Leaf") { return Options$dApplicative$dTypes.$OptTree("Leaf", v._1); }
  if (v.tag === "MultNode") {
    const v1 = Control$dBind.arrayBind(v._1)(x => {
      const $0 = simplify(x);
      if ($0.tag === "MultNode") { return $0._1; }
      return [$0];
    });
    if (v1.length === 1) { return v1[0]; }
    return Options$dApplicative$dTypes.$OptTree("MultNode", v1);
  }
  if (v.tag === "AltNode") {
    const v1 = Control$dBind.arrayBind(v._1)(x => {
      const $0 = simplify(x);
      if ($0.tag === "AltNode") { return $0._1; }
      if ($0.tag === "MultNode" && $0._1.length === 0) { return []; }
      return [$0];
    });
    if (v1.length === 0) { return Options$dApplicative$dTypes.$OptTree("MultNode", []); }
    if (v1.length === 1) { return v1[0]; }
    return Options$dApplicative$dTypes.$OptTree("AltNode", v1);
  }
  $runtime.fail();
};
const showOption = v => {
  if (v.tag === "OptLong") { return "--" + v._1; }
  if (v.tag === "OptShort") { return Data$dString$dCodeUnits.fromCharArray(["-", v._1]); }
  $runtime.fail();
};
const parseWord = /* #__PURE__ */ (() => {
  const $0 = Data$dFoldable.foldrArray(Data$dList$dTypes.Cons)(Data$dList$dTypes.Nil);
  return x => {
    const $1 = $0(Data$dString$dCodeUnits.toCharArray(x));
    if ($1.tag === "Cons" && $1._1 === "-") {
      if ($1._2.tag === "Cons" && $1._2._1 === "-") {
        return Data$dMaybe.$Maybe(
          "Just",
          (() => {
            const v2 = Data$dList.span(v3 => v3 !== "=")($1._2._2);
            if (v2.rest.tag === "Nil") {
              return $OptWord(
                Options$dApplicative$dTypes.$OptName("OptLong", Data$dString$dCodeUnits.fromCharArray(Data$dArray.fromFoldableImpl(Data$dList$dTypes.foldableList.foldr, $1._2._2))),
                Data$dMaybe.Nothing
              );
            }
            if (v2.rest.tag === "Cons") {
              return $OptWord(
                Options$dApplicative$dTypes.$OptName("OptLong", Data$dString$dCodeUnits.fromCharArray(Data$dArray.fromFoldableImpl(Data$dList$dTypes.foldableList.foldr, v2.init))),
                Data$dMaybe.$Maybe("Just", Data$dString$dCodeUnits.fromCharArray(Data$dArray.fromFoldableImpl(Data$dList$dTypes.foldableList.foldr, v2.rest._2)))
              );
            }
            $runtime.fail();
          })()
        );
      }
      if ($1._2.tag === "Nil") { return Data$dMaybe.Nothing; }
      if ($1._2.tag === "Cons") {
        return Data$dMaybe.$Maybe(
          "Just",
          $OptWord(
            Options$dApplicative$dTypes.$OptName("OptShort", $1._2._1),
            $1._2._2.tag !== "Nil"
              ? Data$dMaybe.$Maybe("Just", Data$dString$dCodeUnits.fromCharArray(Data$dArray.fromFoldableImpl(Data$dList$dTypes.foldableList.foldr, $1._2._2)))
              : Data$dMaybe.Nothing
          )
        );
      }
      $runtime.fail();
    }
    return Data$dMaybe.Nothing;
  };
})();
const optionNames = v => {
  if (v.tag === "OptReader") { return v._1; }
  if (v.tag === "FlagReader") { return v._1; }
  return [];
};
const liftOpt = Options$dApplicative$dTypes.OptP;
const isOptionPrefix = v => v1 => {
  if (v.tag === "OptShort") { return v1.tag === "OptShort" && v._1 === v1._1; }
  return v.tag === "OptLong" && v1.tag === "OptLong" && Options$dApplicative$dInternal$dUtils.startsWith(v._1)(v1._1);
};
const optMatches = dictMonadP => {
  const Monad0 = dictMonadP.Monad0();
  const bindStateT = Control$dMonad$dState$dTrans.bindStateT(Monad0);
  const monadStateStateT = Control$dMonad$dState$dTrans.monadStateStateT(Monad0);
  const $$get = monadStateStateT.state(s => Data$dTuple.$Tuple(s, s));
  const $0 = Control$dMonad$dState$dTrans.applicativeStateT(Monad0);
  const $1 = dictMonadP.Monad0().Applicative0().pure;
  return disambiguate => opt => v => {
    if (opt.tag === "OptReader") {
      const $2 = (disambiguate ? any(isOptionPrefix(v._1))(opt._1) : elem(v._1)(opt._1)) ? Data$dMaybe.$Maybe("Just", undefined) : Data$dMaybe.Nothing;
      if ($2.tag === "Just") {
        return Data$dMaybe.$Maybe(
          "Just",
          bindStateT.bind($$get)(args => {
            const missing_arg = dictMonadP.missingArgP(opt._3(showOption(v._1)))(opt._2.crCompleter);
            return bindStateT.bind((() => {
              if (v._2.tag === "Nothing") {
                if (args.tag === "Nil") { return s => Monad0.Bind1().bind(missing_arg)(x => Monad0.Applicative0().pure(Data$dTuple.$Tuple(x, s))); }
                if (args.tag === "Cons") { return $0.pure(Data$dTuple.$Tuple(args._1, args._2)); }
                $runtime.fail();
              }
              if (v._2.tag === "Just") { return $0.pure(Data$dTuple.$Tuple(v._2._1, args)); }
              $runtime.fail();
            })())(v1 => {
              const $3 = v1._1;
              const $4 = v1._2;
              return bindStateT.bind(monadStateStateT.state(v$1 => Data$dTuple.$Tuple(undefined, $4)))(() => {
                const $5 = opt._2.crReader($3);
                if ($5.tag === "Right") {
                  const $6 = $1($5._1);
                  return s => Monad0.Bind1().bind($6)(x => Monad0.Applicative0().pure(Data$dTuple.$Tuple(x, s)));
                }
                if ($5.tag === "Left") {
                  const $6 = dictMonadP.errorP($5._1.tag === "ErrorMsg"
                    ? Options$dApplicative$dTypes.$ParseError("ErrorMsg", "option " + showOption(v._1) + ": " + $5._1._1)
                    : $5._1);
                  return s => Monad0.Bind1().bind($6)(x => Monad0.Applicative0().pure(Data$dTuple.$Tuple(x, s)));
                }
                $runtime.fail();
              });
            });
          })
        );
      }
      if ($2.tag === "Nothing") { return Data$dMaybe.Nothing; }
      $runtime.fail();
    }
    if (
      opt.tag === "FlagReader" && (disambiguate ? any(isOptionPrefix(v._1))(opt._1) : elem(v._1)(opt._1)) && (
        (() => {
          if (v._1.tag === "OptShort") { return true; }
          if (v._1.tag === "OptLong") { return false; }
          $runtime.fail();
        })() || (() => {
          if (v._2.tag === "Nothing") { return true; }
          if (v._2.tag === "Just") { return false; }
          $runtime.fail();
        })()
      )
    ) {
      return Data$dMaybe.$Maybe(
        "Just",
        bindStateT.bind($$get)(args => bindStateT.bind((() => {
          const $2 = v._2.tag === "Just"
            ? Data$dList$dTypes.$List("Cons", Data$dString$dCodeUnits.fromCharArray(["-", ...Data$dString$dCodeUnits.toCharArray(v._2._1)]), args)
            : args;
          return monadStateStateT.state(v$1 => Data$dTuple.$Tuple(undefined, $2));
        })())(() => $0.pure(opt._2)))
      );
    }
    return Data$dMaybe.Nothing;
  };
};
const evalParser = v => {
  if (v.tag === "NilP") { return Data$dMaybe.$Maybe("Just", v._1); }
  if (v.tag === "OptP") { return Data$dMaybe.Nothing; }
  if (v.tag === "MultP") {
    const $0 = evalParser(v._1._1);
    const $1 = evalParser(v._1._2);
    if ($0.tag === "Just") {
      if ($1.tag === "Just") { return Data$dMaybe.$Maybe("Just", $0._1($1._1)); }
      return Data$dMaybe.Nothing;
    }
    if ($0.tag === "Nothing") { return Data$dMaybe.Nothing; }
    $runtime.fail();
  }
  if (v.tag === "AltP") {
    const $0 = evalParser(v._1);
    const $1 = evalParser(v._2);
    if ($0.tag === "Nothing") { return $1; }
    return $0;
  }
  if (v.tag === "BindP") {
    return Control$dMonad$dFree.resume$p(p => k => {
      const $0 = evalParser(p);
      if ($0.tag === "Just") { return evalParser(Options$dApplicative$dTypes.$Parser("BindP", k($0._1))); }
      if ($0.tag === "Nothing") { return Data$dMaybe.Nothing; }
      $runtime.fail();
    })(Data$dMaybe.Just)(v._1);
  }
  $runtime.fail();
};
const searchParser = dictMonad => {
  const nondetTPlus = Options$dApplicative$dInternal.nondetTPlus(dictMonad);
  const empty = nondetTPlus.empty;
  const $0 = Options$dApplicative$dInternal.nondetTFunctor(dictMonad);
  const nondetTAltOp = Options$dApplicative$dInternal.nondetTAltOp(dictMonad);
  const oneOf1 = Data$dFoldable.foldrArray(nondetTPlus.Alt0().alt)(nondetTPlus.empty);
  return v => v1 => {
    if (v1.tag === "NilP") { return empty; }
    if (v1.tag === "OptP") { return v(v1._1); }
    if (v1.tag === "MultP") {
      const $1 = v1._1._1;
      const $2 = v1._1._2;
      return nondetTAltOp($0.map(p1$p => Options$dApplicative$dTypes.$Parser("MultP", Options$dApplicative$dTypes.$MultPE(p1$p, $2)))(searchParser(dictMonad)(v)($1)))($0.map(p2$p => Options$dApplicative$dTypes.$Parser(
        "MultP",
        Options$dApplicative$dTypes.$MultPE($1, p2$p)
      ))(searchParser(dictMonad)(v)($2)));
    }
    if (v1.tag === "AltP") { return oneOf1([searchParser(dictMonad)(v)(v1._1), searchParser(dictMonad)(v)(v1._2)]); }
    if (v1.tag === "BindP") {
      return Control$dMonad$dFree.resume$p(p => k => oneOf1([
        $0.map(p$p => Options$dApplicative$dTypes.$Parser(
          "BindP",
          Control$dMonad$dFree.$Free(
            Control$dMonad$dFree.$FreeView("Bind", p$p, x => Control$dMonad$dFree.$Free(Control$dMonad$dFree.$FreeView("Return", x), Data$dCatList.CatNil)),
            Data$dCatList.$CatList("CatCons", k, Data$dCatQueue.$CatQueue(Data$dList$dTypes.Nil, Data$dList$dTypes.Nil))
          )
        ))(searchParser(dictMonad)(v)(p)),
        (() => {
          const v2 = evalParser(p);
          if (v2.tag === "Nothing") { return empty; }
          if (v2.tag === "Just") { return searchParser(dictMonad)(v)(Options$dApplicative$dTypes.$Parser("BindP", k(v2._1))); }
          $runtime.fail();
        })()
      ]))(v$1 => empty)(v1._1);
    }
    $runtime.fail();
  };
};
const searchOpt = dictMonadP => {
  const $0 = dictMonadP.Monad0();
  const monadStateT = {Applicative0: () => Control$dMonad$dState$dTrans.applicativeStateT($0), Bind1: () => Control$dMonad$dState$dTrans.bindStateT($0)};
  const searchParser1 = searchParser(monadStateT);
  const optMatches1 = optMatches(dictMonadP);
  const lift2 = Options$dApplicative$dInternal.nondetTMonadTrans.lift(monadStateT);
  const $1 = dictMonadP.Alt1().Functor0();
  const empty = Options$dApplicative$dInternal.nondetTPlus(monadStateT).empty;
  return pprefs => w => searchParser1(opt => {
    const v = optMatches1(pprefs.prefDisambiguate && opt.optProps.propVisibility !== "Internal")(opt.optMain)(w);
    if (v.tag === "Just") { return lift2(s => $1.map(v1 => Data$dTuple.$Tuple(Options$dApplicative$dTypes.$Parser("NilP", v1._1), v1._2))(v._1(s))); }
    if (v.tag === "Nothing") { return empty; }
    $runtime.fail();
  });
};
const stepParser = dictMonadP => {
  const searchOpt1 = searchOpt(dictMonadP);
  return v => v1 => v2 => v3 => {
    if (v1 === "AllPositionals") { return searchArg(dictMonadP)(v)(v2)(v3); }
    if (v1 === "ForwardOptions") {
      const v4 = parseWord(v2);
      if (v4.tag === "Just") {
        return Options$dApplicative$dInternal.nondetTAlt((() => {
          const $0 = dictMonadP.Monad0();
          return {Applicative0: () => Control$dMonad$dState$dTrans.applicativeStateT($0), Bind1: () => Control$dMonad$dState$dTrans.bindStateT($0)};
        })()).alt(searchOpt1(v)(v4._1)(v3))(searchArg(dictMonadP)(v)(v2)(v3));
      }
      if (v4.tag === "Nothing") { return searchArg(dictMonadP)(v)(v2)(v3); }
      $runtime.fail();
    }
    const v4 = parseWord(v2);
    if (v4.tag === "Just") { return searchOpt1(v)(v4._1)(v3); }
    if (v4.tag === "Nothing") { return searchArg(dictMonadP)(v)(v2)(v3); }
    $runtime.fail();
  };
};
const searchArg = dictMonadP => {
  const Monad0 = dictMonadP.Monad0();
  const monadStateT = {Applicative0: () => Control$dMonad$dState$dTrans.applicativeStateT(Monad0), Bind1: () => Control$dMonad$dState$dTrans.bindStateT(Monad0)};
  const searchParser1 = searchParser(monadStateT);
  const $0 = Options$dApplicative$dInternal.nondetTApplicative(monadStateT);
  const cut = Options$dApplicative$dInternal.cut(monadStateT);
  const lift2 = Options$dApplicative$dInternal.nondetTMonadTrans.lift(monadStateT);
  const bindStateT = Control$dMonad$dState$dTrans.bindStateT(Monad0);
  const $1 = Control$dMonad$dState$dTrans.applyStateT(Monad0);
  const monadStateStateT = Control$dMonad$dState$dTrans.monadStateStateT(Monad0);
  const $$get = monadStateStateT.state(s => Data$dTuple.$Tuple(s, s));
  const $2 = dictMonadP.Alt1().Functor0();
  const Apply0 = Monad0.Bind1().Apply0();
  const exitContext = dictMonadP.exitContext;
  const $3 = Options$dApplicative$dInternal.nondetTFunctor(monadStateT);
  const empty = Options$dApplicative$dInternal.nondetTPlus(monadStateT).empty;
  const $4 = dictMonadP.Monad0().Applicative0().pure;
  return prefs => arg => searchParser1(opt => Options$dApplicative$dInternal.nondetTBind(monadStateT).bind(opt.optMain.tag === "ArgReader" ? cut : $0.pure())(() => {
    if (opt.optMain.tag === "CmdReader") {
      const $5 = opt.optMain._3(arg);
      if ($5.tag === "Just") {
        if (prefs.prefBacktrack === "NoBacktrack") {
          const $6 = $5._1;
          return lift2(bindStateT.bind($1.apply($1.Functor0().map(Data$dFunction.const)($$get))(monadStateStateT.state(v => Data$dTuple.$Tuple(undefined, Data$dList$dTypes.Nil))))(args => {
            const $7 = Apply0.apply(Apply0.Functor0().map(Data$dFunction.const)(Apply0.apply(Apply0.Functor0().map(v => Control$dApply.identity)(dictMonadP.enterContext(arg)($6)))(runParserInfo(dictMonadP)($6)(args))))(exitContext);
            return s => $2.map(v1 => Data$dTuple.$Tuple(Options$dApplicative$dTypes.$Parser("NilP", v1._1), v1._2))(Monad0.Bind1().bind($7)(x => Monad0.Applicative0().pure(Data$dTuple.$Tuple(
              x,
              s
            ))));
          }));
        }
        if (prefs.prefBacktrack === "Backtrack") {
          const $6 = $5._1;
          return $3.map(Options$dApplicative$dTypes.NilP)(lift2(args => Apply0.apply(Apply0.Functor0().map(Data$dFunction.const)(Apply0.apply(Apply0.Functor0().map(v => Control$dApply.identity)(dictMonadP.enterContext(arg)($6)))(runParser(dictMonadP)($6.infoPolicy)(Options$dApplicative$dTypes.CmdStart)($6.infoParser)(args))))(exitContext)));
        }
        if (prefs.prefBacktrack === "SubparserInline") {
          const $6 = $5._1;
          return lift2(bindStateT.bind((() => {
            const $7 = dictMonadP.enterContext(arg)($6);
            return s => Monad0.Bind1().bind($7)(x => Monad0.Applicative0().pure(Data$dTuple.$Tuple(x, s)));
          })())(() => Control$dMonad$dState$dTrans.applicativeStateT(Monad0).pure($6.infoParser)));
        }
        $runtime.fail();
      }
      if ($5.tag === "Nothing") { return empty; }
      $runtime.fail();
    }
    if (opt.optMain.tag === "ArgReader") {
      const $5 = opt.optMain._1.crReader(arg);
      const $6 = (() => {
        if ($5.tag === "Left") { return dictMonadP.errorP($5._1); }
        if ($5.tag === "Right") { return $4($5._1); }
        $runtime.fail();
      })();
      return $3.map(Options$dApplicative$dTypes.NilP)(lift2(s => Monad0.Bind1().bind($6)(x => Monad0.Applicative0().pure(Data$dTuple.$Tuple(x, s)))));
    }
    return empty;
  }));
};
const runParserInfo = dictMonadP => i => runParserFully(dictMonadP)(i.infoPolicy)(i.infoParser);
const runParserFully = dictMonadP => {
  const Monad0 = dictMonadP.Monad0();
  return policy => p => args => Monad0.Bind1().bind(runParser(dictMonadP)(policy)(Options$dApplicative$dTypes.CmdStart)(p)(args))(v => {
    if (v._2.tag === "Nil") { return Monad0.Applicative0().pure(v._1); }
    if (v._2.tag === "Cons") {
      return dictMonadP.errorP(Options$dApplicative$dTypes.$ParseError(
        "UnexpectedError",
        v._2._1,
        Options$dApplicative$dTypes.$SomeParser(Options$dApplicative$dTypes.$Parser("NilP", undefined))
      ));
    }
    $runtime.fail();
  });
};
const runParser = dictMonadP => {
  const Monad0 = dictMonadP.Monad0();
  const disamb = Options$dApplicative$dInternal.disamb({
    Applicative0: () => Control$dMonad$dState$dTrans.applicativeStateT(Monad0),
    Bind1: () => Control$dMonad$dState$dTrans.bindStateT(Monad0)
  });
  const $0 = Monad0.Bind1();
  const getPrefs = dictMonadP.getPrefs;
  const pure4 = dictMonadP.Monad0().Applicative0().pure;
  return policy => isCmdStart => p => args => {
    const $1 = evalParser(p);
    const result = $1.tag === "Just" ? Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple($1._1, args)) : Data$dMaybe.Nothing;
    if (args.tag === "Nil") { return dictMonadP.exitP(isCmdStart)(policy)(p)(result); }
    if (args.tag === "Cons") {
      if (args._1 === "--" && (policy === "Intersperse" || policy === "NoIntersperse" || policy !== "AllPositionals")) {
        return runParser(dictMonadP)(Options$dApplicative$dTypes.AllPositionals)(Options$dApplicative$dTypes.CmdCont)(p)(args._2);
      }
      const $2 = args._1;
      const $3 = args._2;
      return $0.bind(getPrefs)(prefs => $0.bind(disamb(!prefs.prefDisambiguate)(stepParser(dictMonadP)(prefs)(policy)($2)(p))($3))(v => {
        if (v._1.tag === "Nothing") {
          const $4 = dictMonadP.errorP(Options$dApplicative$dTypes.$ParseError("UnexpectedError", $2, Options$dApplicative$dTypes.$SomeParser(p)));
          if (result.tag === "Nothing") { return $4; }
          if (result.tag === "Just") { return pure4(result._1); }
          $runtime.fail();
        }
        if (v._1.tag === "Just") {
          return runParser(dictMonadP)((() => {
            if (policy === "NoIntersperse") {
              if (
                (() => {
                  const $4 = parseWord($2);
                  if ($4.tag === "Nothing") { return false; }
                  if ($4.tag === "Just") { return true; }
                  $runtime.fail();
                })()
              ) {
                return Options$dApplicative$dTypes.NoIntersperse;
              }
              return Options$dApplicative$dTypes.AllPositionals;
            }
            return policy;
          })())(Options$dApplicative$dTypes.CmdCont)(v._1._1)(v._2);
        }
        $runtime.fail();
      }));
    }
    $runtime.fail();
  };
};
const treeMapParser = g => {
  const hasArg = v => {
    if (v.tag === "NilP") { return false; }
    if (v.tag === "OptP") { return v._1.optMain.tag === "ArgReader"; }
    if (v.tag === "MultP") { return hasArg(v._1._1) || hasArg(v._1._2); }
    if (v.tag === "AltP") { return hasArg(v._1) || hasArg(v._2); }
    if (v.tag === "BindP") { return Control$dMonad$dFree.resume$p(p => v1 => hasArg(p))(v$1 => false)(v._1); }
    $runtime.fail();
  };
  const go = v => v1 => v2 => v3 => v4 => {
    if (v4.tag === "NilP") { return Options$dApplicative$dTypes.$OptTree("MultNode", []); }
    if (v4.tag === "OptP") {
      if (v4._1.optProps.propVisibility !== "Internal") {
        return Options$dApplicative$dTypes.$OptTree("Leaf", v3({hinfoMulti: v, hinfoDefault: v1, hinfoUnreachableArgs: v2})(v4._1));
      }
      return Options$dApplicative$dTypes.$OptTree("MultNode", []);
    }
    if (v4.tag === "MultP") { return Options$dApplicative$dTypes.$OptTree("MultNode", [go(v)(v1)(v2)(v3)(v4._1._1), go(v)(v1)(v2 || hasArg(v4._1._1))(v3)(v4._1._2)]); }
    if (v4.tag === "AltP") {
      const $0 = evalParser(v4._1);
      const d$p = v1 || (() => {
        const $1 = evalParser(v4._2);
        return (() => {
          if ($0.tag === "Nothing") { return false; }
          if ($0.tag === "Just") { return true; }
          $runtime.fail();
        })() || (() => {
          if ($1.tag === "Nothing") { return false; }
          if ($1.tag === "Just") { return true; }
          $runtime.fail();
        })();
      })();
      return Options$dApplicative$dTypes.$OptTree("AltNode", [go(v)(d$p)(v2)(v3)(v4._1), go(v)(d$p)(v2)(v3)(v4._2)]);
    }
    if (v4.tag === "BindP") {
      return Control$dMonad$dFree.resume$p(p => k => {
        const go$p = go(true)(v1)(v2)(v3)(p);
        const v5 = evalParser(p);
        if (v5.tag === "Nothing") { return go$p; }
        if (v5.tag === "Just") { return Options$dApplicative$dTypes.$OptTree("MultNode", [go$p, go(true)(v1)(v2)(v3)(Options$dApplicative$dTypes.$Parser("BindP", k(v5._1)))]); }
        $runtime.fail();
      })(v$1 => Options$dApplicative$dTypes.$OptTree("MultNode", []))(v4._1);
    }
    $runtime.fail();
  };
  const $0 = go(false)(false)(false)(g);
  return x => simplify($0(x));
};
const mapParser = f => {
  const flatten = v => {
    if (v.tag === "Leaf") { return [v._1]; }
    if (v.tag === "MultNode") { return Control$dBind.arrayBind(v._1)(flatten); }
    if (v.tag === "AltNode") { return Control$dBind.arrayBind(v._1)(flatten); }
    $runtime.fail();
  };
  const $0 = treeMapParser(f);
  return x => flatten($0(x));
};
export {
  $OptWord,
  OptWord,
  any,
  elem,
  evalParser,
  isOptionPrefix,
  liftOpt,
  mapParser,
  optMatches,
  optionNames,
  parseWord,
  runParser,
  runParserFully,
  runParserInfo,
  searchArg,
  searchOpt,
  searchParser,
  showOption,
  simplify,
  stepParser,
  treeMapParser
};
