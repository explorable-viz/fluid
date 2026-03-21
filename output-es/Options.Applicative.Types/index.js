import * as $runtime from "../runtime.js";
import * as Control$dMonad$dExcept$dTrans from "../Control.Monad.Except.Trans/index.js";
import * as Control$dMonad$dFree from "../Control.Monad.Free/index.js";
import * as Control$dMonad$dReader$dTrans from "../Control.Monad.Reader.Trans/index.js";
import * as Control$dMonad$dRec$dClass from "../Control.Monad.Rec.Class/index.js";
import * as Data$dCatList from "../Data.CatList/index.js";
import * as Data$dCatQueue from "../Data.CatQueue/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dGeneric$dRep from "../Data.Generic.Rep/index.js";
import * as Data$dIdentity from "../Data.Identity/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNonEmpty from "../Data.NonEmpty/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dShow$dGeneric from "../Data.Show.Generic/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Options$dApplicative$dHelp$dChunk from "../Options.Applicative.Help.Chunk/index.js";
import * as Text$dPrettyPrint$dLeijen from "../Text.PrettyPrint.Leijen/index.js";
const $ArgPolicy = tag => tag;
const $Backtracking = tag => tag;
const $Context = (_1, _2) => ({tag: "Context", _1, _2});
const $IsCmdStart = tag => tag;
const $MultPE = (_1, _2) => ({tag: "MultPE", _1, _2});
const $OptName = (tag, _1) => ({tag, _1});
const $OptReader = (tag, _1, _2, _3) => ({tag, _1, _2, _3});
const $OptTree = (tag, _1) => ({tag, _1});
const $OptVisibility = tag => tag;
const $ParseError = (tag, _1, _2) => ({tag, _1, _2});
const $Parser = (tag, _1, _2) => ({tag, _1, _2});
const $ParserResult = (tag, _1) => ({tag, _1});
const $SomeParser = _1 => ({tag: "SomeParser", _1});
const apply = /* #__PURE__ */ (() => {
  const $0 = Control$dMonad$dExcept$dTrans.applyExceptT(Data$dIdentity.monadIdentity);
  return v => v1 => r => $0.apply(v(r))(v1(r));
})();
const bind = /* #__PURE__ */ (() => Control$dMonad$dReader$dTrans.bindReaderT(Control$dMonad$dExcept$dTrans.bindExceptT(Data$dIdentity.monadIdentity)).bind)();
const LeafIsSymbol = {reflectSymbol: () => "Leaf"};
const MultNodeIsSymbol = {reflectSymbol: () => "MultNode"};
const AltNodeIsSymbol = {reflectSymbol: () => "AltNode"};
const showMaybe = {
  show: v => {
    if (v.tag === "Just") { return "(Just " + Data$dShow.showStringImpl(v._1) + ")"; }
    if (v.tag === "Nothing") { return "Nothing"; }
    $runtime.fail();
  }
};
const SuccessIsSymbol = {reflectSymbol: () => "Success"};
const ParserFailure = x => x;
const Internal = /* #__PURE__ */ $OptVisibility("Internal");
const Hidden = /* #__PURE__ */ $OptVisibility("Hidden");
const Visible = /* #__PURE__ */ $OptVisibility("Visible");
const Leaf = value0 => $OptTree("Leaf", value0);
const MultNode = value0 => $OptTree("MultNode", value0);
const AltNode = value0 => $OptTree("AltNode", value0);
const OptProperties = x => x;
const OptShort = value0 => $OptName("OptShort", value0);
const OptLong = value0 => $OptName("OptLong", value0);
const OptHelpInfo = x => x;
const CmdStart = /* #__PURE__ */ $IsCmdStart("CmdStart");
const CmdCont = /* #__PURE__ */ $IsCmdStart("CmdCont");
const CompletionResult = x => x;
const Success = value0 => $ParserResult("Success", value0);
const Failure = value0 => $ParserResult("Failure", value0);
const CompletionInvoked = value0 => $ParserResult("CompletionInvoked", value0);
const Completer = x => x;
const Backtrack = /* #__PURE__ */ $Backtracking("Backtrack");
const NoBacktrack = /* #__PURE__ */ $Backtracking("NoBacktrack");
const SubparserInline = /* #__PURE__ */ $Backtracking("SubparserInline");
const ParserPrefs = x => x;
const Intersperse = /* #__PURE__ */ $ArgPolicy("Intersperse");
const NoIntersperse = /* #__PURE__ */ $ArgPolicy("NoIntersperse");
const AllPositionals = /* #__PURE__ */ $ArgPolicy("AllPositionals");
const ForwardOptions = /* #__PURE__ */ $ArgPolicy("ForwardOptions");
const ParserInfo = x => x;
const NilP = value0 => $Parser("NilP", value0);
const OptP = value0 => $Parser("OptP", value0);
const MultP = value0 => $Parser("MultP", value0);
const AltP = value0 => value1 => $Parser("AltP", value0, value1);
const BindP = value0 => $Parser("BindP", value0);
const Option = x => x;
const OptReader = value0 => value1 => value2 => $OptReader("OptReader", value0, value1, value2);
const FlagReader = value0 => value1 => $OptReader("FlagReader", value0, value1);
const ArgReader = value0 => $OptReader("ArgReader", value0);
const CmdReader = value0 => value1 => value2 => $OptReader("CmdReader", value0, value1, value2);
const CReader = x => x;
const ReadM = x => x;
const ErrorMsg = value0 => $ParseError("ErrorMsg", value0);
const InfoMsg = value0 => $ParseError("InfoMsg", value0);
const ShowHelpText = /* #__PURE__ */ $ParseError("ShowHelpText");
const MissingError = value0 => value1 => $ParseError("MissingError", value0, value1);
const ExpectsArgError = value0 => $ParseError("ExpectsArgError", value0);
const UnexpectedError = value0 => value1 => $ParseError("UnexpectedError", value0, value1);
const SomeParser = value0 => $SomeParser(value0);
const MultPE = value0 => value1 => $MultPE(value0, value1);
const Context = value0 => value1 => $Context(value0, value1);
const readerAsk = /* #__PURE__ */ (() => Control$dMonad$dExcept$dTrans.applicativeExceptT(Data$dIdentity.monadIdentity).pure)();
const readerAbort = x => {
  const $0 = Control$dMonad$dExcept$dTrans.monadThrowExceptT(Data$dIdentity.monadIdentity).throwError(x);
  return v => $0;
};
const readerError = x => {
  const $0 = Control$dMonad$dExcept$dTrans.monadThrowExceptT(Data$dIdentity.monadIdentity).throwError($ParseError("ErrorMsg", x));
  return v => $0;
};
const readMNewtype = {Coercible0: () => {}};
const readMFunctor = {
  map: f => v => x => {
    const $0 = v(x);
    if ($0.tag === "Left") { return Data$dEither.$Either("Left", $0._1); }
    if ($0.tag === "Right") { return Data$dEither.$Either("Right", f($0._1)); }
    $runtime.fail();
  }
};
const readMApply = {apply: v => v1 => apply(v)(v1), Functor0: () => readMFunctor};
const readMBind = {bind: v => f => bind(v)(x => f(x)), Apply0: () => readMApply};
const readMApplicative = {
  pure: /* #__PURE__ */ (() => {
    const $0 = Control$dMonad$dExcept$dTrans.applicativeExceptT(Data$dIdentity.monadIdentity);
    return x => {
      const $1 = $0.pure(x);
      return v => $1;
    };
  })(),
  Apply0: () => readMApply
};
const readMMonad = {Applicative0: () => readMApplicative, Bind1: () => readMBind};
const readMMonadFail = {throwError: readerError, Monad0: () => readMMonad};
const parserResultGeneric = {
  to: x => {
    if (x.tag === "Inl") { return $ParserResult("Success", x._1); }
    if (x.tag === "Inr") {
      if (x._1.tag === "Inl") { return $ParserResult("Failure", x._1._1); }
      if (x._1.tag === "Inr") { return $ParserResult("CompletionInvoked", x._1._1); }
    }
    $runtime.fail();
  },
  from: x => {
    if (x.tag === "Success") { return Data$dGeneric$dRep.$Sum("Inl", x._1); }
    if (x.tag === "Failure") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", x._1)); }
    if (x.tag === "CompletionInvoked") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", x._1)); }
    $runtime.fail();
  }
};
const parserResultFunctor = {
  map: v => v1 => {
    if (v1.tag === "Success") { return $ParserResult("Success", v(v1._1)); }
    if (v1.tag === "Failure") { return $ParserResult("Failure", v1._1); }
    if (v1.tag === "CompletionInvoked") { return $ParserResult("CompletionInvoked", v1._1); }
    $runtime.fail();
  }
};
const parserResultApply = {
  apply: v => v1 => {
    if (v.tag === "Success") {
      if (v1.tag === "Success") { return $ParserResult("Success", v._1(v1._1)); }
      if (v1.tag === "Failure") { return $ParserResult("Failure", v1._1); }
      if (v1.tag === "CompletionInvoked") { return $ParserResult("CompletionInvoked", v1._1); }
      $runtime.fail();
    }
    if (v.tag === "Failure") { return $ParserResult("Failure", v._1); }
    if (v.tag === "CompletionInvoked") { return $ParserResult("CompletionInvoked", v._1); }
    $runtime.fail();
  },
  Functor0: () => parserResultFunctor
};
const parserResultBind = {
  bind: v => v1 => {
    if (v.tag === "Success") { return v1(v._1); }
    if (v.tag === "Failure") { return $ParserResult("Failure", v._1); }
    if (v.tag === "CompletionInvoked") { return $ParserResult("CompletionInvoked", v._1); }
    $runtime.fail();
  },
  Apply0: () => parserResultApply
};
const parserResultApplicative = {pure: Success, Apply0: () => parserResultApply};
const parserResultMonad = {Applicative0: () => parserResultApplicative, Bind1: () => parserResultBind};
const parserPrefsNewtype = {Coercible0: () => {}};
const parserPrefsGeneric = {to: x => x, from: x => x};
const parserInfoNewtype = {Coercible0: () => {}};
const parserFailureShow = {show: v => "(ParserFailure <function>)"};
const parserFailureFunctor = {
  map: f => v => progn => {
    const v1 = v(progn);
    return Data$dTuple.$Tuple(f(v1._1), Data$dTuple.$Tuple(v1._2._1, Data$dTuple.$Tuple(v1._2._2._1, v1._2._2._2)));
  }
};
const parseErrorSemigroup = {append: v => m => m};
const alt = /* #__PURE__ */ (() => {
  const $0 = Control$dMonad$dExcept$dTrans.altExceptT(parseErrorSemigroup)(Data$dIdentity.monadIdentity);
  return v => v1 => r => $0.alt(v(r))(v1(r));
})();
const readMAlt = {alt: v => v1 => alt(v)(v1), Functor0: () => readMFunctor};
const overFailure = v => v1 => {
  if (v1.tag === "Failure") {
    return $ParserResult(
      "Failure",
      progn => {
        const v1$1 = v1._1(progn);
        return Data$dTuple.$Tuple(v(v1$1._1), Data$dTuple.$Tuple(v1$1._2._1, Data$dTuple.$Tuple(v1$1._2._2._1, v1$1._2._2._2)));
      }
    );
  }
  return v1;
};
const optional = dictAlt => dictApplicative => a => dictAlt.alt(dictAlt.Functor0().map(Data$dMaybe.Just)(a))(dictApplicative.pure(Data$dMaybe.Nothing));
const optionNewtype = {Coercible0: () => {}};
const optVisibilityGeneric = {
  to: x => {
    if (x.tag === "Inl") { return Internal; }
    if (x.tag === "Inr") {
      if (x._1.tag === "Inl") { return Hidden; }
      if (x._1.tag === "Inr") { return Visible; }
    }
    $runtime.fail();
  },
  from: x => {
    if (x === "Internal") { return Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments); }
    if (x === "Hidden") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments)); }
    if (x === "Visible") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.NoArguments)); }
    $runtime.fail();
  }
};
const optVisibilityShow = {
  show: /* #__PURE__ */ (() => {
    const $0 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "Internal"});
    const $1 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "Hidden"});
    const $2 = (() => {
      const $2 = (() => {
        const $2 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "Visible"});
        return {
          "genericShow'": v => {
            if (v.tag === "Inl") { return $1["genericShow'"](v._1); }
            if (v.tag === "Inr") { return $2["genericShow'"](v._1); }
            $runtime.fail();
          }
        };
      })();
      return {
        "genericShow'": v => {
          if (v.tag === "Inl") { return $0["genericShow'"](v._1); }
          if (v.tag === "Inr") { return $2["genericShow'"](v._1); }
          $runtime.fail();
        }
      };
    })();
    return x => $2["genericShow'"]((() => {
      if (x === "Internal") { return Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments); }
      if (x === "Hidden") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments)); }
      if (x === "Visible") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.NoArguments)); }
      $runtime.fail();
    })());
  })()
};
const show = /* #__PURE__ */ (() => {
  const $0 = Options$dApplicative$dHelp$dChunk.chunkShow(Text$dPrettyPrint$dLeijen.docShow);
  return record => "{ propDescMod: " + showMaybe.show(record.propDescMod) + ", propHelp: " + $0.show(record.propHelp) + ", propMetaVar: " + Data$dShow.showStringImpl(record.propMetaVar) + ", propShowDefault: " + showMaybe.show(record.propShowDefault) + ", propVisibility: " + optVisibilityShow.show(record.propVisibility) + " }";
})();
const optVisibilityEq = {
  eq: x => y => {
    if (x === "Internal") { return y === "Internal"; }
    if (x === "Hidden") { return y === "Hidden"; }
    return x === "Visible" && y === "Visible";
  }
};
const optVisibilityOrd = {
  compare: x => y => {
    if (x === "Internal") {
      if (y === "Internal") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Internal") { return Data$dOrdering.GT; }
    if (x === "Hidden") {
      if (y === "Hidden") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Hidden") { return Data$dOrdering.GT; }
    if (x === "Visible" && y === "Visible") { return Data$dOrdering.EQ; }
    $runtime.fail();
  },
  Eq0: () => optVisibilityEq
};
const optTreeGeneric = {
  to: x => {
    if (x.tag === "Inl") { return $OptTree("Leaf", x._1); }
    if (x.tag === "Inr") {
      if (x._1.tag === "Inl") { return $OptTree("MultNode", x._1._1); }
      if (x._1.tag === "Inr") { return $OptTree("AltNode", x._1._1); }
    }
    $runtime.fail();
  },
  from: x => {
    if (x.tag === "Leaf") { return Data$dGeneric$dRep.$Sum("Inl", x._1); }
    if (x.tag === "MultNode") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", x._1)); }
    if (x.tag === "AltNode") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", x._1)); }
    $runtime.fail();
  }
};
const optTreeShow = dictShow => (
  {
    show: (() => {
      const $0 = Data$dShow$dGeneric.genericShowConstructor({genericShowArgs: v => [dictShow.show(v)]})(LeafIsSymbol);
      const $1 = Data$dShow$dGeneric.genericShowConstructor((() => {
        const $1 = Data$dShow.showArrayImpl(optTreeShow(dictShow).show);
        return {genericShowArgs: v => [$1(v)]};
      })())(MultNodeIsSymbol);
      const $2 = (() => {
        const $2 = (() => {
          const $2 = Data$dShow$dGeneric.genericShowConstructor((() => {
            const $2 = Data$dShow.showArrayImpl(optTreeShow(dictShow).show);
            return {genericShowArgs: v => [$2(v)]};
          })())(AltNodeIsSymbol);
          return {
            "genericShow'": v => {
              if (v.tag === "Inl") { return $1["genericShow'"](v._1); }
              if (v.tag === "Inr") { return $2["genericShow'"](v._1); }
              $runtime.fail();
            }
          };
        })();
        return {
          "genericShow'": v => {
            if (v.tag === "Inl") { return $0["genericShow'"](v._1); }
            if (v.tag === "Inr") { return $2["genericShow'"](v._1); }
            $runtime.fail();
          }
        };
      })();
      return x => $2["genericShow'"]((() => {
        if (x.tag === "Leaf") { return Data$dGeneric$dRep.$Sum("Inl", x._1); }
        if (x.tag === "MultNode") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", x._1)); }
        if (x.tag === "AltNode") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", x._1)); }
        $runtime.fail();
      })());
    })()
  }
);
const optPropertiesShow = {
  show: v => "(OptProperties " + show({
    propVisibility: v.propVisibility,
    propHelp: v.propHelp,
    propMetaVar: v.propMetaVar,
    propShowDefault: v.propShowDefault,
    propDescMod: v.propDescMod.tag === "Just" ? Data$dMaybe.$Maybe("Just", "<func>") : Data$dMaybe.Nothing
  }) + ")"
};
const show1 = record => "{ optMain: " + Data$dShow.showStringImpl(record.optMain) + ", optProps: " + optPropertiesShow.show(record.optProps) + " }";
const optionShow = {show: v => "(Option " + show1({optProps: v.optProps, optMain: "<OptReader>"}) + ")"};
const optPropertiesNewtype = {Coercible0: () => {}};
const optShowDefault = x => x.optProps.propShowDefault;
const optVisibility = x => x.optProps.propVisibility;
const optNameGeneric = {
  to: x => {
    if (x.tag === "Inl") { return $OptName("OptShort", x._1); }
    if (x.tag === "Inr") { return $OptName("OptLong", x._1); }
    $runtime.fail();
  },
  from: x => {
    if (x.tag === "OptShort") { return Data$dGeneric$dRep.$Sum("Inl", x._1); }
    if (x.tag === "OptLong") { return Data$dGeneric$dRep.$Sum("Inr", x._1); }
    $runtime.fail();
  }
};
const optNameShow = {
  show: /* #__PURE__ */ (() => {
    const $0 = Data$dShow$dGeneric.genericShowConstructor({genericShowArgs: v => [Data$dShow.showCharImpl(v)]})({reflectSymbol: () => "OptShort"});
    const $1 = Data$dShow$dGeneric.genericShowConstructor({genericShowArgs: v => [Data$dShow.showStringImpl(v)]})({reflectSymbol: () => "OptLong"});
    return x => {
      if (x.tag === "OptShort") { return $0["genericShow'"](x._1); }
      if (x.tag === "OptLong") { return $1["genericShow'"](x._1); }
      $runtime.fail();
    };
  })()
};
const optNameEq = {
  eq: x => y => {
    if (x.tag === "OptShort") { return y.tag === "OptShort" && x._1 === y._1; }
    return x.tag === "OptLong" && y.tag === "OptLong" && x._1 === y._1;
  }
};
const optNameOrd = {
  compare: x => y => {
    if (x.tag === "OptShort") {
      if (y.tag === "OptShort") { return Data$dOrd.ordChar.compare(x._1)(y._1); }
      return Data$dOrdering.LT;
    }
    if (y.tag === "OptShort") { return Data$dOrdering.GT; }
    if (x.tag === "OptLong" && y.tag === "OptLong") { return Data$dOrd.ordString.compare(x._1)(y._1); }
    $runtime.fail();
  },
  Eq0: () => optNameEq
};
const optMetaVar = x => x.optProps.propMetaVar;
const optHelpInfoGeneric = {to: x => x, from: x => x};
const optHelpInfoShow = {
  show: /* #__PURE__ */ (() => {
    const $0 = Data$dShow$dGeneric.genericShowConstructor({
      genericShowArgs: v => [
        (v.hinfoDefault ? "{ hinfoDefault: true," : "{ hinfoDefault: false,") + (v.hinfoMulti ? " hinfoMulti: true" : " hinfoMulti: false") + ", hinfoUnreachableArgs: " + (v.hinfoUnreachableArgs
          ? "true"
          : "false") + " }"
      ]
    })({reflectSymbol: () => "OptHelpInfo"});
    return x => $0["genericShow'"](x);
  })()
};
const optHelpInfoEq = {eq: x => y => x.hinfoDefault === y.hinfoDefault && x.hinfoMulti === y.hinfoMulti && x.hinfoUnreachableArgs === y.hinfoUnreachableArgs};
const optHelp = x => x.optProps.propHelp;
const optDescMod = x => x.optProps.propDescMod;
const newtypeParserFailure = {Coercible0: () => {}};
const newtypeOptHelpInfo = {Coercible0: () => {}};
const newtypeCompletionResult = {Coercible0: () => {}};
const newtypeCompleter = {Coercible0: () => {}};
const newtypeCReader = {Coercible0: () => {}};
const mkCompleter = Completer;
const isCmdStartGeneric = {
  to: x => {
    if (x.tag === "Inl") { return CmdStart; }
    if (x.tag === "Inr") { return CmdCont; }
    $runtime.fail();
  },
  from: x => {
    if (x === "CmdStart") { return Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments); }
    if (x === "CmdCont") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.NoArguments); }
    $runtime.fail();
  }
};
const isCmdStartShow = {
  show: /* #__PURE__ */ (() => {
    const $0 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "CmdStart"});
    const $1 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "CmdCont"});
    return x => {
      if (x === "CmdStart") { return $0["genericShow'"](Data$dGeneric$dRep.NoArguments); }
      if (x === "CmdCont") { return $1["genericShow'"](Data$dGeneric$dRep.NoArguments); }
      $runtime.fail();
    };
  })()
};
const completionResultShow = {show: v => "(CompletionResult <function>)"};
const genericShowSum = /* #__PURE__ */ (() => {
  const $0 = Data$dShow$dGeneric.genericShowConstructor({genericShowArgs: v => ["(ParserFailure <function>)"]})({reflectSymbol: () => "Failure"});
  const $1 = Data$dShow$dGeneric.genericShowConstructor({genericShowArgs: v => ["(CompletionResult <function>)"]})({reflectSymbol: () => "CompletionInvoked"});
  return {
    "genericShow'": v => {
      if (v.tag === "Inl") { return $0["genericShow'"](v._1); }
      if (v.tag === "Inr") { return $1["genericShow'"](v._1); }
      $runtime.fail();
    }
  };
})();
const parserResultShow = dictShow => (
  {
    show: (() => {
      const $0 = Data$dShow$dGeneric.genericShowConstructor({genericShowArgs: v => [dictShow.show(v)]})(SuccessIsSymbol);
      return x => {
        if (x.tag === "Success") { return $0["genericShow'"](x._1); }
        if (x.tag === "Failure") { return genericShowSum["genericShow'"](Data$dGeneric$dRep.$Sum("Inl", x._1)); }
        if (x.tag === "CompletionInvoked") { return genericShowSum["genericShow'"](Data$dGeneric$dRep.$Sum("Inr", x._1)); }
        $runtime.fail();
      };
    })()
  }
);
const completerSemigroup = {
  append: v => v1 => s => {
    const $0 = v(s);
    const $1 = v1(s);
    return () => {
      const a$p = $0();
      const a$p$1 = $1();
      return [...a$p, ...a$p$1];
    };
  }
};
const completerMonoid = {mempty: v => () => [], Semigroup0: () => completerSemigroup};
const cReaderFunctor = {
  map: f => r => (
    {
      crReader: x => {
        const $0 = r.crReader(x);
        if ($0.tag === "Left") { return Data$dEither.$Either("Left", $0._1); }
        if ($0.tag === "Right") { return Data$dEither.$Either("Right", f($0._1)); }
        $runtime.fail();
      },
      crCompleter: r.crCompleter
    }
  )
};
const parserInfoFunctor = {
  map: f => i => (
    {
      infoParser: parserFunctor.map(f)(i.infoParser),
      infoFailureCode: i.infoFailureCode,
      infoFooter: i.infoFooter,
      infoFullDesc: i.infoFullDesc,
      infoHeader: i.infoHeader,
      infoPolicy: i.infoPolicy,
      infoProgDesc: i.infoProgDesc
    }
  )
};
const parserFunctor = {
  map: v => v1 => {
    if (v1.tag === "NilP") { return $Parser("NilP", v(v1._1)); }
    if (v1.tag === "OptP") { return $Parser("OptP", optionFunctor.map(v)(v1._1)); }
    if (v1.tag === "MultP") { return $Parser("MultP", $MultPE(parserFunctor.map(v3 => x => v(v3(x)))(v1._1._1), v1._1._2)); }
    if (v1.tag === "AltP") { return $Parser("AltP", parserFunctor.map(v)(v1._1), parserFunctor.map(v)(v1._2)); }
    if (v1.tag === "BindP") {
      return $Parser(
        "BindP",
        Control$dMonad$dFree.$Free(
          v1._1._1,
          (() => {
            if (v1._1._2.tag === "CatNil") {
              return Data$dCatList.$CatList(
                "CatCons",
                x => Control$dMonad$dFree.$Free(Control$dMonad$dFree.$FreeView("Return", v(x)), Data$dCatList.CatNil),
                Data$dCatQueue.$CatQueue(Data$dList$dTypes.Nil, Data$dList$dTypes.Nil)
              );
            }
            if (v1._1._2.tag === "CatCons") {
              return Data$dCatList.$CatList(
                "CatCons",
                v1._1._2._1,
                Data$dCatQueue.$CatQueue(
                  v1._1._2._2._1,
                  Data$dList$dTypes.$List(
                    "Cons",
                    Data$dCatList.$CatList(
                      "CatCons",
                      x => Control$dMonad$dFree.$Free(Control$dMonad$dFree.$FreeView("Return", v(x)), Data$dCatList.CatNil),
                      Data$dCatQueue.$CatQueue(Data$dList$dTypes.Nil, Data$dList$dTypes.Nil)
                    ),
                    v1._1._2._2._2
                  )
                )
              );
            }
            $runtime.fail();
          })()
        )
      );
    }
    $runtime.fail();
  }
};
const optionFunctor = {map: f => o => ({optMain: optReaderFunctor.map(f)(o.optMain), optProps: o.optProps})};
const optReaderFunctor = {
  map: v => v1 => {
    if (v1.tag === "OptReader") {
      const $0 = v1._2;
      return $OptReader(
        "OptReader",
        v1._1,
        {
          crReader: x => {
            const $1 = $0.crReader(x);
            if ($1.tag === "Left") { return Data$dEither.$Either("Left", $1._1); }
            if ($1.tag === "Right") { return Data$dEither.$Either("Right", v($1._1)); }
            $runtime.fail();
          },
          crCompleter: $0.crCompleter
        },
        v1._3
      );
    }
    if (v1.tag === "FlagReader") { return $OptReader("FlagReader", v1._1, v(v1._2)); }
    if (v1.tag === "ArgReader") {
      const $0 = v1._1;
      return $OptReader(
        "ArgReader",
        {
          crReader: x => {
            const $1 = $0.crReader(x);
            if ($1.tag === "Left") { return Data$dEither.$Either("Left", $1._1); }
            if ($1.tag === "Right") { return Data$dEither.$Either("Right", v($1._1)); }
            $runtime.fail();
          },
          crCompleter: $0.crCompleter
        }
      );
    }
    if (v1.tag === "CmdReader") {
      return $OptReader(
        "CmdReader",
        v1._1,
        v1._2,
        x => {
          const $0 = v1._3(x);
          if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", parserInfoFunctor.map(v)($0._1)); }
          return Data$dMaybe.Nothing;
        }
      );
    }
    $runtime.fail();
  }
};
const parserAlt = {alt: AltP, Functor0: () => parserFunctor};
const parserApply = {apply: a => b => $Parser("MultP", $MultPE(a, b)), Functor0: () => parserFunctor};
const parserApplicative = {pure: NilP, Apply0: () => parserApply};
const manyM = p => Control$dMonad$dFree.freeMonadRec.tailRecM(acc => Control$dMonad$dFree.$Free(
  Control$dMonad$dFree.$FreeView(
    "Bind",
    $Parser("AltP", parserFunctor.map(Control$dMonad$dRec$dClass.Loop)(p), $Parser("NilP", Control$dMonad$dRec$dClass.$Step("Done", undefined))),
    x => Control$dMonad$dFree.$Free(Control$dMonad$dFree.$FreeView("Return", x), Data$dCatList.CatNil)
  ),
  Data$dCatList.$CatList(
    "CatCons",
    aa => Control$dMonad$dFree.$Free(
      Control$dMonad$dFree.$FreeView(
        "Return",
        (() => {
          if (aa.tag === "Loop") { return Control$dMonad$dRec$dClass.$Step("Loop", Data$dList$dTypes.$List("Cons", aa._1, acc)); }
          if (aa.tag === "Done") {
            return Control$dMonad$dRec$dClass.$Step(
              "Done",
              (() => {
                const go = go$a0$copy => go$a1$copy => {
                  let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
                  while (go$c) {
                    const v = go$a0, v1 = go$a1;
                    if (v1.tag === "Nil") {
                      go$c = false;
                      go$r = v;
                      continue;
                    }
                    if (v1.tag === "Cons") {
                      go$a0 = Data$dList$dTypes.$List("Cons", v1._1, v);
                      go$a1 = v1._2;
                      continue;
                    }
                    $runtime.fail();
                  }
                  return go$r;
                };
                return go(Data$dList$dTypes.Nil)(acc);
              })()
            );
          }
          $runtime.fail();
        })()
      ),
      Data$dCatList.CatNil
    ),
    Data$dCatQueue.$CatQueue(Data$dList$dTypes.Nil, Data$dList$dTypes.Nil)
  )
))(Data$dList$dTypes.Nil);
const many = x => $Parser("BindP", manyM(x));
const someM = p => Control$dMonad$dFree.freeApply.apply(Control$dMonad$dFree.$Free(
  Control$dMonad$dFree.$FreeView("Bind", p, x => Control$dMonad$dFree.$Free(Control$dMonad$dFree.$FreeView("Return", x), Data$dCatList.CatNil)),
  Data$dCatList.$CatList(
    "CatCons",
    x => Control$dMonad$dFree.$Free(Control$dMonad$dFree.$FreeView("Return", xs => Data$dNonEmpty.$NonEmpty(x, xs)), Data$dCatList.CatNil),
    Data$dCatQueue.$CatQueue(Data$dList$dTypes.Nil, Data$dList$dTypes.Nil)
  )
))(manyM(p));
const some = x => $Parser("BindP", someM(x));
const backtrackingGeneric = {
  to: x => {
    if (x.tag === "Inl") { return Backtrack; }
    if (x.tag === "Inr") {
      if (x._1.tag === "Inl") { return NoBacktrack; }
      if (x._1.tag === "Inr") { return SubparserInline; }
    }
    $runtime.fail();
  },
  from: x => {
    if (x === "Backtrack") { return Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments); }
    if (x === "NoBacktrack") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments)); }
    if (x === "SubparserInline") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.NoArguments)); }
    $runtime.fail();
  }
};
const backtrackingShow = {
  show: /* #__PURE__ */ (() => {
    const $0 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "Backtrack"});
    const $1 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "NoBacktrack"});
    const $2 = (() => {
      const $2 = (() => {
        const $2 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "SubparserInline"});
        return {
          "genericShow'": v => {
            if (v.tag === "Inl") { return $1["genericShow'"](v._1); }
            if (v.tag === "Inr") { return $2["genericShow'"](v._1); }
            $runtime.fail();
          }
        };
      })();
      return {
        "genericShow'": v => {
          if (v.tag === "Inl") { return $0["genericShow'"](v._1); }
          if (v.tag === "Inr") { return $2["genericShow'"](v._1); }
          $runtime.fail();
        }
      };
    })();
    return x => $2["genericShow'"]((() => {
      if (x === "Backtrack") { return Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments); }
      if (x === "NoBacktrack") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments)); }
      if (x === "SubparserInline") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.NoArguments)); }
      $runtime.fail();
    })());
  })()
};
const parserPrefsShow = {
  show: /* #__PURE__ */ (() => {
    const $0 = Data$dShow$dGeneric.genericShowConstructor({
      genericShowArgs: v => [
        "{ prefBacktrack: " + backtrackingShow.show(v.prefBacktrack) + ", prefColumns: " + Data$dShow.showIntImpl(v.prefColumns) + "," + (v.prefDisambiguate
          ? " prefDisambiguate: true"
          : " prefDisambiguate: false") + ", prefMultiSuffix: " + Data$dShow.showStringImpl(v.prefMultiSuffix) + "," + (v.prefShowHelpOnEmpty
          ? " prefShowHelpOnEmpty: true"
          : " prefShowHelpOnEmpty: false") + ", prefShowHelpOnError: " + (v.prefShowHelpOnError ? "true" : "false") + " }"
      ]
    })({reflectSymbol: () => "ParserPrefs"});
    return x => $0["genericShow'"](x);
  })()
};
const backtrackingEq = {
  eq: x => y => {
    if (x === "Backtrack") { return y === "Backtrack"; }
    if (x === "NoBacktrack") { return y === "NoBacktrack"; }
    return x === "SubparserInline" && y === "SubparserInline";
  }
};
const parserPrefsEq = {
  eq: x => y => (() => {
    if (x.prefBacktrack === "Backtrack") { return y.prefBacktrack === "Backtrack"; }
    if (x.prefBacktrack === "NoBacktrack") { return y.prefBacktrack === "NoBacktrack"; }
    return x.prefBacktrack === "SubparserInline" && y.prefBacktrack === "SubparserInline";
  })() && x.prefColumns === y.prefColumns && x.prefDisambiguate === y.prefDisambiguate && x.prefMultiSuffix === y.prefMultiSuffix && x.prefShowHelpOnEmpty === y.prefShowHelpOnEmpty && x.prefShowHelpOnError === y.prefShowHelpOnError
};
const argPolicyGeneric = {
  to: x => {
    if (x.tag === "Inl") { return Intersperse; }
    if (x.tag === "Inr") {
      if (x._1.tag === "Inl") { return NoIntersperse; }
      if (x._1.tag === "Inr") {
        if (x._1._1.tag === "Inl") { return AllPositionals; }
        if (x._1._1.tag === "Inr") { return ForwardOptions; }
      }
    }
    $runtime.fail();
  },
  from: x => {
    if (x === "Intersperse") { return Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments); }
    if (x === "NoIntersperse") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments)); }
    if (x === "AllPositionals") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments))); }
    if (x === "ForwardOptions") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.NoArguments))); }
    $runtime.fail();
  }
};
const argPolicyShow = {
  show: /* #__PURE__ */ (() => {
    const $0 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "Intersperse"});
    const $1 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "NoIntersperse"});
    const $2 = (() => {
      const $2 = (() => {
        const $2 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "AllPositionals"});
        const $3 = (() => {
          const $3 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "ForwardOptions"});
          return {
            "genericShow'": v => {
              if (v.tag === "Inl") { return $2["genericShow'"](v._1); }
              if (v.tag === "Inr") { return $3["genericShow'"](v._1); }
              $runtime.fail();
            }
          };
        })();
        return {
          "genericShow'": v => {
            if (v.tag === "Inl") { return $1["genericShow'"](v._1); }
            if (v.tag === "Inr") { return $3["genericShow'"](v._1); }
            $runtime.fail();
          }
        };
      })();
      return {
        "genericShow'": v => {
          if (v.tag === "Inl") { return $0["genericShow'"](v._1); }
          if (v.tag === "Inr") { return $2["genericShow'"](v._1); }
          $runtime.fail();
        }
      };
    })();
    return x => $2["genericShow'"]((() => {
      if (x === "Intersperse") { return Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments); }
      if (x === "NoIntersperse") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments)); }
      if (x === "AllPositionals") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments))); }
      if (x === "ForwardOptions") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.NoArguments))); }
      $runtime.fail();
    })());
  })()
};
const argPolicyEq = {
  eq: x => y => {
    if (x === "Intersperse") { return y === "Intersperse"; }
    if (x === "NoIntersperse") { return y === "NoIntersperse"; }
    if (x === "AllPositionals") { return y === "AllPositionals"; }
    return x === "ForwardOptions" && y === "ForwardOptions";
  }
};
const argPolicyOrd = {
  compare: x => y => {
    if (x === "Intersperse") {
      if (y === "Intersperse") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Intersperse") { return Data$dOrdering.GT; }
    if (x === "NoIntersperse") {
      if (y === "NoIntersperse") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "NoIntersperse") { return Data$dOrdering.GT; }
    if (x === "AllPositionals") {
      if (y === "AllPositionals") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "AllPositionals") { return Data$dOrdering.GT; }
    if (x === "ForwardOptions" && y === "ForwardOptions") { return Data$dOrdering.EQ; }
    $runtime.fail();
  },
  Eq0: () => argPolicyEq
};
export {
  $ArgPolicy,
  $Backtracking,
  $Context,
  $IsCmdStart,
  $MultPE,
  $OptName,
  $OptReader,
  $OptTree,
  $OptVisibility,
  $ParseError,
  $Parser,
  $ParserResult,
  $SomeParser,
  AllPositionals,
  AltNode,
  AltNodeIsSymbol,
  AltP,
  ArgReader,
  Backtrack,
  BindP,
  CReader,
  CmdCont,
  CmdReader,
  CmdStart,
  Completer,
  CompletionInvoked,
  CompletionResult,
  Context,
  ErrorMsg,
  ExpectsArgError,
  Failure,
  FlagReader,
  ForwardOptions,
  Hidden,
  InfoMsg,
  Internal,
  Intersperse,
  Leaf,
  LeafIsSymbol,
  MissingError,
  MultNode,
  MultNodeIsSymbol,
  MultP,
  MultPE,
  NilP,
  NoBacktrack,
  NoIntersperse,
  OptHelpInfo,
  OptLong,
  OptP,
  OptProperties,
  OptReader,
  OptShort,
  Option,
  ParserFailure,
  ParserInfo,
  ParserPrefs,
  ReadM,
  ShowHelpText,
  SomeParser,
  SubparserInline,
  Success,
  SuccessIsSymbol,
  UnexpectedError,
  Visible,
  alt,
  apply,
  argPolicyEq,
  argPolicyGeneric,
  argPolicyOrd,
  argPolicyShow,
  backtrackingEq,
  backtrackingGeneric,
  backtrackingShow,
  bind,
  cReaderFunctor,
  completerMonoid,
  completerSemigroup,
  completionResultShow,
  genericShowSum,
  isCmdStartGeneric,
  isCmdStartShow,
  many,
  manyM,
  mkCompleter,
  newtypeCReader,
  newtypeCompleter,
  newtypeCompletionResult,
  newtypeOptHelpInfo,
  newtypeParserFailure,
  optDescMod,
  optHelp,
  optHelpInfoEq,
  optHelpInfoGeneric,
  optHelpInfoShow,
  optMetaVar,
  optNameEq,
  optNameGeneric,
  optNameOrd,
  optNameShow,
  optPropertiesNewtype,
  optPropertiesShow,
  optReaderFunctor,
  optShowDefault,
  optTreeGeneric,
  optTreeShow,
  optVisibility,
  optVisibilityEq,
  optVisibilityGeneric,
  optVisibilityOrd,
  optVisibilityShow,
  optionFunctor,
  optionNewtype,
  optionShow,
  optional,
  overFailure,
  parseErrorSemigroup,
  parserAlt,
  parserApplicative,
  parserApply,
  parserFailureFunctor,
  parserFailureShow,
  parserFunctor,
  parserInfoFunctor,
  parserInfoNewtype,
  parserPrefsEq,
  parserPrefsGeneric,
  parserPrefsNewtype,
  parserPrefsShow,
  parserResultApplicative,
  parserResultApply,
  parserResultBind,
  parserResultFunctor,
  parserResultGeneric,
  parserResultMonad,
  parserResultShow,
  readMAlt,
  readMApplicative,
  readMApply,
  readMBind,
  readMFunctor,
  readMMonad,
  readMMonadFail,
  readMNewtype,
  readerAbort,
  readerAsk,
  readerError,
  show,
  show1,
  showMaybe,
  some,
  someM
};
