import * as $runtime from "../runtime.js";
import * as Control$dCategory from "../Control.Category/index.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dGeneric$dRep from "../Data.Generic.Rep/index.js";
import * as Data$dInt from "../Data.Int/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNullable from "../Data.Nullable/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Data$dProfunctor$dStrong from "../Data.Profunctor.Strong/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dShow$dGeneric from "../Data.Show.Generic/index.js";
import * as Data$dString$dCodePoints from "../Data.String.CodePoints/index.js";
import * as Data$dString$dCodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data$dString$dCommon from "../Data.String.Common/index.js";
import * as Data$dTraversable from "../Data.Traversable/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Dict from "../Dict/index.js";
import * as Effect from "../Effect/index.js";
import * as Effect$dAff from "../Effect.Aff/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
import * as Effect$dException from "../Effect.Exception/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
import * as Lattice from "../Lattice/index.js";
import * as Pretty from "../Pretty/index.js";
import * as Primitive from "../Primitive/index.js";
import * as Util from "../Util/index.js";
import * as Util$dMap from "../Util.Map/index.js";
import * as Val from "../Val/index.js";
import * as Web$dEvent$dEvent from "../Web.Event.Event/index.js";
const $SelState = (tag, _1) => ({tag, _1});
const $SelectionType = tag => tag;
const $𝕊 = tag => tag;
const genericShowSum = /* #__PURE__ */ (() => {
  const $0 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "Inert"});
  return dictGenericShow1 => (
    {
      "genericShow'": v => {
        if (v.tag === "Inl") { return $0["genericShow'"](v._1); }
        if (v.tag === "Inr") { return dictGenericShow1["genericShow'"](v._1); }
        $runtime.fail();
      }
    }
  );
})();
const ReactiveIsSymbol = {reflectSymbol: () => "Reactive"};
const SelStatesIsSymbol = {reflectSymbol: () => "SelStates"};
const DimensionsIsSymbol = {reflectSymbol: () => "Dimensions"};
const sequence = /* #__PURE__ */ (() => Data$dTraversable.traversableArray.traverse(Effect$dAff.applicativeAff)(Data$dTraversable.identity))();
const sequence_ = /* #__PURE__ */ Data$dFoldable.traverse_(Effect.applicativeEffect)(Data$dFoldable.foldableArray)(Data$dFoldable.identity);
const clamp = low => hi => x => {
  const v = Data$dOrd.ordInt.compare(low)(x);
  const $0 = (() => {
    if (v === "LT") { return x; }
    if (v === "EQ") { return low; }
    if (v === "GT") { return low; }
    $runtime.fail();
  })();
  const v$1 = Data$dOrd.ordInt.compare(hi)($0);
  if (v$1 === "LT") { return hi; }
  if (v$1 === "EQ") { return hi; }
  if (v$1 === "GT") { return $0; }
  $runtime.fail();
};
const fromFoldable = /* #__PURE__ */ Foreign$dObject.fromFoldable(Data$dFoldable.foldableArray);
const None = /* #__PURE__ */ $𝕊("None");
const Secondary = /* #__PURE__ */ $𝕊("Secondary");
const Primary = /* #__PURE__ */ $𝕊("Primary");
const Persistent = /* #__PURE__ */ $SelectionType("Persistent");
const Transient = /* #__PURE__ */ $SelectionType("Transient");
const Inert = /* #__PURE__ */ $SelState("Inert");
const Reactive = value0 => $SelState("Reactive", value0);
const SelStates = x => x;
const Dimensions = x => x;
const newtypeSelStates_ = {Coercible0: () => {}};
const newtypeDimensions_ = {Coercible0: () => {}};
const meetSemilatticeSelStates = dictMeetSemilattice => (
  {
    meet: v => v1 => {
      if (v1.tag === "Inert") { return Inert; }
      if (v.tag === "Inert") { return Inert; }
      if (v.tag === "Reactive" && v1.tag === "Reactive") {
        return $SelState(
          "Reactive",
          {persistent: dictMeetSemilattice.meet(v._1.persistent)(v1._1.persistent), transient: dictMeetSemilattice.meet(v._1.transient)(v1._1.transient)}
        );
      }
      $runtime.fail();
    }
  }
);
const meetSemilatticeSelState = dictMeetSemilattice => (
  {
    meet: v => v1 => {
      if (v1.tag === "Inert") { return Inert; }
      if (v.tag === "Inert") { return Inert; }
      if (v.tag === "Reactive" && v1.tag === "Reactive") { return $SelState("Reactive", dictMeetSemilattice.meet(v._1)(v1._1)); }
      $runtime.fail();
    }
  }
);
const joinSemilatticeSelStates = dictJoinSemilattice => (
  {
    join: v => v1 => {
      if (v1.tag === "Inert") { return v; }
      if (v.tag === "Inert") { return v1; }
      if (v.tag === "Reactive" && v1.tag === "Reactive") {
        return $SelState(
          "Reactive",
          {persistent: dictJoinSemilattice.join(v._1.persistent)(v1._1.persistent), transient: dictJoinSemilattice.join(v._1.transient)(v1._1.transient)}
        );
      }
      $runtime.fail();
    }
  }
);
const joinSemilatticeSelState = dictJoinSemilattice => (
  {
    join: v => v1 => {
      if (v1.tag === "Inert") { return v; }
      if (v.tag === "Inert") { return v1; }
      if (v.tag === "Reactive" && v1.tag === "Reactive") { return $SelState("Reactive", dictJoinSemilattice.join(v._1)(v1._1)); }
      $runtime.fail();
    }
  }
);
const highlightableSelStates = dictHighlightable => dictJoinSemilattice => (
  {
    highlightIf: v => {
      if (v.tag === "Inert") { return Val.identity; }
      if (v.tag === "Reactive") { return dictHighlightable.highlightIf(dictJoinSemilattice.join(v._1.persistent)(v._1.transient)); }
      $runtime.fail();
    }
  }
);
const prettyP = /* #__PURE__ */ Pretty.prettyP(/* #__PURE__ */ Pretty.prettyVal(/* #__PURE__ */ highlightableSelStates(Val.highlightableBoolean)(Lattice.joinSemilatticeBoolean)));
const highlightableSelState = dictHighlightable => dictJoinSemilattice => (
  {
    highlightIf: v => {
      if (v.tag === "Inert") { return Val.identity; }
      if (v.tag === "Reactive") { return dictHighlightable.highlightIf(v._1); }
      $runtime.fail();
    }
  }
);
const generic𝕊_ = {
  to: x => {
    if (x.tag === "Inl") { return None; }
    if (x.tag === "Inr") {
      if (x._1.tag === "Inl") { return Secondary; }
      if (x._1.tag === "Inr") { return Primary; }
    }
    $runtime.fail();
  },
  from: x => {
    if (x === "None") { return Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments); }
    if (x === "Secondary") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments)); }
    if (x === "Primary") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.NoArguments)); }
    $runtime.fail();
  }
};
const show𝕊 = {
  show: /* #__PURE__ */ (() => {
    const $0 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "None"});
    const $1 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "Secondary"});
    const $2 = (() => {
      const $2 = (() => {
        const $2 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "Primary"});
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
      if (x === "None") { return Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments); }
      if (x === "Secondary") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments)); }
      if (x === "Primary") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.NoArguments)); }
      $runtime.fail();
    })());
  })()
};
const genericSelStates_ = {to: x => x, from: x => x};
const genericSelState_ = {
  to: x => {
    if (x.tag === "Inl") { return Inert; }
    if (x.tag === "Inr") { return $SelState("Reactive", x._1); }
    $runtime.fail();
  },
  from: x => {
    if (x.tag === "Inert") { return Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments); }
    if (x.tag === "Reactive") { return Data$dGeneric$dRep.$Sum("Inr", x._1); }
    $runtime.fail();
  }
};
const showSelState = dictShow => (
  {
    show: (() => {
      const $0 = genericShowSum(Data$dShow$dGeneric.genericShowConstructor({genericShowArgs: v => [dictShow.show(v)]})(ReactiveIsSymbol));
      return x => $0["genericShow'"]((() => {
        if (x.tag === "Inert") { return Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments); }
        if (x.tag === "Reactive") { return Data$dGeneric$dRep.$Sum("Inr", x._1); }
        $runtime.fail();
      })());
    })()
  }
);
const showSelStates = dictShow => (
  {
    show: (() => {
      const $0 = Data$dShow$dGeneric.genericShowConstructor((() => {
        const $0 = showSelState({show: record => "{ persistent: " + dictShow.show(record.persistent) + ", transient: " + dictShow.show(record.transient) + " }"});
        return {genericShowArgs: v => [$0.show(v)]};
      })())(SelStatesIsSymbol);
      return x => $0["genericShow'"](x);
    })()
  }
);
const show1 = /* #__PURE__ */ (() => showSelStates(Data$dShow.showBoolean).show)();
const genericDimensions_ = {to: x => x, from: x => x};
const showDimensions = dictShow => (
  {
    show: (() => {
      const $0 = Data$dShow$dGeneric.genericShowConstructor({genericShowArgs: v => ["{ height: " + dictShow.show(v.height) + ", width: " + dictShow.show(v.width) + " }"]})(DimensionsIsSymbol);
      return x => $0["genericShow'"](x);
    })()
  }
);
const functorSelState = {
  map: f => m => {
    if (m.tag === "Inert") { return Inert; }
    if (m.tag === "Reactive") { return $SelState("Reactive", f(m._1)); }
    $runtime.fail();
  }
};
const functorSelStates = {
  map: f => m => {
    if (m.tag === "Inert") { return Inert; }
    if (m.tag === "Reactive") { return $SelState("Reactive", {persistent: f(m._1.persistent), transient: f(m._1.transient)}); }
    $runtime.fail();
  }
};
const functorDimensions = {map: f => m => ({width: f(m.width), height: f(m.height)})};
const eqSelectionType = {
  eq: x => y => {
    if (x === "Persistent") { return y === "Persistent"; }
    return x === "Transient" && y === "Transient";
  }
};
const eqSelState = dictEq => (
  {
    eq: x => y => {
      if (x.tag === "Inert") { return y.tag === "Inert"; }
      return x.tag === "Reactive" && y.tag === "Reactive" && dictEq.eq(x._1)(y._1);
    }
  }
);
const eqSelStates = dictEq => (
  {
    eq: x => y => {
      if (x.tag === "Inert") { return y.tag === "Inert"; }
      return x.tag === "Reactive" && y.tag === "Reactive" && dictEq.eq(x._1.persistent)(y._1.persistent) && dictEq.eq(x._1.transient)(y._1.transient);
    }
  }
);
const boundedMeetSemilatticeSel = dictBounded => {
  const top = dictBounded.top;
  return dictBoundedMeetSemilattice => {
    const $0 = dictBoundedMeetSemilattice.MeetSemilattice0();
    const meetSemilatticeSelStates1 = {
      meet: v => v1 => {
        if (v1.tag === "Inert") { return Inert; }
        if (v.tag === "Inert") { return Inert; }
        if (v.tag === "Reactive" && v1.tag === "Reactive") {
          return $SelState("Reactive", {persistent: $0.meet(v._1.persistent)(v1._1.persistent), transient: $0.meet(v._1.transient)(v1._1.transient)});
        }
        $runtime.fail();
      }
    };
    return {top: $SelState("Reactive", {persistent: top, transient: top}), MeetSemilattice0: () => meetSemilatticeSelStates1};
  };
};
const boundedMeetSemilatticeSel1 = dictBounded => {
  const top = dictBounded.top;
  return dictBoundedMeetSemilattice => {
    const $0 = dictBoundedMeetSemilattice.MeetSemilattice0();
    const meetSemilatticeSelState1 = {
      meet: v => v1 => {
        if (v1.tag === "Inert") { return Inert; }
        if (v.tag === "Inert") { return Inert; }
        if (v.tag === "Reactive" && v1.tag === "Reactive") { return $SelState("Reactive", $0.meet(v._1)(v1._1)); }
        $runtime.fail();
      }
    };
    return {top: $SelState("Reactive", top), MeetSemilattice0: () => meetSemilatticeSelState1};
  };
};
const boundedJoinSemilatticeSel = dictBoundedJoinSemilattice => {
  const $0 = dictBoundedJoinSemilattice.JoinSemilattice0();
  const joinSemilatticeSelStates1 = {
    join: v => v1 => {
      if (v1.tag === "Inert") { return v; }
      if (v.tag === "Inert") { return v1; }
      if (v.tag === "Reactive" && v1.tag === "Reactive") {
        return $SelState("Reactive", {persistent: $0.join(v._1.persistent)(v1._1.persistent), transient: $0.join(v._1.transient)(v1._1.transient)});
      }
      $runtime.fail();
    }
  };
  return {bot: Inert, JoinSemilattice0: () => joinSemilatticeSelStates1};
};
const boundedJoinSemilatticeSel1 = dictBoundedJoinSemilattice => {
  const $0 = dictBoundedJoinSemilattice.JoinSemilattice0();
  const joinSemilatticeSelState1 = {
    join: v => v1 => {
      if (v1.tag === "Inert") { return v; }
      if (v.tag === "Inert") { return v1; }
      if (v.tag === "Reactive" && v1.tag === "Reactive") { return $SelState("Reactive", $0.join(v._1)(v1._1)); }
      $runtime.fail();
    }
  };
  return {bot: Inert, JoinSemilattice0: () => joinSemilatticeSelState1};
};
const applySelStates = {
  apply: v => v1 => {
    if (v.tag === "Inert") {
      if (v1.tag === "Inert") { return Inert; }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
    if (v.tag === "Reactive" && v1.tag === "Reactive") {
      return $SelState("Reactive", {persistent: v._1.persistent(v1._1.persistent), transient: v._1.transient(v1._1.transient)});
    }
    return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
  },
  Functor0: () => functorSelStates
};
const applySelState = {
  apply: v => v1 => {
    if (v.tag === "Inert") {
      if (v1.tag === "Inert") { return Inert; }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
    if (v.tag === "Reactive" && v1.tag === "Reactive") { return $SelState("Reactive", v._1(v1._1)); }
    return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
  },
  Functor0: () => functorSelState
};
const unselected = /* #__PURE__ */ $SelState("Reactive", {persistent: false, transient: false});
const to𝕊 = v => {
  if (v) { return Primary; }
  return None;
};
const to𝔹 = v => {
  if (v.tag === "Inert") { return false; }
  if (v.tag === "Reactive") { return v._1; }
  $runtime.fail();
};
const selector = v => v1 => Data$dTuple.$Tuple(
  Val.functorVal.map(x => Util.spyWhen(false)("to ")(show1)((() => {
    if (x.tag === "Inert") { return Inert; }
    if (x.tag === "Reactive") {
      if (v === "mousedown") { return $SelState("Reactive", {persistent: !x._1.persistent, transient: x._1.transient}); }
      if (v === "mouseenter") { return $SelState("Reactive", {transient: true, persistent: x._1.persistent}); }
      if (v === "mouseleave") { return $SelState("Reactive", {transient: false, persistent: x._1.persistent}); }
      return Effect$dException.throwException(Effect$dException.error("Unsupported event type"))();
    }
    $runtime.fail();
  })()))(Util.spyWhen(false)("Setting selStates of ")(prettyP)(v1)),
  (() => {
    if (v === "mousedown") { return Persistent; }
    if (v === "mouseenter") { return Transient; }
    if (v === "mouseleave") { return Transient; }
    return Effect$dException.throwException(Effect$dException.error("Unsupported event type"))();
  })()
);
const selection = v => v1 => v2 => {
  if (v) { return {persistent: Inert, transient: Inert}; }
  return {persistent: $SelState("Reactive", v1), transient: $SelState("Reactive", v2)};
};
const selStates = v => v1 => v2 => {
  if (v) { return Inert; }
  return $SelState("Reactive", {persistent: v1, transient: v2});
};
const selState = v => {
  if (v) { return v$1 => Inert; }
  return Reactive;
};
const sel = Data$dTuple.snd;
const runAffs_ = f => as1 => {
  const $0 = Effect$dAff.runAff(v => {
    if (v.tag === "Left") { return Effect$dConsole.log(Effect$dException.showErrorImpl(v._1)); }
    if (v.tag === "Right") { return sequence_(Data$dFunctor.arrayMap(f)(v._1)); }
    $runtime.fail();
  })(sequence(as1));
  return () => {$0();};
};
const primary = dictApply => x => dictApply.Functor0().map(v => {
  if (v.tag === "Inert") { return Inert; }
  if (v.tag === "Reactive") { return $SelState("Reactive", v._1 ? Primary : None); }
  $runtime.fail();
})(x);
const isInert = v => {
  if (v.tag === "Inert") { return true; }
  if (v.tag === "Reactive") { return false; }
  $runtime.fail();
};
const inert = Inert;
const get_intOrNumber = x => r => {
  const $0 = Util$dMap.get(Data$dShow.showString)(Dict.mapDictString)(x)(r);
  const $1 = Primitive.intOrNumber.unpack($0._2._3);
  return Data$dTuple.$Tuple(
    (() => {
      if ($1.tag === "Left") { return Data$dInt.toNumber($1._1); }
      if ($1.tag === "Right") { return $1._1; }
      $runtime.fail();
    })(),
    $0._2._1
  );
};
const getTransient = dictBoundedJoinSemilattice => {
  const bot = dictBoundedJoinSemilattice.bot;
  return v => {
    if (v.tag === "Inert") { return bot; }
    if (v.tag === "Reactive") { return v._1.transient; }
    $runtime.fail();
  };
};
const getSel = selType => s => {
  const $0 = (() => {
    if (selType === "Persistent") { return v => v.persistent; }
    if (selType === "Transient") { return v => v.transient; }
    $runtime.fail();
  })();
  if (s.tag === "Inert") { return Inert; }
  if (s.tag === "Reactive") { return $SelState("Reactive", $0(s._1)); }
  $runtime.fail();
};
const getPersistent = dictBoundedJoinSemilattice => {
  const bot = dictBoundedJoinSemilattice.bot;
  return v => {
    if (v.tag === "Inert") { return bot; }
    if (v.tag === "Reactive") { return v._1.persistent; }
    $runtime.fail();
  };
};
const eventData = x => Util.definitely("absurd")(Data$dNullable.nullable(Web$dEvent$dEvent._target(x), Data$dMaybe.Nothing, Data$dMaybe.Just)).__data__;
const selectionEventData$p = /* #__PURE__ */ Data$dProfunctor$dStrong.fanout(Control$dCategory.categoryFn)(Data$dProfunctor$dStrong.strongFn)(eventData)(x => selector(Web$dEvent$dEvent.type_(x)));
const dict = toDict => v => toDict(v._3.tag === "Dictionary" ? v._3._1 : Primitive.typeError(v._3)("Dictionary"));
const css = {
  sel: {
    transient: {primary: "selected-primary-transient", secondary: "selected-secondary-transient"},
    persistent: {primary: "selected-primary-persistent", secondary: "selected-secondary-persistent"}
  },
  inert: "inert"
};
const selClasses = /* #__PURE__ */ (() => Data$dString$dCommon.joinWith(" ")([
  css.sel.transient.primary,
  css.sel.transient.secondary,
  css.sel.persistent.primary,
  css.sel.persistent.secondary,
  "inert"
]))();
const contents = Data$dTuple.fst;
const compare$p = v => v1 => {
  if (v === "None") {
    if (v1 === "None") { return Data$dOrdering.EQ; }
    return Data$dOrdering.LT;
  }
  if (v === "Secondary") {
    if (v1 === "Secondary") { return Data$dOrdering.EQ; }
    if (v1 === "Primary") { return Data$dOrdering.LT; }
    if (v1 === "None") { return Data$dOrdering.GT; }
    $runtime.fail();
  }
  if (v === "Primary") {
    if (v1 === "Primary") { return Data$dOrdering.EQ; }
    return Data$dOrdering.GT;
  }
  $runtime.fail();
};
const eq𝕊 = {
  eq: s => s$p => {
    if (s === "None") { return s$p === "None"; }
    if (s === "Secondary") {
      if (s$p === "Secondary") { return true; }
      if (s$p === "Primary") { return false; }
      if (s$p === "None") { return false; }
      $runtime.fail();
    }
    if (s === "Primary") { return s$p === "Primary"; }
    $runtime.fail();
  }
};
const isPrimary = v => {
  if (v.tag === "Inert") { return false; }
  if (v.tag === "Reactive") {
    return (() => {
      if (v._1.persistent === "None") { return false; }
      if (v._1.persistent === "Secondary") { return false; }
      if (v._1.persistent === "Primary") { return true; }
      $runtime.fail();
    })() || (() => {
      if (v._1.transient === "None") { return false; }
      if (v._1.transient === "Secondary") { return false; }
      if (v._1.transient === "Primary") { return true; }
      $runtime.fail();
    })();
  }
  $runtime.fail();
};
const isSecondary = v => {
  if (v.tag === "Inert") { return false; }
  if (v.tag === "Reactive") {
    return (() => {
      if (v._1.persistent === "None") { return false; }
      if (v._1.persistent === "Secondary") { return true; }
      if (v._1.persistent === "Primary") { return false; }
      $runtime.fail();
    })() || (() => {
      if (v._1.transient === "None") { return false; }
      if (v._1.transient === "Secondary") { return true; }
      if (v._1.transient === "Primary") { return false; }
      $runtime.fail();
    })();
  }
  $runtime.fail();
};
const ord𝕊 = {compare: compare$p, Eq0: () => eq𝕊};
const joinSemilattice𝕊 = {
  join: x => y => {
    if (x === "None") {
      if (y === "None") { return x; }
      return y;
    }
    if (x === "Secondary") {
      if (y === "Secondary") { return x; }
      if (y === "Primary") { return y; }
      if (y === "None") { return x; }
      $runtime.fail();
    }
    if (x === "Primary") { return x; }
    $runtime.fail();
  }
};
const boundedJoinSemilattice𝕊 = {bot: None, JoinSemilattice0: () => joinSemilattice𝕊};
const isPersistent = x => {
  const $0 = (() => {
    if (x.tag === "Inert") { return None; }
    if (x.tag === "Reactive") { return x._1.persistent; }
    $runtime.fail();
  })();
  if ($0 === "None") { return false; }
  if ($0 === "Secondary") { return true; }
  if ($0 === "Primary") { return true; }
  $runtime.fail();
};
const isTransient = x => {
  const $0 = (() => {
    if (x.tag === "Inert") { return None; }
    if (x.tag === "Reactive") { return x._1.transient; }
    $runtime.fail();
  })();
  if ($0 === "None") { return false; }
  if ($0 === "Secondary") { return true; }
  if ($0 === "Primary") { return true; }
  $runtime.fail();
};
const selClassesFor = v => {
  if (v.tag === "Inert") { return "inert"; }
  return Data$dString$dCommon.joinWith(" ")(Data$dArray.concat([
    (() => {
      const v1 = (() => {
        if (v.tag === "Inert") { return None; }
        if (v.tag === "Reactive") { return v._1.persistent; }
        $runtime.fail();
      })();
      if (v1 === "Secondary") { return [css.sel.persistent.secondary]; }
      if (v1 === "Primary") { return [css.sel.persistent.primary]; }
      if (v1 === "None") { return []; }
      $runtime.fail();
    })(),
    (() => {
      const v1 = (() => {
        if (v.tag === "Inert") { return None; }
        if (v.tag === "Reactive") { return v._1.transient; }
        $runtime.fail();
      })();
      if (v1 === "Secondary") { return [css.sel.transient.secondary]; }
      if (v1 === "Primary") { return [css.sel.transient.primary]; }
      if (v1 === "None") { return []; }
      $runtime.fail();
    })()
  ]));
};
const meetSemilattice𝕊 = {
  meet: x => y => {
    if (x === "None") { return x; }
    if (x === "Secondary") {
      if (y === "Secondary") { return x; }
      if (y === "Primary") { return x; }
      if (y === "None") { return y; }
      $runtime.fail();
    }
    if (x === "Primary") {
      if (y === "Primary") { return x; }
      return y;
    }
    $runtime.fail();
  }
};
const colorShade = col => n => {
  const shade = rgbComponent => {
    const s = Data$dInt.toStringAs(16)(clamp(0)(255)(Util.definitely("absurd")(Data$dInt.fromStringAs(16)(rgbComponent)) + n | 0));
    if (Data$dString$dCodePoints.toCodePointArray(s).length === 1) { return "0" + s; }
    return s;
  };
  return "#" + shade(Data$dString$dCodeUnits.take(2)(Data$dString$dCodeUnits.drop(1)(col))) + shade(Data$dString$dCodeUnits.take(2)(Data$dString$dCodeUnits.drop(3)(col))) + shade(Data$dString$dCodeUnits.take(2)(Data$dString$dCodeUnits.drop(5)(col)));
};
const classes = /* #__PURE__ */ (() => {
  const $0 = Data$dString$dCommon.joinWith(" ");
  return x => Data$dTuple.$Tuple("class", $0(x));
})();
const attrs = /* #__PURE__ */ Data$dFoldable.foldlArray(kvs => x => Foreign$dObject.union(kvs)(fromFoldable(x)))(Foreign$dObject.empty);
const as𝕊 = v => v1 => {
  if (!v) {
    if (!v1) { return None; }
    if (v1) { return Secondary; }
    $runtime.fail();
  }
  if (v) {
    if (!v1) { return None; }
    if (v1) { return Primary; }
  }
  $runtime.fail();
};
const primaryOrSecondary = dictApply => {
  const $0 = dictApply.Functor0();
  return selType => x => x$p => dictApply.apply($0.map(a => b => applySelState.apply((() => {
    if (a.tag === "Inert") { return Inert; }
    if (a.tag === "Reactive") {
      return $SelState(
        "Reactive",
        (() => {
          const $1 = a._1;
          return v1 => {
            if (!$1) {
              if (!v1) { return None; }
              if (v1) { return Secondary; }
              $runtime.fail();
            }
            if ($1) {
              if (!v1) { return None; }
              if (v1) { return Primary; }
            }
            $runtime.fail();
          };
        })()
      );
    }
    $runtime.fail();
  })())(b))($0.map(s => {
    const $1 = (() => {
      if (selType === "Persistent") { return v => v.persistent; }
      if (selType === "Transient") { return v => v.transient; }
      $runtime.fail();
    })();
    if (s.tag === "Inert") { return Inert; }
    if (s.tag === "Reactive") { return $SelState("Reactive", $1(s._1)); }
    $runtime.fail();
  })(x)))(x$p);
};
export {
  $SelState,
  
  
  
  
  Inert,
  None,
  Persistent,
  Primary,
  
  
  
  
  
  Transient,
  
  
  
  
  
  
  
  
  
  
  classes,
  colorShade,
  
  
  
  dict,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  get_intOrNumber,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  primaryOrSecondary,
  runAffs_,
  
  selClasses,
  selClassesFor,
  selState,
  selStates,
  
  selectionEventData$p,
  
  
  
  
  
  
  
  
  to𝔹,
  
  
};
