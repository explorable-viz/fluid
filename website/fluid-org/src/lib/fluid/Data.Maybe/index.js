import * as $runtime from "../runtime.js";
import * as Data$dGeneric$dRep from "../Data.Generic.Rep/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
const $Maybe = (tag, _1) => ({tag, _1});
const Nothing = /* #__PURE__ */ $Maybe("Nothing");
const Just = value0 => $Maybe("Just", value0);
const showMaybe = dictShow => (
  {
    show: v => {
      if (v.tag === "Just") { return "(Just " + dictShow.show(v._1) + ")"; }
      if (v.tag === "Nothing") { return "Nothing"; }
      $runtime.fail();
    }
  }
);
const semigroupMaybe = dictSemigroup => (
  {
    append: v => v1 => {
      if (v.tag === "Nothing") { return v1; }
      if (v1.tag === "Nothing") { return v; }
      if (v.tag === "Just" && v1.tag === "Just") { return $Maybe("Just", dictSemigroup.append(v._1)(v1._1)); }
      $runtime.fail();
    }
  }
);
const optional = dictAlt => dictApplicative => a => dictAlt.alt(dictAlt.Functor0().map(Just)(a))(dictApplicative.pure(Nothing));
const monoidMaybe = dictSemigroup => {
  const semigroupMaybe1 = {
    append: v => v1 => {
      if (v.tag === "Nothing") { return v1; }
      if (v1.tag === "Nothing") { return v; }
      if (v.tag === "Just" && v1.tag === "Just") { return $Maybe("Just", dictSemigroup.append(v._1)(v1._1)); }
      $runtime.fail();
    }
  };
  return {mempty: Nothing, Semigroup0: () => semigroupMaybe1};
};
const maybe$p = v => v1 => v2 => {
  if (v2.tag === "Nothing") { return v(); }
  if (v2.tag === "Just") { return v1(v2._1); }
  $runtime.fail();
};
const maybe = v => v1 => v2 => {
  if (v2.tag === "Nothing") { return v; }
  if (v2.tag === "Just") { return v1(v2._1); }
  $runtime.fail();
};
const isNothing = v2 => {
  if (v2.tag === "Nothing") { return true; }
  if (v2.tag === "Just") { return false; }
  $runtime.fail();
};
const isJust = v2 => {
  if (v2.tag === "Nothing") { return false; }
  if (v2.tag === "Just") { return true; }
  $runtime.fail();
};
const genericMaybe = {
  to: x => {
    if (x.tag === "Inl") { return Nothing; }
    if (x.tag === "Inr") { return $Maybe("Just", x._1); }
    $runtime.fail();
  },
  from: x => {
    if (x.tag === "Nothing") { return Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments); }
    if (x.tag === "Just") { return Data$dGeneric$dRep.$Sum("Inr", x._1); }
    $runtime.fail();
  }
};
const functorMaybe = {
  map: v => v1 => {
    if (v1.tag === "Just") { return $Maybe("Just", v(v1._1)); }
    return Nothing;
  }
};
const invariantMaybe = {
  imap: f => v => v1 => {
    if (v1.tag === "Just") { return $Maybe("Just", f(v1._1)); }
    return Nothing;
  }
};
const fromMaybe$p = a => v2 => {
  if (v2.tag === "Nothing") { return a(); }
  if (v2.tag === "Just") { return v2._1; }
  $runtime.fail();
};
const fromMaybe = a => v2 => {
  if (v2.tag === "Nothing") { return a; }
  if (v2.tag === "Just") { return v2._1; }
  $runtime.fail();
};
const fromJust = () => v => {
  if (v.tag === "Just") { return v._1; }
  $runtime.fail();
};
const extendMaybe = {
  extend: v => v1 => {
    if (v1.tag === "Nothing") { return Nothing; }
    return $Maybe("Just", v(v1));
  },
  Functor0: () => functorMaybe
};
const eqMaybe = dictEq => (
  {
    eq: x => y => {
      if (x.tag === "Nothing") { return y.tag === "Nothing"; }
      return x.tag === "Just" && y.tag === "Just" && dictEq.eq(x._1)(y._1);
    }
  }
);
const ordMaybe = dictOrd => {
  const $0 = dictOrd.Eq0();
  const eqMaybe1 = {
    eq: x => y => {
      if (x.tag === "Nothing") { return y.tag === "Nothing"; }
      return x.tag === "Just" && y.tag === "Just" && $0.eq(x._1)(y._1);
    }
  };
  return {
    compare: x => y => {
      if (x.tag === "Nothing") {
        if (y.tag === "Nothing") { return Data$dOrdering.EQ; }
        return Data$dOrdering.LT;
      }
      if (y.tag === "Nothing") { return Data$dOrdering.GT; }
      if (x.tag === "Just" && y.tag === "Just") { return dictOrd.compare(x._1)(y._1); }
      $runtime.fail();
    },
    Eq0: () => eqMaybe1
  };
};
const eq1Maybe = {
  eq1: dictEq => x => y => {
    if (x.tag === "Nothing") { return y.tag === "Nothing"; }
    return x.tag === "Just" && y.tag === "Just" && dictEq.eq(x._1)(y._1);
  }
};
const ord1Maybe = {
  compare1: dictOrd => x => y => {
    if (x.tag === "Nothing") {
      if (y.tag === "Nothing") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y.tag === "Nothing") { return Data$dOrdering.GT; }
    if (x.tag === "Just" && y.tag === "Just") { return dictOrd.compare(x._1)(y._1); }
    $runtime.fail();
  },
  Eq10: () => eq1Maybe
};
const boundedMaybe = dictBounded => {
  const $0 = dictBounded.Ord0();
  const $1 = $0.Eq0();
  const ordMaybe1 = (() => {
    const eqMaybe1 = {
      eq: x => y => {
        if (x.tag === "Nothing") { return y.tag === "Nothing"; }
        return x.tag === "Just" && y.tag === "Just" && $1.eq(x._1)(y._1);
      }
    };
    return {
      compare: x => y => {
        if (x.tag === "Nothing") {
          if (y.tag === "Nothing") { return Data$dOrdering.EQ; }
          return Data$dOrdering.LT;
        }
        if (y.tag === "Nothing") { return Data$dOrdering.GT; }
        if (x.tag === "Just" && y.tag === "Just") { return $0.compare(x._1)(y._1); }
        $runtime.fail();
      },
      Eq0: () => eqMaybe1
    };
  })();
  return {top: $Maybe("Just", dictBounded.top), bottom: Nothing, Ord0: () => ordMaybe1};
};
const applyMaybe = {
  apply: v => v1 => {
    if (v.tag === "Just") {
      if (v1.tag === "Just") { return $Maybe("Just", v._1(v1._1)); }
      return Nothing;
    }
    if (v.tag === "Nothing") { return Nothing; }
    $runtime.fail();
  },
  Functor0: () => functorMaybe
};
const bindMaybe = {
  bind: v => v1 => {
    if (v.tag === "Just") { return v1(v._1); }
    if (v.tag === "Nothing") { return Nothing; }
    $runtime.fail();
  },
  Apply0: () => applyMaybe
};
const semiringMaybe = dictSemiring => (
  {
    zero: Nothing,
    one: $Maybe("Just", dictSemiring.one),
    add: v => v1 => {
      if (v.tag === "Nothing") { return v1; }
      if (v1.tag === "Nothing") { return v; }
      if (v.tag === "Just" && v1.tag === "Just") { return $Maybe("Just", dictSemiring.add(v._1)(v1._1)); }
      $runtime.fail();
    },
    mul: x => y => {
      if (x.tag === "Just" && y.tag === "Just") { return $Maybe("Just", dictSemiring.mul(x._1)(y._1)); }
      return Nothing;
    }
  }
);
const applicativeMaybe = {pure: Just, Apply0: () => applyMaybe};
const monadMaybe = {Applicative0: () => applicativeMaybe, Bind1: () => bindMaybe};
const altMaybe = {
  alt: v => v1 => {
    if (v.tag === "Nothing") { return v1; }
    return v;
  },
  Functor0: () => functorMaybe
};
const plusMaybe = {empty: Nothing, Alt0: () => altMaybe};
const alternativeMaybe = {Applicative0: () => applicativeMaybe, Plus1: () => plusMaybe};
export {
  $Maybe,
  Just,
  Nothing,
  
  
  applicativeMaybe,
  applyMaybe,
  bindMaybe,
  boundedMaybe,
  eq1Maybe,
  
  extendMaybe,
  
  fromMaybe,
  
  functorMaybe,
  
  invariantMaybe,
  isJust,
  isNothing,
  
  
  monadMaybe,
  
  
  ord1Maybe,
  
  
  
  
  
};
