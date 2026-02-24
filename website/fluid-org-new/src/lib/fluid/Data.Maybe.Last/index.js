import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
const Last = x => x;
const showLast = dictShow => (
  {
    show: v => {
      if (v.tag === "Just") { return "(Last (Just " + dictShow.show(v._1) + "))"; }
      if (v.tag === "Nothing") { return "(Last Nothing)"; }
      $runtime.fail();
    }
  }
);
const semigroupLast = {
  append: v => v1 => {
    if (v1.tag === "Just") { return v1; }
    if (v1.tag === "Nothing") { return v; }
    $runtime.fail();
  }
};
const ordLast = dictOrd => {
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
const ord1Last = Data$dMaybe.ord1Maybe;
const newtypeLast = {Coercible0: () => {}};
const monoidLast = {mempty: Data$dMaybe.Nothing, Semigroup0: () => semigroupLast};
const monadLast = Data$dMaybe.monadMaybe;
const invariantLast = Data$dMaybe.invariantMaybe;
const functorLast = Data$dMaybe.functorMaybe;
const extendLast = Data$dMaybe.extendMaybe;
const eqLast = dictEq => (
  {
    eq: x => y => {
      if (x.tag === "Nothing") { return y.tag === "Nothing"; }
      return x.tag === "Just" && y.tag === "Just" && dictEq.eq(x._1)(y._1);
    }
  }
);
const eq1Last = Data$dMaybe.eq1Maybe;
const boundedLast = dictBounded => Data$dMaybe.boundedMaybe(dictBounded);
const bindLast = Data$dMaybe.bindMaybe;
const applyLast = Data$dMaybe.applyMaybe;
const applicativeLast = Data$dMaybe.applicativeMaybe;
const altLast = /* #__PURE__ */ (() => ({alt: semigroupLast.append, Functor0: () => Data$dMaybe.functorMaybe}))();
const plusLast = {empty: Data$dMaybe.Nothing, Alt0: () => altLast};
const alternativeLast = {Applicative0: () => Data$dMaybe.applicativeMaybe, Plus1: () => plusLast};
export {
  Last,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
};
