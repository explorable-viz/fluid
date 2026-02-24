import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
const First = x => x;
const showFirst = dictShow => (
  {
    show: v => {
      if (v.tag === "Just") { return "First ((Just " + dictShow.show(v._1) + "))"; }
      if (v.tag === "Nothing") { return "First (Nothing)"; }
      $runtime.fail();
    }
  }
);
const semigroupFirst = {
  append: v => v1 => {
    if (v.tag === "Just") { return v; }
    return v1;
  }
};
const ordFirst = dictOrd => {
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
const ord1First = Data$dMaybe.ord1Maybe;
const newtypeFirst = {Coercible0: () => {}};
const monoidFirst = {mempty: Data$dMaybe.Nothing, Semigroup0: () => semigroupFirst};
const monadFirst = Data$dMaybe.monadMaybe;
const invariantFirst = Data$dMaybe.invariantMaybe;
const functorFirst = Data$dMaybe.functorMaybe;
const extendFirst = Data$dMaybe.extendMaybe;
const eqFirst = dictEq => (
  {
    eq: x => y => {
      if (x.tag === "Nothing") { return y.tag === "Nothing"; }
      return x.tag === "Just" && y.tag === "Just" && dictEq.eq(x._1)(y._1);
    }
  }
);
const eq1First = Data$dMaybe.eq1Maybe;
const boundedFirst = dictBounded => Data$dMaybe.boundedMaybe(dictBounded);
const bindFirst = Data$dMaybe.bindMaybe;
const applyFirst = Data$dMaybe.applyMaybe;
const applicativeFirst = Data$dMaybe.applicativeMaybe;
const altFirst = /* #__PURE__ */ (() => ({alt: semigroupFirst.append, Functor0: () => Data$dMaybe.functorMaybe}))();
const plusFirst = {empty: Data$dMaybe.Nothing, Alt0: () => altFirst};
const alternativeFirst = {Applicative0: () => Data$dMaybe.applicativeMaybe, Plus1: () => plusFirst};
export {
  First,
  
  
  
  
  
  
  
  
  
  
  
  
  monoidFirst,
  
  
  
  
  
  
};
