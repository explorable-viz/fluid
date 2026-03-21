// | This module defines the random generator monad used by the `Test.QuickCheck`
// | module, as well as helper functions for constructing random generators.
import * as $runtime from "../runtime.js";
import * as Control$dMonad$dRec$dClass from "../Control.Monad.Rec.Class/index.js";
import * as Control$dMonad$dState$dTrans from "../Control.Monad.State.Trans/index.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dArray$dNonEmpty$dInternal from "../Data.Array.NonEmpty.Internal/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dIdentity from "../Data.Identity/index.js";
import * as Data$dInt from "../Data.Int/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNumber from "../Data.Number/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Data$dUnfoldable from "../Data.Unfoldable/index.js";
import * as Random$dLCG from "../Random.LCG/index.js";
import {float32ToInt32} from "./foreign.js";
const monadStateStateT = /* #__PURE__ */ Control$dMonad$dState$dTrans.monadStateStateT(Data$dIdentity.monadIdentity);
const bindStateT = /* #__PURE__ */ Control$dMonad$dState$dTrans.bindStateT(Data$dIdentity.monadIdentity);
const functorStateT = {
  map: f => v => s => {
    const $0 = v(s);
    return Data$dTuple.$Tuple(f($0._1), $0._2);
  }
};
const toUnfoldable = /* #__PURE__ */ (() => Data$dUnfoldable.unfoldableArray.unfoldr(xs => {
  if (xs.tag === "Nil") { return Data$dMaybe.Nothing; }
  if (xs.tag === "Cons") { return Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple(xs._1, xs._2)); }
  $runtime.fail();
}))();
const min = x => y => {
  const v = Data$dOrd.ordNumber.compare(x)(y);
  if (v === "LT") { return x; }
  if (v === "EQ") { return x; }
  if (v === "GT") { return y; }
  $runtime.fail();
};
const max = x => y => {
  const v = Data$dOrd.ordNumber.compare(x)(y);
  if (v === "LT") { return y; }
  if (v === "EQ") { return x; }
  if (v === "GT") { return x; }
  $runtime.fail();
};
const foldMap1 = /* #__PURE__ */ (() => Data$dArray$dNonEmpty$dInternal.foldable1NonEmptyArray.foldMap1({append: v => v1 => v + v1}))();
const unGen = v => v;
const runGen = x => x$1 => x(x$1);
const stateful = f => monadStateStateT.state(s => f(s)(s));
const sized = f => monadStateStateT.state(s => f(s.size)(s));
const variant = n => g => monadStateStateT.state(s => g({newSeed: n, size: s.size}));
const resize = sz => g => monadStateStateT.state(v => {
  const $0 = g({newSeed: v.newSeed, size: sz});
  return Data$dTuple.$Tuple($0._1, {size: v.size, newSeed: $0._2.newSeed});
});
const replicateMRec = dictMonadRec => {
  const Monad0 = dictMonadRec.Monad0();
  const $0 = Monad0.Applicative0();
  const $1 = Monad0.Bind1().Apply0().Functor0();
  return v => v1 => {
    if (v <= 0) { return $0.pure(Data$dList$dTypes.Nil); }
    return dictMonadRec.tailRecM(v2 => {
      if (v2._2 === 0) { return $0.pure(Control$dMonad$dRec$dClass.$Step("Done", v2._1)); }
      const $2 = v2._1;
      const $3 = v2._2;
      return $1.map(x => Control$dMonad$dRec$dClass.$Step("Loop", Data$dTuple.$Tuple(Data$dList$dTypes.$List("Cons", x, $2), $3 - 1 | 0)))(v1);
    })(Data$dTuple.$Tuple(Data$dList$dTypes.Nil, v));
  };
};
const repeatable = f => monadStateStateT.state(s => Data$dTuple.$Tuple(a => f(a)(s)._1, {newSeed: Random$dLCG.lcgPerturb(0)(s.newSeed), size: s.size}));
const perturbGen = n => gen => bindStateT.bind((() => {
  const $0 = monadStateStateT.state(s => {
    const s$p = {...s, newSeed: Random$dLCG.lcgPerturb(float32ToInt32(n))(s.newSeed)};
    return Data$dTuple.$Tuple(s$p, s$p);
  });
  return s => Data$dTuple.$Tuple(undefined, $0(s)._2);
})())(() => gen);
const monadRecGen = /* #__PURE__ */ Control$dMonad$dState$dTrans.monadRecStateT(Control$dMonad$dRec$dClass.monadRecIdentity);
const monadGen = {
  Applicative0: () => Control$dMonad$dState$dTrans.applicativeStateT(Data$dIdentity.monadIdentity),
  Bind1: () => Control$dMonad$dState$dTrans.bindStateT(Data$dIdentity.monadIdentity)
};
const listOf = /* #__PURE__ */ replicateMRec(monadRecGen);
const lcgStep = /* #__PURE__ */ (() => monadStateStateT.state(s => Data$dTuple.$Tuple(s.newSeed, {...s, newSeed: Random$dLCG.lcgPerturb(0)(s.newSeed)})))();
const lazyGen = Control$dMonad$dState$dTrans.lazyStateT;
const functorGen = functorStateT;
const uniform = s => {
  const $0 = lcgStep(s);
  return Data$dTuple.$Tuple(Data$dInt.toNumber($0._1) / Data$dInt.toNumber(2147483647), $0._2);
};
const vectorOf = k => g => {
  const $0 = listOf(k)(g);
  return s => {
    const $1 = $0(s);
    return Data$dTuple.$Tuple(toUnfoldable($1._1), $1._2);
  };
};
const evalGen = x => s => x(s)._1;
const randomSampleOne = gen => () => {
  const seed = Random$dLCG.randomSeed();
  return gen({newSeed: seed, size: 10})._1;
};
const sample = seed => sz => g => vectorOf(sz)(g)({newSeed: seed, size: sz})._1;
const randomSample$p = n => g => () => {
  const seed = Random$dLCG.randomSeed();
  return vectorOf(n)(g)({newSeed: seed, size: n})._1;
};
const randomSample = /* #__PURE__ */ randomSample$p(10);
const choose = a => b => {
  const min$p = min(a)(b) * 0.5;
  const $0 = max(a)(b) * 0.5 - min$p;
  return s => {
    const $1 = uniform(s);
    return Data$dTuple.$Tuple((min$p + $0 * $1._1) * 2.0, $1._2);
  };
};
const bindGen = bindStateT;
const frequency = xxs => {
  const $$default = (() => {
    if (0 < xxs.length) { return xxs[0]._2; }
    $runtime.fail();
  })();
  const pick = pick$a0$copy => pick$a1$copy => {
    let pick$a0 = pick$a0$copy, pick$a1 = pick$a1$copy, pick$c = true, pick$r;
    while (pick$c) {
      const i = pick$a0, n = pick$a1;
      if (i >= 0 && i < xxs.length) {
        if (n <= xxs[i]._1) {
          pick$c = false;
          pick$r = xxs[i]._2;
          continue;
        }
        pick$a0 = i + 1 | 0;
        pick$a1 = n - xxs[i]._1;
        continue;
      }
      pick$c = false;
      pick$r = $$default;
    }
    return pick$r;
  };
  return bindStateT.bind(choose(0.0)(foldMap1(x => x._1)(xxs)))(n => pick(0)(n));
};
const applyGen = /* #__PURE__ */ Control$dMonad$dState$dTrans.applyStateT(Data$dIdentity.monadIdentity);
const chooseInt$p = a => b => {
  const numB = Data$dInt.toNumber(b);
  const numA = Data$dInt.toNumber(a);
  const $0 = applyGen.apply(s => {
    const $0 = lcgStep(s);
    const $1 = Data$dInt.toNumber($0._1);
    return Data$dTuple.$Tuple($2 => $1 + $2, $0._2);
  })(s => {
    const $0 = lcgStep(s);
    return Data$dTuple.$Tuple(2.0 * Data$dInt.toNumber($0._1), $0._2);
  });
  return s => {
    const $1 = $0(s);
    return Data$dTuple.$Tuple(Data$dInt.unsafeClamp(Data$dNumber.floor(numA + Data$dNumber.remainder($1._1)(numB - numA + 1.0))), $1._2);
  };
};
const chooseInt = a => b => {
  if (a <= b) { return chooseInt$p(a)(b); }
  return chooseInt$p(b)(a);
};
const arrayOf = g => monadStateStateT.state(s => bindStateT.bind(0 <= s.size ? chooseInt$p(0)(s.size) : chooseInt$p(s.size)(0))(k => vectorOf(k)(g))(s));
const monadGenGen = {
  chooseInt,
  chooseFloat: choose,
  chooseBool: s => {
    const $0 = uniform(s);
    return Data$dTuple.$Tuple($0._1 < 0.5, $0._2);
  },
  resize: f => g => monadStateStateT.state(s => resize(f(s.size))(g)(s)),
  sized,
  Monad0: () => monadGen
};
const oneOf = xs => bindStateT.bind((() => {
  const $0 = xs.length - 1 | 0;
  if (0 <= $0) { return chooseInt$p(0)($0); }
  return chooseInt$p($0)(0);
})())(n => xs[n]);
const applicativeGen = /* #__PURE__ */ Control$dMonad$dState$dTrans.applicativeStateT(Data$dIdentity.monadIdentity);
const arrayOf1 = g => monadStateStateT.state(s => bindStateT.bind(0 <= s.size ? chooseInt$p(0)(s.size) : chooseInt$p(s.size)(0))(k => bindStateT.bind(g)(x => bindStateT.bind(vectorOf(k - 1 | 0)(g))(xs => applicativeGen.pure((() => {
  const $0 = [x, ...xs];
  if ($0.length > 0) { return $0; }
  $runtime.fail();
})()))))(s));
const elements = xs => bindStateT.bind((() => {
  const $0 = xs.length - 1 | 0;
  if (0 <= $0) { return chooseInt$p(0)($0); }
  return chooseInt$p($0)(0);
})())(n => applicativeGen.pure(xs[n]));
const $$enum = dictBoundedEnum => {
  const Bounded0 = dictBoundedEnum.Bounded0();
  return bindStateT.bind((() => {
    const $0 = dictBoundedEnum.fromEnum(Bounded0.bottom);
    const $1 = dictBoundedEnum.fromEnum(Bounded0.top);
    if ($0 <= $1) { return chooseInt$p($0)($1); }
    return chooseInt$p($1)($0);
  })())(i => applicativeGen.pure((() => {
    const $0 = dictBoundedEnum.toEnum(i);
    if ($0.tag === "Just") { return $0._1; }
    $runtime.fail();
  })()));
};
const shuffle = xs => bindStateT.bind(vectorOf(xs.length)(chooseInt$p(0)(2147483647)))(ns => applicativeGen.pure(Data$dFunctor.arrayMap(Data$dTuple.snd)(Data$dArray.sortBy(x => y => Data$dOrd.ordInt.compare(x._1)(y._1))(Data$dArray.zipWithImpl(
  Data$dTuple.Tuple,
  ns,
  xs
)))));
const suchThat = gen => pred => monadRecGen.tailRecM(v => bindStateT.bind(gen)(a => applicativeGen.pure(pred(a)
  ? Control$dMonad$dRec$dClass.$Step("Done", a)
  : Control$dMonad$dRec$dClass.$Step("Loop", undefined))))();
const altGen = /* #__PURE__ */ (() => {
  const functorStateT1 = {
    map: f => v => s => {
      const $0 = v(s);
      return Data$dTuple.$Tuple(f($0._1), $0._2);
    }
  };
  return {alt: v => v1 => s => v(s), Functor0: () => functorStateT1};
})();
export {
  altGen,
  applicativeGen,
  applyGen,
  arrayOf,
  arrayOf1,
  bindGen,
  bindStateT,
  choose,
  chooseInt,
  chooseInt$p,
  elements,
  $$enum as enum,
  evalGen,
  foldMap1,
  frequency,
  functorGen,
  functorStateT,
  lazyGen,
  lcgStep,
  listOf,
  max,
  min,
  monadGen,
  monadGenGen,
  monadRecGen,
  monadStateStateT,
  oneOf,
  perturbGen,
  randomSample,
  randomSample$p,
  randomSampleOne,
  repeatable,
  replicateMRec,
  resize,
  runGen,
  sample,
  shuffle,
  sized,
  stateful,
  suchThat,
  toUnfoldable,
  unGen,
  uniform,
  variant,
  vectorOf
};
export * from "./foreign.js";
