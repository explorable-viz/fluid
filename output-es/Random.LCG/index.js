import * as $runtime from "../runtime.js";
import * as Data$dEuclideanRing from "../Data.EuclideanRing/index.js";
import * as Data$dInt from "../Data.Int/index.js";
import * as Data$dNumber from "../Data.Number/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Effect$dRandom from "../Effect.Random/index.js";
const unSeed = v => v;
const showSeed = {show: v => "Seed " + Data$dShow.showIntImpl(v)};
const lcgM = 2147483647;
const mkSeed = x => {
  const n$p = Data$dEuclideanRing.intMod(x)(2147483645);
  if (n$p < 1) { return n$p + 2147483646 | 0; }
  return n$p;
};
const randomSeed = /* #__PURE__ */ (() => {
  const $0 = Effect$dRandom.randomInt(1)(2147483646);
  return () => {
    const a$p = $0();
    const n$p = Data$dEuclideanRing.intMod(a$p)(2147483645);
    if (n$p < 1) { return n$p + 2147483646 | 0; }
    return n$p;
  };
})();
const lcgC = 0;
const lcgA = 48271;
const lcgPerturb = d => v => {
  const $0 = Data$dInt.fromNumber(Data$dNumber.remainder(Data$dInt.toNumber(48271) * Data$dInt.toNumber(v) + Data$dInt.toNumber(d))(Data$dInt.toNumber(2147483647)));
  if ($0.tag === "Just") { return $0._1; }
  $runtime.fail();
};
const lcgNext = /* #__PURE__ */ lcgPerturb(0);
const eqSeed = {eq: x => y => x === y};
const ordSeed = {compare: x => y => Data$dOrd.ordInt.compare(x)(y), Eq0: () => eqSeed};
export {eqSeed, lcgA, lcgC, lcgM, lcgNext, lcgPerturb, mkSeed, ordSeed, randomSeed, showSeed, unSeed};
