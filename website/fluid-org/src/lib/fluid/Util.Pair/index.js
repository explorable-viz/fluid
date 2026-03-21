import * as $runtime from "../runtime.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dList from "../Data.List/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Data$dTraversable from "../Data.Traversable/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const $Pair = (_1, _2) => ({tag: "Pair", _1, _2});
const Pair = value0 => value1 => $Pair(value0, value1);
const showPair = dictShow => ({show: v => "(Pair " + dictShow.show(v._1) + " " + dictShow.show(v._2) + ")"});
const functorPair = {map: f => v => $Pair(f(v._1), f(v._2))};
const foldablePair = {
  foldl: f => z => v => f(f(z)(v._1))(v._2),
  foldr: f => Data$dFoldable.foldrDefault(foldablePair)(f),
  foldMap: dictMonoid => f => foldablePair.foldl(acc => x => dictMonoid.Semigroup0().append(acc)(f(x)))(dictMonoid.mempty)
};
const traversablePair = {
  traverse: dictApplicative => {
    const Apply0 = dictApplicative.Apply0();
    return f => v => Apply0.apply(Apply0.Functor0().map(Pair)(f(v._1)))(f(v._2));
  },
  sequence: dictApplicative => traversablePair.traverse(dictApplicative)(Data$dTraversable.identity),
  Functor0: () => functorPair,
  Foldable1: () => foldablePair
};
const eqPair = dictEq => ({eq: x => y => dictEq.eq(x._1)(y._1) && dictEq.eq(x._2)(y._2)});
const ordPair = dictOrd => {
  const $0 = dictOrd.Eq0();
  const eqPair1 = {eq: x => y => $0.eq(x._1)(y._1) && $0.eq(x._2)(y._2)};
  return {
    compare: x => y => {
      const v = dictOrd.compare(x._1)(y._1);
      if (v === "LT") { return Data$dOrdering.LT; }
      if (v === "GT") { return Data$dOrdering.GT; }
      return dictOrd.compare(x._2)(y._2);
    },
    Eq0: () => eqPair1
  };
};
const applyPair = {apply: v => v1 => $Pair(v._1(v1._1), v._2(v1._2)), Functor0: () => functorPair};
const applicativePair = {pure: x => $Pair(x, x), Apply0: () => applyPair};
const toTuple = v => Data$dTuple.$Tuple(v._1, v._2);
const unzip = xys => Data$dList.unzip(Data$dList$dTypes.listMap(toTuple)(xys));
const fromTuple = v => $Pair(v._1, v._2);
const zip = xs => ys => {
  const go = go$a0$copy => go$a1$copy => go$a2$copy => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$a2 = go$a2$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0, v1 = go$a1, v2 = go$a2;
      if (v.tag === "Nil") {
        go$c = false;
        go$r = v2;
        continue;
      }
      if (v1.tag === "Nil") {
        go$c = false;
        go$r = v2;
        continue;
      }
      if (v.tag === "Cons" && v1.tag === "Cons") {
        go$a0 = v._2;
        go$a1 = v1._2;
        go$a2 = Data$dList$dTypes.$List("Cons", Data$dTuple.$Tuple(v._1, v1._1), v2);
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  const go$1 = go$1$a0$copy => go$1$a1$copy => {
    let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
    while (go$1$c) {
      const v = go$1$a0, v1 = go$1$a1;
      if (v1.tag === "Nil") {
        go$1$c = false;
        go$1$r = v;
        continue;
      }
      if (v1.tag === "Cons") {
        go$1$a0 = Data$dList$dTypes.$List("Cons", v1._1, v);
        go$1$a1 = v1._2;
        continue;
      }
      $runtime.fail();
    }
    return go$1$r;
  };
  return Data$dList$dTypes.listMap(fromTuple)(go$1(Data$dList$dTypes.Nil)(go(xs)(ys)(Data$dList$dTypes.Nil)));
};
export {$Pair,     foldablePair,      traversablePair, unzip, };
