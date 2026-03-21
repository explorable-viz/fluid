import * as $runtime from "../runtime.js";
import * as Data$dLazy from "../Data.Lazy/index.js";
import * as Data$dList$dLazy from "../Data.List.Lazy/index.js";
import * as Data$dList$dLazy$dTypes from "../Data.List.Lazy.Types/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNonEmpty from "../Data.NonEmpty/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const uncons = v => {
  const v1 = Data$dLazy.force(v);
  return {head: v1._1, tail: v1._2};
};
const toList = v => {
  const v1 = Data$dLazy.force(v);
  const $0 = v1._1;
  const $1 = v1._2;
  return Data$dLazy.defer(v$1 => Data$dList$dLazy$dTypes.$Step("Cons", $0, $1));
};
const toUnfoldable = dictUnfoldable => {
  const $0 = dictUnfoldable.unfoldr(xs => {
    const $0 = Data$dList$dLazy.uncons(xs);
    if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple($0._1.head, $0._1.tail)); }
    return Data$dMaybe.Nothing;
  });
  return x => $0(toList(x));
};
const tail = v => Data$dLazy.force(v)._2;
const singleton = /* #__PURE__ */ (() => Data$dList$dLazy$dTypes.applicativeNonEmptyList.pure)();
const repeat = x => Data$dLazy.defer(v => Data$dNonEmpty.$NonEmpty(x, Data$dList$dLazy.repeat(x)));
const length = v => 1 + Data$dList$dLazy.length(Data$dLazy.force(v)._2) | 0;
const last = v => {
  const v1 = Data$dLazy.force(v);
  const $0 = Data$dList$dLazy.last(v1._2);
  if ($0.tag === "Nothing") { return v1._1; }
  if ($0.tag === "Just") { return $0._1; }
  $runtime.fail();
};
const iterate = f => x => Data$dLazy.defer(v => Data$dNonEmpty.$NonEmpty(x, Data$dList$dLazy.iterate(f)(f(x))));
const init = v => {
  const v1 = Data$dLazy.force(v);
  const $0 = Data$dList$dLazy.init(v1._2);
  if ($0.tag === "Nothing") { return Data$dList$dLazy$dTypes.nil; }
  if ($0.tag === "Just") {
    const $1 = $0._1;
    return Data$dLazy.defer(v$1 => Data$dList$dLazy$dTypes.$Step("Cons", v1._1, $1));
  }
  $runtime.fail();
};
const head = v => Data$dLazy.force(v)._1;
const fromList = l => {
  const v = Data$dLazy.force(l);
  if (v.tag === "Nil") { return Data$dMaybe.Nothing; }
  if (v.tag === "Cons") {
    const $0 = v._1;
    const $1 = v._2;
    return Data$dMaybe.$Maybe("Just", Data$dLazy.defer(v1 => Data$dNonEmpty.$NonEmpty($0, $1)));
  }
  $runtime.fail();
};
const fromFoldable = dictFoldable => {
  const $0 = dictFoldable.foldr(Data$dList$dLazy$dTypes.cons)(Data$dList$dLazy$dTypes.nil);
  return x => fromList($0(x));
};
const cons = y => v => Data$dLazy.defer(v1 => {
  const v2 = Data$dLazy.force(v);
  const $0 = v2._1;
  const $1 = v2._2;
  return Data$dNonEmpty.$NonEmpty(y, Data$dLazy.defer(v$1 => Data$dList$dLazy$dTypes.$Step("Cons", $0, $1)));
});
const concatMap = b => a => Data$dList$dLazy$dTypes.bindNonEmptyList.bind(a)(b);
const appendFoldable = dictFoldable => {
  const fromFoldable1 = dictFoldable.foldr(Data$dList$dLazy$dTypes.cons)(Data$dList$dLazy$dTypes.nil);
  return nel => ys => Data$dLazy.defer(v => Data$dNonEmpty.$NonEmpty(
    Data$dLazy.force(nel)._1,
    Data$dList$dLazy$dTypes.semigroupList.append(Data$dLazy.force(nel)._2)(fromFoldable1(ys))
  ));
};
export {appendFoldable, concatMap, cons, fromFoldable, fromList, head, init, iterate, last, length, repeat, singleton, tail, toList, toUnfoldable, uncons};
