// | These functions allow PureScript strings to be treated as if they were
// | sequences of Unicode code points instead of their true underlying
// | implementation (sequences of UTF-16 code units). For nearly all uses of
// | strings, these functions should be preferred over the ones in
// | `Data.String.CodeUnits`.
import * as $runtime from "../runtime.js";
import * as Data$dEnum from "../Data.Enum/index.js";
import * as Data$dEuclideanRing from "../Data.EuclideanRing/index.js";
import * as Data$dInt from "../Data.Int/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dString$dCodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data$dString$dCommon from "../Data.String.Common/index.js";
import * as Data$dString$dUnsafe from "../Data.String.Unsafe/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Data$dUnfoldable from "../Data.Unfoldable/index.js";
import {_codePointAt, _countPrefix, _fromCodePointArray, _singleton, _take, _toCodePointArray, _unsafeCodePointAt0} from "./foreign.js";
const showCodePoint = {show: v => "(CodePoint 0x" + Data$dString$dCommon.toUpper(Data$dInt.toStringAs(16)(v)) + ")"};
const uncons = s => {
  const v = Data$dString$dCodeUnits.length(s);
  if (v === 0) { return Data$dMaybe.Nothing; }
  if (v === 1) { return Data$dMaybe.$Maybe("Just", {head: Data$dEnum.toCharCode(Data$dString$dUnsafe.charAt(0)(s)), tail: ""}); }
  const cu1 = Data$dEnum.toCharCode(Data$dString$dUnsafe.charAt(1)(s));
  const cu0 = Data$dEnum.toCharCode(Data$dString$dUnsafe.charAt(0)(s));
  if (55296 <= cu0 && cu0 <= 56319 && 56320 <= cu1 && cu1 <= 57343) {
    return Data$dMaybe.$Maybe("Just", {head: (((cu0 - 55296 | 0) * 1024 | 0) + (cu1 - 56320 | 0) | 0) + 65536 | 0, tail: Data$dString$dCodeUnits.drop(2)(s)});
  }
  return Data$dMaybe.$Maybe("Just", {head: cu0, tail: Data$dString$dCodeUnits.drop(1)(s)});
};
const unconsButWithTuple = s => {
  const $0 = uncons(s);
  if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple($0._1.head, $0._1.tail)); }
  return Data$dMaybe.Nothing;
};
const toCodePointArrayFallback = s => Data$dUnfoldable.unfoldableArray.unfoldr(unconsButWithTuple)(s);
const unsafeCodePointAt0Fallback = s => {
  const cu0 = Data$dEnum.toCharCode(Data$dString$dUnsafe.charAt(0)(s));
  if (55296 <= cu0 && cu0 <= 56319 && Data$dString$dCodeUnits.length(s) > 1) {
    const cu1 = Data$dEnum.toCharCode(Data$dString$dUnsafe.charAt(1)(s));
    if (56320 <= cu1 && cu1 <= 57343) { return (((cu0 - 55296 | 0) * 1024 | 0) + (cu1 - 56320 | 0) | 0) + 65536 | 0; }
  }
  return cu0;
};
const unsafeCodePointAt0 = /* #__PURE__ */ _unsafeCodePointAt0(unsafeCodePointAt0Fallback);
const toCodePointArray = /* #__PURE__ */ _toCodePointArray(toCodePointArrayFallback)(unsafeCodePointAt0);
const length = x => toCodePointArray(x).length;
const lastIndexOf = p => s => {
  const $0 = Data$dString$dCodeUnits.lastIndexOf(p)(s);
  if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", toCodePointArray(Data$dString$dCodeUnits.take($0._1)(s)).length); }
  return Data$dMaybe.Nothing;
};
const indexOf = p => s => {
  const $0 = Data$dString$dCodeUnits.indexOf(p)(s);
  if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", toCodePointArray(Data$dString$dCodeUnits.take($0._1)(s)).length); }
  return Data$dMaybe.Nothing;
};
const fromCharCode = x => Data$dString$dCodeUnits.singleton((() => {
  if (x >= 0 && x <= 65535) { return Data$dEnum.fromCharCode(x); }
  if (x < 0) { return "\u0000"; }
  return "￿";
})());
const singletonFallback = v => {
  if (v <= 65535) { return fromCharCode(v); }
  return fromCharCode($runtime.intDiv(v - 65536 | 0, 1024) + 55296 | 0) + fromCharCode(Data$dEuclideanRing.intMod(v - 65536 | 0)(1024) + 56320 | 0);
};
const fromCodePointArray = /* #__PURE__ */ _fromCodePointArray(singletonFallback);
const singleton = /* #__PURE__ */ _singleton(singletonFallback);
const takeFallback = v => v1 => {
  if (v < 1) { return ""; }
  const v2 = uncons(v1);
  if (v2.tag === "Just") { return singleton(v2._1.head) + takeFallback(v - 1 | 0)(v2._1.tail); }
  return v1;
};
const take = /* #__PURE__ */ _take(takeFallback);
const lastIndexOf$p = p => i => s => {
  const $0 = Data$dString$dCodeUnits.lastIndexOf$p(p)(Data$dString$dCodeUnits.length(take(i)(s)))(s);
  if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", toCodePointArray(Data$dString$dCodeUnits.take($0._1)(s)).length); }
  return Data$dMaybe.Nothing;
};
const splitAt = i => s => {
  const before = take(i)(s);
  return {before, after: Data$dString$dCodeUnits.drop(Data$dString$dCodeUnits.length(before))(s)};
};
const eqCodePoint = {eq: x => y => x === y};
const ordCodePoint = {compare: x => y => Data$dOrd.ordInt.compare(x)(y), Eq0: () => eqCodePoint};
const drop = n => s => Data$dString$dCodeUnits.drop(Data$dString$dCodeUnits.length(take(n)(s)))(s);
const indexOf$p = p => i => s => {
  const s$p = Data$dString$dCodeUnits.drop(Data$dString$dCodeUnits.length(take(i)(s)))(s);
  const $0 = Data$dString$dCodeUnits.indexOf(p)(s$p);
  if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", i + toCodePointArray(Data$dString$dCodeUnits.take($0._1)(s$p)).length | 0); }
  return Data$dMaybe.Nothing;
};
const countTail = countTail$a0$copy => countTail$a1$copy => countTail$a2$copy => {
  let countTail$a0 = countTail$a0$copy, countTail$a1 = countTail$a1$copy, countTail$a2 = countTail$a2$copy, countTail$c = true, countTail$r;
  while (countTail$c) {
    const p = countTail$a0, s = countTail$a1, accum = countTail$a2;
    const v = uncons(s);
    if (v.tag === "Just" && p(v._1.head)) {
      countTail$a0 = p;
      countTail$a1 = v._1.tail;
      countTail$a2 = accum + 1 | 0;
      continue;
    }
    countTail$c = false;
    countTail$r = accum;
  }
  return countTail$r;
};
const countFallback = p => s => countTail(p)(s)(0);
const countPrefix = /* #__PURE__ */ _countPrefix(countFallback)(unsafeCodePointAt0);
const dropWhile = p => s => Data$dString$dCodeUnits.drop(Data$dString$dCodeUnits.length(take(countPrefix(p)(s))(s)))(s);
const takeWhile = p => s => take(countPrefix(p)(s))(s);
const codePointFromChar = x => Data$dEnum.toCharCode(x);
const codePointAtFallback = codePointAtFallback$a0$copy => codePointAtFallback$a1$copy => {
  let codePointAtFallback$a0 = codePointAtFallback$a0$copy, codePointAtFallback$a1 = codePointAtFallback$a1$copy, codePointAtFallback$c = true, codePointAtFallback$r;
  while (codePointAtFallback$c) {
    const n = codePointAtFallback$a0, s = codePointAtFallback$a1;
    const v = uncons(s);
    if (v.tag === "Just") {
      if (n === 0) {
        codePointAtFallback$c = false;
        codePointAtFallback$r = Data$dMaybe.$Maybe("Just", v._1.head);
        continue;
      }
      codePointAtFallback$a0 = n - 1 | 0;
      codePointAtFallback$a1 = v._1.tail;
      continue;
    }
    codePointAtFallback$c = false;
    codePointAtFallback$r = Data$dMaybe.Nothing;
  }
  return codePointAtFallback$r;
};
const codePointAt = v => v1 => {
  if (v < 0) { return Data$dMaybe.Nothing; }
  if (v === 0) {
    if (v1 === "") { return Data$dMaybe.Nothing; }
    return Data$dMaybe.$Maybe("Just", unsafeCodePointAt0(v1));
  }
  return _codePointAt(codePointAtFallback)(Data$dMaybe.Just)(Data$dMaybe.Nothing)(unsafeCodePointAt0)(v)(v1);
};
const boundedCodePoint = {bottom: 0, top: 1114111, Ord0: () => ordCodePoint};
const boundedEnumCodePoint = {
  cardinality: 1114112,
  fromEnum: v => v,
  toEnum: n => {
    if (n >= 0 && n <= 1114111) { return Data$dMaybe.$Maybe("Just", n); }
    return Data$dMaybe.Nothing;
  },
  Bounded0: () => boundedCodePoint,
  Enum1: () => enumCodePoint
};
const enumCodePoint = {
  succ: a => {
    const $0 = a + 1 | 0;
    if ($0 >= 0 && $0 <= 1114111) { return Data$dMaybe.$Maybe("Just", $0); }
    return Data$dMaybe.Nothing;
  },
  pred: a => {
    const $0 = a - 1 | 0;
    if ($0 >= 0 && $0 <= 1114111) { return Data$dMaybe.$Maybe("Just", $0); }
    return Data$dMaybe.Nothing;
  },
  Ord0: () => ordCodePoint
};
export {
  
  
  codePointAt,
  
  
  
  countPrefix,
  
  
  
  
  eqCodePoint,
  
  
  indexOf,
  
  
  
  
  
  
  singleton,
  
  splitAt,
  take,
  
  
  toCodePointArray,
  
  uncons,
  
  
  
};
export * from "./foreign.js";
