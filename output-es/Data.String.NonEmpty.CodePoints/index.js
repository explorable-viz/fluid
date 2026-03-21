import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dSemigroup from "../Data.Semigroup/index.js";
import * as Data$dString$dCodePoints from "../Data.String.CodePoints/index.js";
import * as Data$dString$dCodeUnits from "../Data.String.CodeUnits/index.js";
const snoc = c => s => s + Data$dString$dCodePoints.singleton(c);
const singleton = x => Data$dString$dCodePoints.singleton(x);
const takeWhile = f => x => {
  const $0 = Data$dString$dCodePoints.take(Data$dString$dCodePoints.countPrefix(f)(x))(x);
  if ($0 === "") { return Data$dMaybe.Nothing; }
  return Data$dMaybe.$Maybe("Just", $0);
};
const lastIndexOf$p = pat => x => v => Data$dString$dCodePoints.lastIndexOf$p(pat)(x)(v);
const lastIndexOf = x => v => Data$dString$dCodePoints.lastIndexOf(x)(v);
const indexOf$p = pat => x => v => Data$dString$dCodePoints.indexOf$p(pat)(x)(v);
const indexOf = x => v => Data$dString$dCodePoints.indexOf(x)(v);
const length = x => Data$dString$dCodePoints.toCodePointArray(x).length;
const splitAt = i => nes => {
  const v = Data$dString$dCodePoints.splitAt(i)(nes);
  return {before: v.before === "" ? Data$dMaybe.Nothing : Data$dMaybe.$Maybe("Just", v.before), after: v.after === "" ? Data$dMaybe.Nothing : Data$dMaybe.$Maybe("Just", v.after)};
};
const take = i => nes => {
  if (i < 1) { return Data$dMaybe.Nothing; }
  return Data$dMaybe.$Maybe("Just", Data$dString$dCodePoints.take(i)(nes));
};
const toCodePointArray = x => Data$dString$dCodePoints.toCodePointArray(x);
const toNonEmptyCodePointArray = x => {
  const $0 = Data$dString$dCodePoints.toCodePointArray(x);
  if ($0.length > 0) { return $0; }
  $runtime.fail();
};
const uncons = nes => (
  {
    head: (() => {
      const $0 = Data$dString$dCodePoints.codePointAt(0)(nes);
      if ($0.tag === "Just") { return $0._1; }
      $runtime.fail();
    })(),
    tail: (() => {
      const $0 = Data$dString$dCodeUnits.drop(Data$dString$dCodeUnits.length(Data$dString$dCodePoints.take(1)(nes)))(nes);
      if ($0 === "") { return Data$dMaybe.Nothing; }
      return Data$dMaybe.$Maybe("Just", $0);
    })()
  }
);
const fromFoldable1 = dictFoldable1 => dictFoldable1.foldMap1(Data$dSemigroup.semigroupString)(singleton);
const fromCodePointArray = v => {
  if (v.length === 0) { return Data$dMaybe.Nothing; }
  return Data$dMaybe.$Maybe("Just", Data$dString$dCodePoints.fromCodePointArray(v));
};
const fromNonEmptyCodePointArray = x => {
  if (x.length === 0) { $runtime.fail(); }
  return Data$dString$dCodePoints.fromCodePointArray(x);
};
const dropWhile = f => x => {
  const $0 = Data$dString$dCodePoints.dropWhile(f)(x);
  if ($0 === "") { return Data$dMaybe.Nothing; }
  return Data$dMaybe.$Maybe("Just", $0);
};
const drop = i => nes => {
  if (i >= Data$dString$dCodePoints.toCodePointArray(nes).length) { return Data$dMaybe.Nothing; }
  return Data$dMaybe.$Maybe("Just", Data$dString$dCodeUnits.drop(Data$dString$dCodeUnits.length(Data$dString$dCodePoints.take(i)(nes)))(nes));
};
const countPrefix = x => Data$dString$dCodePoints.countPrefix(x);
const cons = c => s => Data$dString$dCodePoints.singleton(c) + s;
const codePointAt = x => v => Data$dString$dCodePoints.codePointAt(x)(v);
export {
  codePointAt,
  cons,
  countPrefix,
  drop,
  dropWhile,
  fromCodePointArray,
  fromFoldable1,
  fromNonEmptyCodePointArray,
  indexOf,
  indexOf$p,
  lastIndexOf,
  lastIndexOf$p,
  length,
  singleton,
  snoc,
  splitAt,
  take,
  takeWhile,
  toCodePointArray,
  toNonEmptyCodePointArray,
  uncons
};
