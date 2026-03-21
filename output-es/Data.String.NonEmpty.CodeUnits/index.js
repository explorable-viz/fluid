import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dSemigroup from "../Data.Semigroup/index.js";
import * as Data$dSemigroup$dFoldable from "../Data.Semigroup.Foldable/index.js";
import * as Data$dString$dCodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data$dString$dUnsafe from "../Data.String.Unsafe/index.js";
const snoc = c => s => s + Data$dString$dCodeUnits.singleton(c);
const singleton = x => Data$dString$dCodeUnits.singleton(x);
const takeWhile = f => x => {
  const $0 = Data$dString$dCodeUnits.take(Data$dString$dCodeUnits.countPrefix(f)(x))(x);
  if ($0 === "") { return Data$dMaybe.Nothing; }
  return Data$dMaybe.$Maybe("Just", $0);
};
const lastIndexOf$p = pat => Data$dString$dCodeUnits.lastIndexOf$p(pat);
const lastIndexOf = x => Data$dString$dCodeUnits.lastIndexOf(x);
const indexOf$p = pat => Data$dString$dCodeUnits.indexOf$p(pat);
const indexOf = x => Data$dString$dCodeUnits.indexOf(x);
const length = x => Data$dString$dCodeUnits.length(x);
const splitAt = i => nes => {
  const v = Data$dString$dCodeUnits.splitAt(i)(nes);
  return {before: v.before === "" ? Data$dMaybe.Nothing : Data$dMaybe.$Maybe("Just", v.before), after: v.after === "" ? Data$dMaybe.Nothing : Data$dMaybe.$Maybe("Just", v.after)};
};
const take = i => nes => {
  if (i < 1) { return Data$dMaybe.Nothing; }
  return Data$dMaybe.$Maybe("Just", Data$dString$dCodeUnits.take(i)(nes));
};
const takeRight = i => nes => {
  if (i < 1) { return Data$dMaybe.Nothing; }
  return Data$dMaybe.$Maybe("Just", Data$dString$dCodeUnits.drop(Data$dString$dCodeUnits.length(nes) - i | 0)(nes));
};
const toChar = x => Data$dString$dCodeUnits.toChar(x);
const toCharArray = x => Data$dString$dCodeUnits.toCharArray(x);
const toNonEmptyCharArray = x => {
  const $0 = Data$dString$dCodeUnits.toCharArray(x);
  if ($0.length > 0) { return $0; }
  $runtime.fail();
};
const uncons = nes => (
  {
    head: Data$dString$dUnsafe.charAt(0)(nes),
    tail: (() => {
      const $0 = Data$dString$dCodeUnits.drop(1)(nes);
      if ($0 === "") { return Data$dMaybe.Nothing; }
      return Data$dMaybe.$Maybe("Just", $0);
    })()
  }
);
const fromFoldable1 = dictFoldable1 => dictFoldable1.foldMap1(Data$dSemigroup.semigroupString)(Data$dSemigroup$dFoldable.identity);
const fromCharArray = v => {
  if (v.length === 0) { return Data$dMaybe.Nothing; }
  return Data$dMaybe.$Maybe("Just", Data$dString$dCodeUnits.fromCharArray(v));
};
const fromNonEmptyCharArray = x => {
  if (x.length === 0) { $runtime.fail(); }
  return Data$dString$dCodeUnits.fromCharArray(x);
};
const dropWhile = f => x => {
  const $0 = Data$dString$dCodeUnits.drop(Data$dString$dCodeUnits.countPrefix(f)(x))(x);
  if ($0 === "") { return Data$dMaybe.Nothing; }
  return Data$dMaybe.$Maybe("Just", $0);
};
const dropRight = i => nes => {
  if (i >= Data$dString$dCodeUnits.length(nes)) { return Data$dMaybe.Nothing; }
  return Data$dMaybe.$Maybe("Just", Data$dString$dCodeUnits.take(Data$dString$dCodeUnits.length(nes) - i | 0)(nes));
};
const drop = i => nes => {
  if (i >= Data$dString$dCodeUnits.length(nes)) { return Data$dMaybe.Nothing; }
  return Data$dMaybe.$Maybe("Just", Data$dString$dCodeUnits.drop(i)(nes));
};
const countPrefix = x => Data$dString$dCodeUnits.countPrefix(x);
const cons = c => s => Data$dString$dCodeUnits.singleton(c) + s;
const charAt = x => Data$dString$dCodeUnits.charAt(x);
export {
  charAt,
  cons,
  countPrefix,
  drop,
  dropRight,
  dropWhile,
  fromCharArray,
  fromFoldable1,
  fromNonEmptyCharArray,
  indexOf,
  indexOf$p,
  lastIndexOf,
  lastIndexOf$p,
  length,
  singleton,
  snoc,
  splitAt,
  take,
  takeRight,
  takeWhile,
  toChar,
  toCharArray,
  toNonEmptyCharArray,
  uncons
};
