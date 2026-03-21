// | While most of the code in this module is safe, this module does
// | export a few partial functions and the `NonEmptyString` constructor.
// | While the partial functions are obvious from the `Partial` constraint in
// | their type signature, the `NonEmptyString` constructor can be overlooked
// | when searching for issues in one's code. See the constructor's
// | documentation for more information.
import * as $runtime from "../runtime.js";
import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dSemigroup from "../Data.Semigroup/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dString$dCodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data$dString$dCommon from "../Data.String.Common/index.js";
const NonEmptyString = x => x;
const NonEmptyReplacement = x => x;
const toUpper = v => Data$dString$dCommon.toUpper(v);
const toString = v => v;
const toLower = v => Data$dString$dCommon.toLower(v);
const showNonEmptyString = {show: v => "(NonEmptyString.unsafeFromString " + Data$dShow.showStringImpl(v) + ")"};
const showNonEmptyReplacement = {show: v => "(NonEmptyReplacement (NonEmptyString.unsafeFromString " + Data$dShow.showStringImpl(v) + "))"};
const semigroupNonEmptyString = Data$dSemigroup.semigroupString;
const semigroupNonEmptyReplacement = Data$dSemigroup.semigroupString;
const replaceAll = pat => v => v1 => Data$dString$dCommon.replaceAll(pat)(v)(v1);
const replace = pat => v => v1 => Data$dString$dCommon.replace(pat)(v)(v1);
const prependString = s1 => v => s1 + v;
const ordNonEmptyString = Data$dOrd.ordString;
const ordNonEmptyReplacement = Data$dOrd.ordString;
const nonEmptyNonEmpty = dictIsSymbol => ({nes: p => dictIsSymbol.reflectSymbol(p)});
const nes = dict => dict.nes;
const makeNonEmptyBad = () => ({nes: v => ""});
const localeCompare = v => v1 => Data$dString$dCommon.localeCompare(v)(v1);
const liftS = f => v => f(v);
const joinWith1 = dictFoldable1 => {
  const $0 = dictFoldable1.Foldable0();
  return sep => xs => $0.foldl(v => v1 => {
    if (v.init) { return {init: false, acc: v1}; }
    return {init: false, acc: v.acc + sep + v1};
  })({init: true, acc: ""})(xs).acc;
};
const joinWith = dictFoldable => splice => xs => dictFoldable.foldl(v => v1 => {
  if (v.init) { return {init: false, acc: v1}; }
  return {init: false, acc: v.acc + splice + v1};
})({init: true, acc: ""})(xs).acc;
const join1With = dictFoldable1 => {
  const $0 = dictFoldable1.Foldable0();
  return splice => xs => $0.foldl(v => v1 => {
    if (v.init) { return {init: false, acc: v1}; }
    return {init: false, acc: v.acc + splice + v1};
  })({init: true, acc: ""})(xs).acc;
};
const fromString = v => {
  if (v === "") { return Data$dMaybe.Nothing; }
  return Data$dMaybe.$Maybe("Just", v);
};
const stripPrefix = pat => a => {
  const $0 = Data$dString$dCodeUnits.stripPrefix(pat)(a);
  if ($0.tag === "Just") {
    if ($0._1 === "") { return Data$dMaybe.Nothing; }
    return Data$dMaybe.$Maybe("Just", $0._1);
  }
  if ($0.tag === "Nothing") { return Data$dMaybe.Nothing; }
  $runtime.fail();
};
const stripSuffix = pat => a => {
  const $0 = Data$dString$dCodeUnits.stripSuffix(pat)(a);
  if ($0.tag === "Just") {
    if ($0._1 === "") { return Data$dMaybe.Nothing; }
    return Data$dMaybe.$Maybe("Just", $0._1);
  }
  if ($0.tag === "Nothing") { return Data$dMaybe.Nothing; }
  $runtime.fail();
};
const trim = v => {
  const $0 = Data$dString$dCommon.trim(v);
  if ($0 === "") { return Data$dMaybe.Nothing; }
  return Data$dMaybe.$Maybe("Just", $0);
};
const unsafeFromString = () => x => {
  if (x === "") { $runtime.fail(); }
  return x;
};
const eqNonEmptyString = Data$dEq.eqString;
const eqNonEmptyReplacement = Data$dEq.eqString;
const contains = x => Data$dString$dCodeUnits.contains(x);
const appendString = v => s2 => v + s2;
export {
  NonEmptyReplacement,
  NonEmptyString,
  appendString,
  contains,
  eqNonEmptyReplacement,
  eqNonEmptyString,
  fromString,
  join1With,
  joinWith,
  joinWith1,
  liftS,
  localeCompare,
  makeNonEmptyBad,
  nes,
  nonEmptyNonEmpty,
  ordNonEmptyReplacement,
  ordNonEmptyString,
  prependString,
  replace,
  replaceAll,
  semigroupNonEmptyReplacement,
  semigroupNonEmptyString,
  showNonEmptyReplacement,
  showNonEmptyString,
  stripPrefix,
  stripSuffix,
  toLower,
  toString,
  toUpper,
  trim,
  unsafeFromString
};
