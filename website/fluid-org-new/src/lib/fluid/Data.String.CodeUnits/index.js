import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dString$dUnsafe from "../Data.String.Unsafe/index.js";
import {
  _charAt,
  _indexOf,
  _indexOfStartingAt,
  _lastIndexOf,
  _lastIndexOfStartingAt,
  _toChar,
  countPrefix,
  drop,
  fromCharArray,
  length,
  singleton,
  slice,
  splitAt,
  take,
  toCharArray
} from "./foreign.js";
const uncons = v => {
  if (v === "") { return Data$dMaybe.Nothing; }
  return Data$dMaybe.$Maybe("Just", {head: Data$dString$dUnsafe.charAt(0)(v), tail: drop(1)(v)});
};
const toChar = /* #__PURE__ */ _toChar(Data$dMaybe.Just)(Data$dMaybe.Nothing);
const takeWhile = p => s => take(countPrefix(p)(s))(s);
const takeRight = i => s => drop(length(s) - i | 0)(s);
const stripSuffix = v => str => {
  const v1 = splitAt(length(str) - length(v) | 0)(str);
  if (v1.after === v) { return Data$dMaybe.$Maybe("Just", v1.before); }
  return Data$dMaybe.Nothing;
};
const stripPrefix = v => str => {
  const v1 = splitAt(length(v))(str);
  if (v1.before === v) { return Data$dMaybe.$Maybe("Just", v1.after); }
  return Data$dMaybe.Nothing;
};
const lastIndexOf$p = /* #__PURE__ */ _lastIndexOfStartingAt(Data$dMaybe.Just)(Data$dMaybe.Nothing);
const lastIndexOf = /* #__PURE__ */ _lastIndexOf(Data$dMaybe.Just)(Data$dMaybe.Nothing);
const indexOf$p = /* #__PURE__ */ _indexOfStartingAt(Data$dMaybe.Just)(Data$dMaybe.Nothing);
const indexOf = /* #__PURE__ */ _indexOf(Data$dMaybe.Just)(Data$dMaybe.Nothing);
const dropWhile = p => s => drop(countPrefix(p)(s))(s);
const dropRight = i => s => take(length(s) - i | 0)(s);
const contains = pat => {
  const $0 = indexOf(pat);
  return x => {
    const $1 = $0(x);
    if ($1.tag === "Nothing") { return false; }
    if ($1.tag === "Just") { return true; }
    $runtime.fail();
  };
};
const charAt = /* #__PURE__ */ _charAt(Data$dMaybe.Just)(Data$dMaybe.Nothing);
export {charAt, contains,   indexOf,  lastIndexOf, lastIndexOf$p, stripPrefix,    toChar, };
export * from "./foreign.js";
