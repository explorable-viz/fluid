import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
const $CanPlayType = tag => tag;
const Unspecified = /* #__PURE__ */ $CanPlayType("Unspecified");
const Maybe = /* #__PURE__ */ $CanPlayType("Maybe");
const Probably = /* #__PURE__ */ $CanPlayType("Probably");
const showCanPlayType = {
  show: v => {
    if (v === "Unspecified") { return "Unspecified"; }
    if (v === "Maybe") { return "Maybe"; }
    if (v === "Probably") { return "Probably"; }
    $runtime.fail();
  }
};
const print = v => {
  if (v === "Unspecified") { return ""; }
  if (v === "Maybe") { return "maybe"; }
  if (v === "Probably") { return "probably"; }
  $runtime.fail();
};
const parse = v => {
  if (v === "") { return Data$dMaybe.$Maybe("Just", Unspecified); }
  if (v === "maybe") { return Data$dMaybe.$Maybe("Just", Maybe); }
  if (v === "probably") { return Data$dMaybe.$Maybe("Just", Probably); }
  return Data$dMaybe.Nothing;
};
const eqCanPlayType = {
  eq: x => y => {
    if (x === "Unspecified") { return y === "Unspecified"; }
    if (x === "Maybe") { return y === "Maybe"; }
    return x === "Probably" && y === "Probably";
  }
};
const ordCanPlayType = {
  compare: x => y => {
    if (x === "Unspecified") {
      if (y === "Unspecified") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Unspecified") { return Data$dOrdering.GT; }
    if (x === "Maybe") {
      if (y === "Maybe") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Maybe") { return Data$dOrdering.GT; }
    if (x === "Probably" && y === "Probably") { return Data$dOrdering.EQ; }
    $runtime.fail();
  },
  Eq0: () => eqCanPlayType
};
export {$CanPlayType, Maybe, Probably, Unspecified, eqCanPlayType, ordCanPlayType, parse, print, showCanPlayType};
