import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
const $DecodingHint = tag => tag;
const Sync = /* #__PURE__ */ $DecodingHint("Sync");
const Async = /* #__PURE__ */ $DecodingHint("Async");
const Auto = /* #__PURE__ */ $DecodingHint("Auto");
const showDecodingHint = {
  show: v => {
    if (v === "Sync") { return "Sync"; }
    if (v === "Async") { return "Async"; }
    if (v === "Auto") { return "Auto"; }
    $runtime.fail();
  }
};
const print = v => {
  if (v === "Sync") { return "sync"; }
  if (v === "Async") { return "async"; }
  if (v === "Auto") { return "auto"; }
  $runtime.fail();
};
const parse = v => {
  if (v === "") { return Data$dMaybe.$Maybe("Just", Auto); }
  if (v === "sync") { return Data$dMaybe.$Maybe("Just", Sync); }
  if (v === "async") { return Data$dMaybe.$Maybe("Just", Async); }
  if (v === "auto") { return Data$dMaybe.$Maybe("Just", Auto); }
  return Data$dMaybe.Nothing;
};
const eqDecodingHint = {
  eq: x => y => {
    if (x === "Sync") { return y === "Sync"; }
    if (x === "Async") { return y === "Async"; }
    return x === "Auto" && y === "Auto";
  }
};
const ordDecodingHint = {
  compare: x => y => {
    if (x === "Sync") {
      if (y === "Sync") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Sync") { return Data$dOrdering.GT; }
    if (x === "Async") {
      if (y === "Async") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Async") { return Data$dOrdering.GT; }
    if (x === "Auto" && y === "Auto") { return Data$dOrdering.EQ; }
    $runtime.fail();
  },
  Eq0: () => eqDecodingHint
};
export {$DecodingHint, Async, Auto, Sync, eqDecodingHint, ordDecodingHint, parse, print, showDecodingHint};
