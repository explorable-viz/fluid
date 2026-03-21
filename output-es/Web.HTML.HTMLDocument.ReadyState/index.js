import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
const $ReadyState = tag => tag;
const Loading = /* #__PURE__ */ $ReadyState("Loading");
const Interactive = /* #__PURE__ */ $ReadyState("Interactive");
const Complete = /* #__PURE__ */ $ReadyState("Complete");
const showReadyState = {
  show: v => {
    if (v === "Loading") { return "Loading"; }
    if (v === "Interactive") { return "Interactive"; }
    if (v === "Complete") { return "Complete"; }
    $runtime.fail();
  }
};
const print = v => {
  if (v === "Loading") { return "loading"; }
  if (v === "Interactive") { return "interactive"; }
  if (v === "Complete") { return "complete"; }
  $runtime.fail();
};
const parse = v => {
  if (v === "loading") { return Data$dMaybe.$Maybe("Just", Loading); }
  if (v === "interactive") { return Data$dMaybe.$Maybe("Just", Interactive); }
  if (v === "complete") { return Data$dMaybe.$Maybe("Just", Complete); }
  return Data$dMaybe.Nothing;
};
const eqReadyState = {
  eq: x => y => {
    if (x === "Loading") { return y === "Loading"; }
    if (x === "Interactive") { return y === "Interactive"; }
    return x === "Complete" && y === "Complete";
  }
};
const ordReadyState = {
  compare: x => y => {
    if (x === "Loading") {
      if (y === "Loading") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Loading") { return Data$dOrdering.GT; }
    if (x === "Interactive") {
      if (y === "Interactive") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Interactive") { return Data$dOrdering.GT; }
    if (x === "Complete" && y === "Complete") { return Data$dOrdering.EQ; }
    $runtime.fail();
  },
  Eq0: () => eqReadyState
};
export {$ReadyState, Complete, Interactive, Loading, eqReadyState, ordReadyState, parse, print, showReadyState};
