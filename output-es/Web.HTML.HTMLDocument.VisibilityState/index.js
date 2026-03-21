import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
const $VisibilityState = tag => tag;
const Visible = /* #__PURE__ */ $VisibilityState("Visible");
const Hidden = /* #__PURE__ */ $VisibilityState("Hidden");
const showVisibilityState = {
  show: v => {
    if (v === "Visible") { return "Visible"; }
    if (v === "Hidden") { return "Hidden"; }
    $runtime.fail();
  }
};
const print = v => {
  if (v === "Visible") { return "visible"; }
  if (v === "Hidden") { return "hidden"; }
  $runtime.fail();
};
const parse = v => {
  if (v === "visible") { return Data$dMaybe.$Maybe("Just", Visible); }
  if (v === "hidden") { return Data$dMaybe.$Maybe("Just", Hidden); }
  return Data$dMaybe.Nothing;
};
const eqVisibilityState = {
  eq: x => y => {
    if (x === "Visible") { return y === "Visible"; }
    return x === "Hidden" && y === "Hidden";
  }
};
const ordVisibilityState = {
  compare: x => y => {
    if (x === "Visible") {
      if (y === "Visible") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Visible") { return Data$dOrdering.GT; }
    if (x === "Hidden" && y === "Hidden") { return Data$dOrdering.EQ; }
    $runtime.fail();
  },
  Eq0: () => eqVisibilityState
};
export {$VisibilityState, Hidden, Visible, eqVisibilityState, ordVisibilityState, parse, print, showVisibilityState};
