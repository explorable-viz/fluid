import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
const $SelectionMode = tag => tag;
const Preserve = /* #__PURE__ */ $SelectionMode("Preserve");
const Select = /* #__PURE__ */ $SelectionMode("Select");
const Start = /* #__PURE__ */ $SelectionMode("Start");
const End = /* #__PURE__ */ $SelectionMode("End");
const showSelectionMode = {
  show: v => {
    if (v === "Preserve") { return "Preserve"; }
    if (v === "Select") { return "Select"; }
    if (v === "Start") { return "Start"; }
    if (v === "End") { return "End"; }
    $runtime.fail();
  }
};
const print = v => {
  if (v === "Preserve") { return "preserve"; }
  if (v === "Select") { return "select"; }
  if (v === "Start") { return "start"; }
  if (v === "End") { return "end"; }
  $runtime.fail();
};
const parse = v => {
  if (v === "preserve") { return Data$dMaybe.$Maybe("Just", Preserve); }
  if (v === "select") { return Data$dMaybe.$Maybe("Just", Select); }
  if (v === "start") { return Data$dMaybe.$Maybe("Just", Start); }
  if (v === "end") { return Data$dMaybe.$Maybe("Just", End); }
  return Data$dMaybe.Nothing;
};
const eqSelectionMode = {
  eq: x => y => {
    if (x === "Preserve") { return y === "Preserve"; }
    if (x === "Select") { return y === "Select"; }
    if (x === "Start") { return y === "Start"; }
    return x === "End" && y === "End";
  }
};
const ordSelectionMode = {
  compare: x => y => {
    if (x === "Preserve") {
      if (y === "Preserve") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Preserve") { return Data$dOrdering.GT; }
    if (x === "Select") {
      if (y === "Select") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Select") { return Data$dOrdering.GT; }
    if (x === "Start") {
      if (y === "Start") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Start") { return Data$dOrdering.GT; }
    if (x === "End" && y === "End") { return Data$dOrdering.EQ; }
    $runtime.fail();
  },
  Eq0: () => eqSelectionMode
};
export {$SelectionMode, End, Preserve, Select, Start, eqSelectionMode, ordSelectionMode, parse, print, showSelectionMode};
