import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
const $ReadyState = tag => tag;
const None = /* #__PURE__ */ $ReadyState("None");
const Loading = /* #__PURE__ */ $ReadyState("Loading");
const Loaded = /* #__PURE__ */ $ReadyState("Loaded");
const $$Error = /* #__PURE__ */ $ReadyState("Error");
const toEnumReadyState = v => {
  if (v === 0) { return Data$dMaybe.$Maybe("Just", None); }
  if (v === 1) { return Data$dMaybe.$Maybe("Just", Loading); }
  if (v === 2) { return Data$dMaybe.$Maybe("Just", Loaded); }
  if (v === 3) { return Data$dMaybe.$Maybe("Just", $$Error); }
  return Data$dMaybe.Nothing;
};
const showReadyState = {
  show: v => {
    if (v === "None") { return "None"; }
    if (v === "Loading") { return "Loading"; }
    if (v === "Loaded") { return "Loaded"; }
    if (v === "Error") { return "Error"; }
    $runtime.fail();
  }
};
const fromEnumReadyState = v => {
  if (v === "None") { return 0; }
  if (v === "Loading") { return 1; }
  if (v === "Loaded") { return 2; }
  if (v === "Error") { return 3; }
  $runtime.fail();
};
const eqReadyState = {
  eq: x => y => {
    if (x === "None") { return y === "None"; }
    if (x === "Loading") { return y === "Loading"; }
    if (x === "Loaded") { return y === "Loaded"; }
    return x === "Error" && y === "Error";
  }
};
const ordReadyState = {
  compare: x => y => {
    if (x === "None") {
      if (y === "None") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "None") { return Data$dOrdering.GT; }
    if (x === "Loading") {
      if (y === "Loading") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Loading") { return Data$dOrdering.GT; }
    if (x === "Loaded") {
      if (y === "Loaded") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Loaded") { return Data$dOrdering.GT; }
    if (x === "Error" && y === "Error") { return Data$dOrdering.EQ; }
    $runtime.fail();
  },
  Eq0: () => eqReadyState
};
const enumReadyState = {
  succ: a => {
    const $0 = (() => {
      if (a === "None") { return 1; }
      if (a === "Loading") { return 2; }
      if (a === "Loaded") { return 3; }
      if (a === "Error") { return 4; }
      $runtime.fail();
    })();
    if ($0 === 0) { return Data$dMaybe.$Maybe("Just", None); }
    if ($0 === 1) { return Data$dMaybe.$Maybe("Just", Loading); }
    if ($0 === 2) { return Data$dMaybe.$Maybe("Just", Loaded); }
    if ($0 === 3) { return Data$dMaybe.$Maybe("Just", $$Error); }
    return Data$dMaybe.Nothing;
  },
  pred: a => {
    const $0 = (() => {
      if (a === "None") { return -1; }
      if (a === "Loading") { return 0; }
      if (a === "Loaded") { return 1; }
      if (a === "Error") { return 2; }
      $runtime.fail();
    })();
    if ($0 === 0) { return Data$dMaybe.$Maybe("Just", None); }
    if ($0 === 1) { return Data$dMaybe.$Maybe("Just", Loading); }
    if ($0 === 2) { return Data$dMaybe.$Maybe("Just", Loaded); }
    if ($0 === 3) { return Data$dMaybe.$Maybe("Just", $$Error); }
    return Data$dMaybe.Nothing;
  },
  Ord0: () => ordReadyState
};
const boundedReadyState = {bottom: None, top: $$Error, Ord0: () => ordReadyState};
const boundedEnumReadyState = {cardinality: 4, toEnum: toEnumReadyState, fromEnum: fromEnumReadyState, Bounded0: () => boundedReadyState, Enum1: () => enumReadyState};
export {
  $ReadyState,
  $$Error as Error,
  Loaded,
  Loading,
  None,
  boundedEnumReadyState,
  boundedReadyState,
  enumReadyState,
  eqReadyState,
  fromEnumReadyState,
  ordReadyState,
  showReadyState,
  toEnumReadyState
};
