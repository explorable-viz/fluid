import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
const $ReadyState = tag => tag;
const EMPTY = /* #__PURE__ */ $ReadyState("EMPTY");
const LOADING = /* #__PURE__ */ $ReadyState("LOADING");
const DONE = /* #__PURE__ */ $ReadyState("DONE");
const toEnumReadyState = v => {
  if (v === 0) { return Data$dMaybe.$Maybe("Just", EMPTY); }
  if (v === 1) { return Data$dMaybe.$Maybe("Just", LOADING); }
  if (v === 2) { return Data$dMaybe.$Maybe("Just", DONE); }
  return Data$dMaybe.Nothing;
};
const showReadyState = {
  show: v => {
    if (v === "EMPTY") { return "EMPTY"; }
    if (v === "LOADING") { return "LOADING"; }
    if (v === "DONE") { return "DONE"; }
    $runtime.fail();
  }
};
const fromEnumReadyState = v => {
  if (v === "EMPTY") { return 0; }
  if (v === "LOADING") { return 1; }
  if (v === "DONE") { return 2; }
  $runtime.fail();
};
const eqReadyState = {
  eq: x => y => {
    if (x === "EMPTY") { return y === "EMPTY"; }
    if (x === "LOADING") { return y === "LOADING"; }
    return x === "DONE" && y === "DONE";
  }
};
const ordReadyState = {
  compare: x => y => {
    if (x === "EMPTY") {
      if (y === "EMPTY") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "EMPTY") { return Data$dOrdering.GT; }
    if (x === "LOADING") {
      if (y === "LOADING") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "LOADING") { return Data$dOrdering.GT; }
    if (x === "DONE" && y === "DONE") { return Data$dOrdering.EQ; }
    $runtime.fail();
  },
  Eq0: () => eqReadyState
};
const enumReadyState = {
  succ: a => {
    const $0 = (() => {
      if (a === "EMPTY") { return 1; }
      if (a === "LOADING") { return 2; }
      if (a === "DONE") { return 3; }
      $runtime.fail();
    })();
    if ($0 === 0) { return Data$dMaybe.$Maybe("Just", EMPTY); }
    if ($0 === 1) { return Data$dMaybe.$Maybe("Just", LOADING); }
    if ($0 === 2) { return Data$dMaybe.$Maybe("Just", DONE); }
    return Data$dMaybe.Nothing;
  },
  pred: a => {
    const $0 = (() => {
      if (a === "EMPTY") { return -1; }
      if (a === "LOADING") { return 0; }
      if (a === "DONE") { return 1; }
      $runtime.fail();
    })();
    if ($0 === 0) { return Data$dMaybe.$Maybe("Just", EMPTY); }
    if ($0 === 1) { return Data$dMaybe.$Maybe("Just", LOADING); }
    if ($0 === 2) { return Data$dMaybe.$Maybe("Just", DONE); }
    return Data$dMaybe.Nothing;
  },
  Ord0: () => ordReadyState
};
const boundedReadyState = {bottom: EMPTY, top: DONE, Ord0: () => ordReadyState};
const boundedEnumReadyState = {cardinality: 3, toEnum: toEnumReadyState, fromEnum: fromEnumReadyState, Bounded0: () => boundedReadyState, Enum1: () => enumReadyState};
export {
  $ReadyState,
  DONE,
  EMPTY,
  LOADING,
  boundedEnumReadyState,
  boundedReadyState,
  enumReadyState,
  eqReadyState,
  fromEnumReadyState,
  ordReadyState,
  showReadyState,
  toEnumReadyState
};
