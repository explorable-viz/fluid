import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
const $NetworkState = tag => tag;
const Empty = /* #__PURE__ */ $NetworkState("Empty");
const Idle = /* #__PURE__ */ $NetworkState("Idle");
const Loading = /* #__PURE__ */ $NetworkState("Loading");
const NoSource = /* #__PURE__ */ $NetworkState("NoSource");
const toEnumNetworkState = v => {
  if (v === 0) { return Data$dMaybe.$Maybe("Just", Empty); }
  if (v === 1) { return Data$dMaybe.$Maybe("Just", Idle); }
  if (v === 2) { return Data$dMaybe.$Maybe("Just", Loading); }
  if (v === 3) { return Data$dMaybe.$Maybe("Just", NoSource); }
  return Data$dMaybe.Nothing;
};
const showNetworkState = {
  show: v => {
    if (v === "Empty") { return "Empty"; }
    if (v === "Idle") { return "Idle"; }
    if (v === "Loading") { return "Loading"; }
    if (v === "NoSource") { return "NoSource"; }
    $runtime.fail();
  }
};
const fromEnumNetworkState = v => {
  if (v === "Empty") { return 0; }
  if (v === "Idle") { return 1; }
  if (v === "Loading") { return 2; }
  if (v === "NoSource") { return 3; }
  $runtime.fail();
};
const eqNetworkState = {
  eq: x => y => {
    if (x === "Empty") { return y === "Empty"; }
    if (x === "Idle") { return y === "Idle"; }
    if (x === "Loading") { return y === "Loading"; }
    return x === "NoSource" && y === "NoSource";
  }
};
const ordNetworkState = {
  compare: x => y => {
    if (x === "Empty") {
      if (y === "Empty") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Empty") { return Data$dOrdering.GT; }
    if (x === "Idle") {
      if (y === "Idle") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Idle") { return Data$dOrdering.GT; }
    if (x === "Loading") {
      if (y === "Loading") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Loading") { return Data$dOrdering.GT; }
    if (x === "NoSource" && y === "NoSource") { return Data$dOrdering.EQ; }
    $runtime.fail();
  },
  Eq0: () => eqNetworkState
};
const enumNetworkState = {
  succ: a => {
    const $0 = (() => {
      if (a === "Empty") { return 1; }
      if (a === "Idle") { return 2; }
      if (a === "Loading") { return 3; }
      if (a === "NoSource") { return 4; }
      $runtime.fail();
    })();
    if ($0 === 0) { return Data$dMaybe.$Maybe("Just", Empty); }
    if ($0 === 1) { return Data$dMaybe.$Maybe("Just", Idle); }
    if ($0 === 2) { return Data$dMaybe.$Maybe("Just", Loading); }
    if ($0 === 3) { return Data$dMaybe.$Maybe("Just", NoSource); }
    return Data$dMaybe.Nothing;
  },
  pred: a => {
    const $0 = (() => {
      if (a === "Empty") { return -1; }
      if (a === "Idle") { return 0; }
      if (a === "Loading") { return 1; }
      if (a === "NoSource") { return 2; }
      $runtime.fail();
    })();
    if ($0 === 0) { return Data$dMaybe.$Maybe("Just", Empty); }
    if ($0 === 1) { return Data$dMaybe.$Maybe("Just", Idle); }
    if ($0 === 2) { return Data$dMaybe.$Maybe("Just", Loading); }
    if ($0 === 3) { return Data$dMaybe.$Maybe("Just", NoSource); }
    return Data$dMaybe.Nothing;
  },
  Ord0: () => ordNetworkState
};
const boundedNetworkState = {bottom: Empty, top: NoSource, Ord0: () => ordNetworkState};
const boundedEnumNetworkState = {cardinality: 4, toEnum: toEnumNetworkState, fromEnum: fromEnumNetworkState, Bounded0: () => boundedNetworkState, Enum1: () => enumNetworkState};
export {
  $NetworkState,
  Empty,
  Idle,
  Loading,
  NoSource,
  boundedEnumNetworkState,
  boundedNetworkState,
  enumNetworkState,
  eqNetworkState,
  fromEnumNetworkState,
  ordNetworkState,
  showNetworkState,
  toEnumNetworkState
};
