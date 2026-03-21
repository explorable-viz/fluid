import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
const $ReadyState = tag => tag;
const HaveNothing = /* #__PURE__ */ $ReadyState("HaveNothing");
const HaveMetadata = /* #__PURE__ */ $ReadyState("HaveMetadata");
const HaveCurrentData = /* #__PURE__ */ $ReadyState("HaveCurrentData");
const HaveFutureData = /* #__PURE__ */ $ReadyState("HaveFutureData");
const HaveEnoughData = /* #__PURE__ */ $ReadyState("HaveEnoughData");
const toEnumReadyState = v => {
  if (v === 0) { return Data$dMaybe.$Maybe("Just", HaveNothing); }
  if (v === 1) { return Data$dMaybe.$Maybe("Just", HaveMetadata); }
  if (v === 2) { return Data$dMaybe.$Maybe("Just", HaveCurrentData); }
  if (v === 3) { return Data$dMaybe.$Maybe("Just", HaveFutureData); }
  if (v === 4) { return Data$dMaybe.$Maybe("Just", HaveEnoughData); }
  return Data$dMaybe.Nothing;
};
const showReadyState = {
  show: v => {
    if (v === "HaveNothing") { return "HaveNothing"; }
    if (v === "HaveMetadata") { return "HaveMetadata"; }
    if (v === "HaveCurrentData") { return "HaveCurrentData"; }
    if (v === "HaveFutureData") { return "HaveFutureData"; }
    if (v === "HaveEnoughData") { return "HaveEnoughData"; }
    $runtime.fail();
  }
};
const fromEnumReadyState = v => {
  if (v === "HaveNothing") { return 0; }
  if (v === "HaveMetadata") { return 1; }
  if (v === "HaveCurrentData") { return 2; }
  if (v === "HaveFutureData") { return 3; }
  if (v === "HaveEnoughData") { return 4; }
  $runtime.fail();
};
const eqReadyState = {
  eq: x => y => {
    if (x === "HaveNothing") { return y === "HaveNothing"; }
    if (x === "HaveMetadata") { return y === "HaveMetadata"; }
    if (x === "HaveCurrentData") { return y === "HaveCurrentData"; }
    if (x === "HaveFutureData") { return y === "HaveFutureData"; }
    return x === "HaveEnoughData" && y === "HaveEnoughData";
  }
};
const ordReadyState = {
  compare: x => y => {
    if (x === "HaveNothing") {
      if (y === "HaveNothing") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "HaveNothing") { return Data$dOrdering.GT; }
    if (x === "HaveMetadata") {
      if (y === "HaveMetadata") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "HaveMetadata") { return Data$dOrdering.GT; }
    if (x === "HaveCurrentData") {
      if (y === "HaveCurrentData") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "HaveCurrentData") { return Data$dOrdering.GT; }
    if (x === "HaveFutureData") {
      if (y === "HaveFutureData") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "HaveFutureData") { return Data$dOrdering.GT; }
    if (x === "HaveEnoughData" && y === "HaveEnoughData") { return Data$dOrdering.EQ; }
    $runtime.fail();
  },
  Eq0: () => eqReadyState
};
const enumReadyState = {
  succ: a => {
    const $0 = (() => {
      if (a === "HaveNothing") { return 1; }
      if (a === "HaveMetadata") { return 2; }
      if (a === "HaveCurrentData") { return 3; }
      if (a === "HaveFutureData") { return 4; }
      if (a === "HaveEnoughData") { return 5; }
      $runtime.fail();
    })();
    if ($0 === 0) { return Data$dMaybe.$Maybe("Just", HaveNothing); }
    if ($0 === 1) { return Data$dMaybe.$Maybe("Just", HaveMetadata); }
    if ($0 === 2) { return Data$dMaybe.$Maybe("Just", HaveCurrentData); }
    if ($0 === 3) { return Data$dMaybe.$Maybe("Just", HaveFutureData); }
    if ($0 === 4) { return Data$dMaybe.$Maybe("Just", HaveEnoughData); }
    return Data$dMaybe.Nothing;
  },
  pred: a => {
    const $0 = (() => {
      if (a === "HaveNothing") { return -1; }
      if (a === "HaveMetadata") { return 0; }
      if (a === "HaveCurrentData") { return 1; }
      if (a === "HaveFutureData") { return 2; }
      if (a === "HaveEnoughData") { return 3; }
      $runtime.fail();
    })();
    if ($0 === 0) { return Data$dMaybe.$Maybe("Just", HaveNothing); }
    if ($0 === 1) { return Data$dMaybe.$Maybe("Just", HaveMetadata); }
    if ($0 === 2) { return Data$dMaybe.$Maybe("Just", HaveCurrentData); }
    if ($0 === 3) { return Data$dMaybe.$Maybe("Just", HaveFutureData); }
    if ($0 === 4) { return Data$dMaybe.$Maybe("Just", HaveEnoughData); }
    return Data$dMaybe.Nothing;
  },
  Ord0: () => ordReadyState
};
const boundedReadyState = {bottom: HaveNothing, top: HaveEnoughData, Ord0: () => ordReadyState};
const boundedEnumReadyState = {cardinality: 5, toEnum: toEnumReadyState, fromEnum: fromEnumReadyState, Bounded0: () => boundedReadyState, Enum1: () => enumReadyState};
export {
  $ReadyState,
  HaveCurrentData,
  HaveEnoughData,
  HaveFutureData,
  HaveMetadata,
  HaveNothing,
  boundedEnumReadyState,
  boundedReadyState,
  enumReadyState,
  eqReadyState,
  fromEnumReadyState,
  ordReadyState,
  showReadyState,
  toEnumReadyState
};
