import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
const $EventPhase = tag => tag;
const None = /* #__PURE__ */ $EventPhase("None");
const Capturing = /* #__PURE__ */ $EventPhase("Capturing");
const AtTarget = /* #__PURE__ */ $EventPhase("AtTarget");
const Bubbling = /* #__PURE__ */ $EventPhase("Bubbling");
const toEnumEventPhase = v => {
  if (v === 0) { return Data$dMaybe.$Maybe("Just", None); }
  if (v === 1) { return Data$dMaybe.$Maybe("Just", Capturing); }
  if (v === 2) { return Data$dMaybe.$Maybe("Just", AtTarget); }
  if (v === 3) { return Data$dMaybe.$Maybe("Just", Bubbling); }
  return Data$dMaybe.Nothing;
};
const fromEnumEventPhase = v => {
  if (v === "None") { return 0; }
  if (v === "Capturing") { return 1; }
  if (v === "AtTarget") { return 2; }
  if (v === "Bubbling") { return 3; }
  $runtime.fail();
};
const eqEventPhase = {
  eq: x => y => {
    if (x === "None") { return y === "None"; }
    if (x === "Capturing") { return y === "Capturing"; }
    if (x === "AtTarget") { return y === "AtTarget"; }
    return x === "Bubbling" && y === "Bubbling";
  }
};
const ordEventPhase = {
  compare: x => y => {
    if (x === "None") {
      if (y === "None") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "None") { return Data$dOrdering.GT; }
    if (x === "Capturing") {
      if (y === "Capturing") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Capturing") { return Data$dOrdering.GT; }
    if (x === "AtTarget") {
      if (y === "AtTarget") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "AtTarget") { return Data$dOrdering.GT; }
    if (x === "Bubbling" && y === "Bubbling") { return Data$dOrdering.EQ; }
    $runtime.fail();
  },
  Eq0: () => eqEventPhase
};
const enumEventPhase = {
  succ: a => {
    const $0 = (() => {
      if (a === "None") { return 1; }
      if (a === "Capturing") { return 2; }
      if (a === "AtTarget") { return 3; }
      if (a === "Bubbling") { return 4; }
      $runtime.fail();
    })();
    if ($0 === 0) { return Data$dMaybe.$Maybe("Just", None); }
    if ($0 === 1) { return Data$dMaybe.$Maybe("Just", Capturing); }
    if ($0 === 2) { return Data$dMaybe.$Maybe("Just", AtTarget); }
    if ($0 === 3) { return Data$dMaybe.$Maybe("Just", Bubbling); }
    return Data$dMaybe.Nothing;
  },
  pred: a => {
    const $0 = (() => {
      if (a === "None") { return -1; }
      if (a === "Capturing") { return 0; }
      if (a === "AtTarget") { return 1; }
      if (a === "Bubbling") { return 2; }
      $runtime.fail();
    })();
    if ($0 === 0) { return Data$dMaybe.$Maybe("Just", None); }
    if ($0 === 1) { return Data$dMaybe.$Maybe("Just", Capturing); }
    if ($0 === 2) { return Data$dMaybe.$Maybe("Just", AtTarget); }
    if ($0 === 3) { return Data$dMaybe.$Maybe("Just", Bubbling); }
    return Data$dMaybe.Nothing;
  },
  Ord0: () => ordEventPhase
};
const boundedEventPhase = {bottom: None, top: Bubbling, Ord0: () => ordEventPhase};
const boundedEnumEventPhase = {cardinality: 4, toEnum: toEnumEventPhase, fromEnum: fromEnumEventPhase, Bounded0: () => boundedEventPhase, Enum1: () => enumEventPhase};
export {
  
  AtTarget,
  Bubbling,
  Capturing,
  None,
  
  
  
  
  
  
  
};
