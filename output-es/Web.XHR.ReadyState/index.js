import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
const $ReadyState = tag => tag;
const Unsent = /* #__PURE__ */ $ReadyState("Unsent");
const Opened = /* #__PURE__ */ $ReadyState("Opened");
const HeadersReceived = /* #__PURE__ */ $ReadyState("HeadersReceived");
const Loading = /* #__PURE__ */ $ReadyState("Loading");
const Done = /* #__PURE__ */ $ReadyState("Done");
const toEnum = v => {
  if (v === 0) { return Data$dMaybe.$Maybe("Just", Unsent); }
  if (v === 1) { return Data$dMaybe.$Maybe("Just", Opened); }
  if (v === 2) { return Data$dMaybe.$Maybe("Just", HeadersReceived); }
  if (v === 3) { return Data$dMaybe.$Maybe("Just", Loading); }
  if (v === 4) { return Data$dMaybe.$Maybe("Just", Done); }
  return Data$dMaybe.Nothing;
};
const fromEnum = v => {
  if (v === "Unsent") { return 0; }
  if (v === "Opened") { return 1; }
  if (v === "HeadersReceived") { return 2; }
  if (v === "Loading") { return 3; }
  if (v === "Done") { return 4; }
  $runtime.fail();
};
const eqReadyState = {
  eq: x => y => {
    if (x === "Unsent") { return y === "Unsent"; }
    if (x === "Opened") { return y === "Opened"; }
    if (x === "HeadersReceived") { return y === "HeadersReceived"; }
    if (x === "Loading") { return y === "Loading"; }
    return x === "Done" && y === "Done";
  }
};
const ordReadyState = {
  compare: x => y => {
    if (x === "Unsent") {
      if (y === "Unsent") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Unsent") { return Data$dOrdering.GT; }
    if (x === "Opened") {
      if (y === "Opened") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Opened") { return Data$dOrdering.GT; }
    if (x === "HeadersReceived") {
      if (y === "HeadersReceived") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "HeadersReceived") { return Data$dOrdering.GT; }
    if (x === "Loading") {
      if (y === "Loading") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Loading") { return Data$dOrdering.GT; }
    if (x === "Done" && y === "Done") { return Data$dOrdering.EQ; }
    $runtime.fail();
  },
  Eq0: () => eqReadyState
};
const enumReadyState = {
  succ: a => {
    const $0 = (() => {
      if (a === "Unsent") { return 1; }
      if (a === "Opened") { return 2; }
      if (a === "HeadersReceived") { return 3; }
      if (a === "Loading") { return 4; }
      if (a === "Done") { return 5; }
      $runtime.fail();
    })();
    if ($0 === 0) { return Data$dMaybe.$Maybe("Just", Unsent); }
    if ($0 === 1) { return Data$dMaybe.$Maybe("Just", Opened); }
    if ($0 === 2) { return Data$dMaybe.$Maybe("Just", HeadersReceived); }
    if ($0 === 3) { return Data$dMaybe.$Maybe("Just", Loading); }
    if ($0 === 4) { return Data$dMaybe.$Maybe("Just", Done); }
    return Data$dMaybe.Nothing;
  },
  pred: a => {
    const $0 = (() => {
      if (a === "Unsent") { return -1; }
      if (a === "Opened") { return 0; }
      if (a === "HeadersReceived") { return 1; }
      if (a === "Loading") { return 2; }
      if (a === "Done") { return 3; }
      $runtime.fail();
    })();
    if ($0 === 0) { return Data$dMaybe.$Maybe("Just", Unsent); }
    if ($0 === 1) { return Data$dMaybe.$Maybe("Just", Opened); }
    if ($0 === 2) { return Data$dMaybe.$Maybe("Just", HeadersReceived); }
    if ($0 === 3) { return Data$dMaybe.$Maybe("Just", Loading); }
    if ($0 === 4) { return Data$dMaybe.$Maybe("Just", Done); }
    return Data$dMaybe.Nothing;
  },
  Ord0: () => ordReadyState
};
const boundedReadyState = {bottom: Unsent, top: Done, Ord0: () => ordReadyState};
const boundedEnumReadyState = {cardinality: 5, toEnum, fromEnum, Bounded0: () => boundedReadyState, Enum1: () => enumReadyState};
export {$ReadyState, Done, HeadersReceived, Loading, Opened, Unsent, boundedEnumReadyState, boundedReadyState, enumReadyState, eqReadyState, fromEnum, ordReadyState, toEnum};
