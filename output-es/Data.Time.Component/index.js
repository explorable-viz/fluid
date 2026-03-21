import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dShow from "../Data.Show/index.js";
const showSecond = {show: v => "(Second " + Data$dShow.showIntImpl(v) + ")"};
const showMinute = {show: v => "(Minute " + Data$dShow.showIntImpl(v) + ")"};
const showMillisecond = {show: v => "(Millisecond " + Data$dShow.showIntImpl(v) + ")"};
const showHour = {show: v => "(Hour " + Data$dShow.showIntImpl(v) + ")"};
const ordSecond = Data$dOrd.ordInt;
const ordMinute = Data$dOrd.ordInt;
const ordMillisecond = Data$dOrd.ordInt;
const ordHour = Data$dOrd.ordInt;
const eqSecond = Data$dEq.eqInt;
const eqMinute = Data$dEq.eqInt;
const eqMillisecond = Data$dEq.eqInt;
const eqHour = Data$dEq.eqInt;
const boundedSecond = {bottom: 0, top: 59, Ord0: () => Data$dOrd.ordInt};
const boundedMinute = {bottom: 0, top: 59, Ord0: () => Data$dOrd.ordInt};
const boundedMillisecond = {bottom: 0, top: 999, Ord0: () => Data$dOrd.ordInt};
const boundedHour = {bottom: 0, top: 23, Ord0: () => Data$dOrd.ordInt};
const boundedEnumSecond = {
  cardinality: 60,
  toEnum: n => {
    if (n >= 0 && n <= 59) { return Data$dMaybe.$Maybe("Just", n); }
    return Data$dMaybe.Nothing;
  },
  fromEnum: v => v,
  Bounded0: () => boundedSecond,
  Enum1: () => enumSecond
};
const enumSecond = {
  succ: x => {
    const $0 = x + 1 | 0;
    if ($0 >= 0 && $0 <= 59) { return Data$dMaybe.$Maybe("Just", $0); }
    return Data$dMaybe.Nothing;
  },
  pred: x => {
    const $0 = x - 1 | 0;
    if ($0 >= 0 && $0 <= 59) { return Data$dMaybe.$Maybe("Just", $0); }
    return Data$dMaybe.Nothing;
  },
  Ord0: () => Data$dOrd.ordInt
};
const boundedEnumMinute = {
  cardinality: 60,
  toEnum: n => {
    if (n >= 0 && n <= 59) { return Data$dMaybe.$Maybe("Just", n); }
    return Data$dMaybe.Nothing;
  },
  fromEnum: v => v,
  Bounded0: () => boundedMinute,
  Enum1: () => enumMinute
};
const enumMinute = {
  succ: x => {
    const $0 = x + 1 | 0;
    if ($0 >= 0 && $0 <= 59) { return Data$dMaybe.$Maybe("Just", $0); }
    return Data$dMaybe.Nothing;
  },
  pred: x => {
    const $0 = x - 1 | 0;
    if ($0 >= 0 && $0 <= 59) { return Data$dMaybe.$Maybe("Just", $0); }
    return Data$dMaybe.Nothing;
  },
  Ord0: () => Data$dOrd.ordInt
};
const boundedEnumMillisecond = {
  cardinality: 1000,
  toEnum: n => {
    if (n >= 0 && n <= 999) { return Data$dMaybe.$Maybe("Just", n); }
    return Data$dMaybe.Nothing;
  },
  fromEnum: v => v,
  Bounded0: () => boundedMillisecond,
  Enum1: () => enumMillisecond
};
const enumMillisecond = {
  succ: x => {
    const $0 = x + 1 | 0;
    if ($0 >= 0 && $0 <= 999) { return Data$dMaybe.$Maybe("Just", $0); }
    return Data$dMaybe.Nothing;
  },
  pred: x => {
    const $0 = x - 1 | 0;
    if ($0 >= 0 && $0 <= 999) { return Data$dMaybe.$Maybe("Just", $0); }
    return Data$dMaybe.Nothing;
  },
  Ord0: () => Data$dOrd.ordInt
};
const boundedEnumHour = {
  cardinality: 24,
  toEnum: n => {
    if (n >= 0 && n <= 23) { return Data$dMaybe.$Maybe("Just", n); }
    return Data$dMaybe.Nothing;
  },
  fromEnum: v => v,
  Bounded0: () => boundedHour,
  Enum1: () => enumHour
};
const enumHour = {
  succ: x => {
    const $0 = x + 1 | 0;
    if ($0 >= 0 && $0 <= 23) { return Data$dMaybe.$Maybe("Just", $0); }
    return Data$dMaybe.Nothing;
  },
  pred: x => {
    const $0 = x - 1 | 0;
    if ($0 >= 0 && $0 <= 23) { return Data$dMaybe.$Maybe("Just", $0); }
    return Data$dMaybe.Nothing;
  },
  Ord0: () => Data$dOrd.ordInt
};
export {
  boundedEnumHour,
  boundedEnumMillisecond,
  boundedEnumMinute,
  boundedEnumSecond,
  boundedHour,
  boundedMillisecond,
  boundedMinute,
  boundedSecond,
  enumHour,
  enumMillisecond,
  enumMinute,
  enumSecond,
  eqHour,
  eqMillisecond,
  eqMinute,
  eqSecond,
  ordHour,
  ordMillisecond,
  ordMinute,
  ordSecond,
  showHour,
  showMillisecond,
  showMinute,
  showSecond
};
