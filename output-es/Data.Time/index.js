import * as $runtime from "../runtime.js";
import * as Data$dInt from "../Data.Int/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNumber from "../Data.Number/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const $Time = (_1, _2, _3, _4) => ({tag: "Time", _1, _2, _3, _4});
const Time = value0 => value1 => value2 => value3 => $Time(value0, value1, value2, value3);
const showTime = {
  show: v => "(Time (Hour " + Data$dShow.showIntImpl(v._1) + ") (Minute " + Data$dShow.showIntImpl(v._2) + ") (Second " + Data$dShow.showIntImpl(v._3) + ") (Millisecond " + Data$dShow.showIntImpl(v._4) + "))"
};
const setSecond = s => v => $Time(v._1, v._2, s, v._4);
const setMinute = m => v => $Time(v._1, m, v._3, v._4);
const setMillisecond = ms => v => $Time(v._1, v._2, v._3, ms);
const setHour = h => v => $Time(h, v._2, v._3, v._4);
const second = v => v._3;
const minute = v => v._2;
const millisecond = v => v._4;
const millisToTime = v => {
  const hours = Data$dNumber.floor(v / 3600000.0);
  const minutes = Data$dNumber.floor((v - hours * 3600000.0) / 60000.0);
  const seconds = Data$dNumber.floor((v - (hours * 3600000.0 + minutes * 60000.0)) / 1000.0);
  const $0 = Data$dInt.unsafeClamp(Data$dNumber.floor(hours));
  const $1 = $0 >= 0 && $0 <= 23 ? Data$dMaybe.$Maybe("Just", $0) : Data$dMaybe.Nothing;
  if ($1.tag === "Just") {
    const $2 = Data$dInt.unsafeClamp(Data$dNumber.floor(minutes));
    if ($2 >= 0 && $2 <= 59) {
      const $3 = Data$dInt.unsafeClamp(Data$dNumber.floor(seconds));
      if ($3 >= 0 && $3 <= 59) {
        const $4 = Data$dInt.unsafeClamp(Data$dNumber.floor(v - (hours * 3600000.0 + minutes * 60000.0 + seconds * 1000.0)));
        if ($4 >= 0 && $4 <= 999) { return $Time($1._1, $2, $3, $4); }
        $runtime.fail();
      }
      const $4 = Data$dInt.unsafeClamp(Data$dNumber.floor(v - (hours * 3600000.0 + minutes * 60000.0 + seconds * 1000.0)));
      $runtime.fail();
    }
    const $3 = Data$dInt.unsafeClamp(Data$dNumber.floor(seconds));
    if ($3 >= 0 && $3 <= 59) {
      const $4 = Data$dInt.unsafeClamp(Data$dNumber.floor(v - (hours * 3600000.0 + minutes * 60000.0 + seconds * 1000.0)));
      $runtime.fail();
    }
    const $4 = Data$dInt.unsafeClamp(Data$dNumber.floor(v - (hours * 3600000.0 + minutes * 60000.0 + seconds * 1000.0)));
    $runtime.fail();
  }
  const $2 = Data$dInt.unsafeClamp(Data$dNumber.floor(minutes));
  if ($2 >= 0 && $2 <= 59) {
    const $3 = Data$dInt.unsafeClamp(Data$dNumber.floor(seconds));
    if ($3 >= 0 && $3 <= 59) {
      const $4 = Data$dInt.unsafeClamp(Data$dNumber.floor(v - (hours * 3600000.0 + minutes * 60000.0 + seconds * 1000.0)));
      $runtime.fail();
    }
    const $4 = Data$dInt.unsafeClamp(Data$dNumber.floor(v - (hours * 3600000.0 + minutes * 60000.0 + seconds * 1000.0)));
    $runtime.fail();
  }
  const $3 = Data$dInt.unsafeClamp(Data$dNumber.floor(seconds));
  if ($3 >= 0 && $3 <= 59) {
    const $4 = Data$dInt.unsafeClamp(Data$dNumber.floor(v - (hours * 3600000.0 + minutes * 60000.0 + seconds * 1000.0)));
    $runtime.fail();
  }
  const $4 = Data$dInt.unsafeClamp(Data$dNumber.floor(v - (hours * 3600000.0 + minutes * 60000.0 + seconds * 1000.0)));
  $runtime.fail();
};
const hour = v => v._1;
const timeToMillis = t => 3600000.0 * Data$dInt.toNumber(t._1) + 60000.0 * Data$dInt.toNumber(t._2) + 1000.0 * Data$dInt.toNumber(t._3) + Data$dInt.toNumber(t._4);
const eqTime = {eq: x => y => x._1 === y._1 && x._2 === y._2 && x._3 === y._3 && x._4 === y._4};
const ordTime = {
  compare: x => y => {
    const v = Data$dOrd.ordInt.compare(x._1)(y._1);
    if (v === "LT") { return Data$dOrdering.LT; }
    if (v === "GT") { return Data$dOrdering.GT; }
    const v1 = Data$dOrd.ordInt.compare(x._2)(y._2);
    if (v1 === "LT") { return Data$dOrdering.LT; }
    if (v1 === "GT") { return Data$dOrdering.GT; }
    const v2 = Data$dOrd.ordInt.compare(x._3)(y._3);
    if (v2 === "LT") { return Data$dOrdering.LT; }
    if (v2 === "GT") { return Data$dOrdering.GT; }
    return Data$dOrd.ordInt.compare(x._4)(y._4);
  },
  Eq0: () => eqTime
};
const diff = dictDuration => t1 => t2 => dictDuration.toDuration(timeToMillis(t1) + -timeToMillis(t2));
const boundedTime = {bottom: /* #__PURE__ */ $Time(0, 0, 0, 0), top: /* #__PURE__ */ $Time(23, 59, 59, 999), Ord0: () => ordTime};
const maxTime = /* #__PURE__ */ timeToMillis(/* #__PURE__ */ $Time(23, 59, 59, 999));
const minTime = /* #__PURE__ */ timeToMillis(/* #__PURE__ */ $Time(0, 0, 0, 0));
const adjust = dictDuration => d => t => {
  const d$p = dictDuration.fromDuration(d);
  const wholeDays = Data$dNumber.floor(d$p / 86400000.0);
  const msAdjusted = timeToMillis(t) + d$p + -(wholeDays * 86400000.0);
  const wrap = (() => {
    if (msAdjusted > maxTime) { return 1.0; }
    if (msAdjusted < minTime) { return -1.0; }
    return 0.0;
  })();
  return Data$dTuple.$Tuple(wholeDays + wrap, millisToTime(msAdjusted + 86400000.0 * -wrap));
};
export {
  $Time,
  Time,
  adjust,
  boundedTime,
  diff,
  eqTime,
  hour,
  maxTime,
  millisToTime,
  millisecond,
  minTime,
  minute,
  ordTime,
  second,
  setHour,
  setMillisecond,
  setMinute,
  setSecond,
  showTime,
  timeToMillis
};
