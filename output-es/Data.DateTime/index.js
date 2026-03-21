import * as $runtime from "../runtime.js";
import * as Data$dDate from "../Data.Date/index.js";
import * as Data$dDate$dComponent from "../Data.Date.Component/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Data$dTime from "../Data.Time/index.js";
import {adjustImpl, calcDiff} from "./foreign.js";
const $DateTime = (_1, _2) => ({tag: "DateTime", _1, _2});
const DateTime = value0 => value1 => $DateTime(value0, value1);
const toRecord = v => (
  {
    year: v._1._1,
    month: (() => {
      if (v._1._2 === "January") { return 1; }
      if (v._1._2 === "February") { return 2; }
      if (v._1._2 === "March") { return 3; }
      if (v._1._2 === "April") { return 4; }
      if (v._1._2 === "May") { return 5; }
      if (v._1._2 === "June") { return 6; }
      if (v._1._2 === "July") { return 7; }
      if (v._1._2 === "August") { return 8; }
      if (v._1._2 === "September") { return 9; }
      if (v._1._2 === "October") { return 10; }
      if (v._1._2 === "November") { return 11; }
      if (v._1._2 === "December") { return 12; }
      $runtime.fail();
    })(),
    day: v._1._3,
    hour: v._2._1,
    minute: v._2._2,
    second: v._2._3,
    millisecond: v._2._4
  }
);
const time = v => v._2;
const showDateTime = {show: v => "(DateTime " + Data$dDate.showDate.show(v._1) + " " + Data$dTime.showTime.show(v._2) + ")"};
const modifyTimeF = dictFunctor => f => v => dictFunctor.map(DateTime(v._1))(f(v._2));
const modifyTime = f => v => $DateTime(v._1, f(v._2));
const modifyDateF = dictFunctor => f => v => {
  const $0 = v._2;
  return dictFunctor.map(a => $DateTime(a, $0))(f(v._1));
};
const modifyDate = f => v => $DateTime(f(v._1), v._2);
const eqDateTime = {eq: x => y => Data$dDate.eqDate.eq(x._1)(y._1) && x._2._1 === y._2._1 && x._2._2 === y._2._2 && x._2._3 === y._2._3 && x._2._4 === y._2._4};
const ordDateTime = {
  compare: x => y => {
    const v = Data$dDate.ordDate.compare(x._1)(y._1);
    if (v === "LT") { return Data$dOrdering.LT; }
    if (v === "GT") { return Data$dOrdering.GT; }
    return Data$dTime.ordTime.compare(x._2)(y._2);
  },
  Eq0: () => eqDateTime
};
const diff = dictDuration => dt1 => dt2 => dictDuration.toDuration(calcDiff(toRecord(dt1), toRecord(dt2)));
const date = v => v._1;
const boundedDateTime = {
  bottom: /* #__PURE__ */ $DateTime(/* #__PURE__ */ Data$dDate.$$$Date(-271820, Data$dDate$dComponent.January, 1), /* #__PURE__ */ Data$dTime.$Time(0, 0, 0, 0)),
  top: /* #__PURE__ */ $DateTime(/* #__PURE__ */ Data$dDate.$$$Date(275759, Data$dDate$dComponent.December, 31), /* #__PURE__ */ Data$dTime.$Time(23, 59, 59, 999)),
  Ord0: () => ordDateTime
};
const adjust = dictDuration => d => dt => {
  const $0 = adjustImpl(Data$dMaybe.Just)(Data$dMaybe.Nothing)(dictDuration.fromDuration(d))(toRecord(dt));
  if ($0.tag === "Just") {
    const $1 = $0._1.year >= -271820 && $0._1.year <= 275759 ? Data$dMaybe.$Maybe("Just", $0._1.year) : Data$dMaybe.Nothing;
    const $2 = $1.tag === "Just" ? Data$dMaybe.$Maybe("Just", Data$dDate.exactDate($1._1)) : Data$dMaybe.Nothing;
    const $3 = (() => {
      if ($0._1.month === 1) { return Data$dMaybe.$Maybe("Just", Data$dDate$dComponent.January); }
      if ($0._1.month === 2) { return Data$dMaybe.$Maybe("Just", Data$dDate$dComponent.February); }
      if ($0._1.month === 3) { return Data$dMaybe.$Maybe("Just", Data$dDate$dComponent.March); }
      if ($0._1.month === 4) { return Data$dMaybe.$Maybe("Just", Data$dDate$dComponent.April); }
      if ($0._1.month === 5) { return Data$dMaybe.$Maybe("Just", Data$dDate$dComponent.May); }
      if ($0._1.month === 6) { return Data$dMaybe.$Maybe("Just", Data$dDate$dComponent.June); }
      if ($0._1.month === 7) { return Data$dMaybe.$Maybe("Just", Data$dDate$dComponent.July); }
      if ($0._1.month === 8) { return Data$dMaybe.$Maybe("Just", Data$dDate$dComponent.August); }
      if ($0._1.month === 9) { return Data$dMaybe.$Maybe("Just", Data$dDate$dComponent.September); }
      if ($0._1.month === 10) { return Data$dMaybe.$Maybe("Just", Data$dDate$dComponent.October); }
      if ($0._1.month === 11) { return Data$dMaybe.$Maybe("Just", Data$dDate$dComponent.November); }
      if ($0._1.month === 12) { return Data$dMaybe.$Maybe("Just", Data$dDate$dComponent.December); }
      return Data$dMaybe.Nothing;
    })();
    const $4 = (() => {
      if ($2.tag === "Just") {
        if ($3.tag === "Just") {
          if ($0._1.day >= 1 && $0._1.day <= 31) { return Data$dMaybe.$Maybe("Just", $2._1($3._1)($0._1.day)); }
          return Data$dMaybe.Nothing;
        }
        return Data$dMaybe.Nothing;
      }
      if ($2.tag === "Nothing") { return Data$dMaybe.Nothing; }
      $runtime.fail();
    })();
    const $5 = (() => {
      if ($4.tag === "Just") { return $4._1; }
      if ($4.tag === "Nothing") { return Data$dMaybe.Nothing; }
      $runtime.fail();
    })();
    const $6 = $5.tag === "Just" ? Data$dMaybe.$Maybe("Just", DateTime($5._1)) : Data$dMaybe.Nothing;
    if ($0._1.hour >= 0 && $0._1.hour <= 23) {
      if ($0._1.minute >= 0 && $0._1.minute <= 59) {
        if ($0._1.second >= 0 && $0._1.second <= 59) {
          if ($0._1.millisecond >= 0 && $0._1.millisecond <= 999) {
            if ($6.tag === "Just") { return Data$dMaybe.$Maybe("Just", $6._1(Data$dTime.$Time($0._1.hour, $0._1.minute, $0._1.second, $0._1.millisecond))); }
            if ($6.tag === "Nothing") { return Data$dMaybe.Nothing; }
            $runtime.fail();
          }
          if ($6.tag === "Just") { return Data$dMaybe.Nothing; }
          if ($6.tag === "Nothing") { return Data$dMaybe.Nothing; }
          $runtime.fail();
        }
        if ($6.tag === "Just") { return Data$dMaybe.Nothing; }
        if ($6.tag === "Nothing") { return Data$dMaybe.Nothing; }
        $runtime.fail();
      }
      if ($0._1.second >= 0 && $0._1.second <= 59) {
        if ($6.tag === "Just") { return Data$dMaybe.Nothing; }
        if ($6.tag === "Nothing") { return Data$dMaybe.Nothing; }
        $runtime.fail();
      }
      if ($6.tag === "Just") { return Data$dMaybe.Nothing; }
      if ($6.tag === "Nothing") { return Data$dMaybe.Nothing; }
      $runtime.fail();
    }
    if ($6.tag === "Just") { return Data$dMaybe.Nothing; }
    if ($6.tag === "Nothing") { return Data$dMaybe.Nothing; }
    $runtime.fail();
  }
  if ($0.tag === "Nothing") { return Data$dMaybe.Nothing; }
  $runtime.fail();
};
export {$DateTime, DateTime, adjust, boundedDateTime, date, diff, eqDateTime, modifyDate, modifyDateF, modifyTime, modifyTimeF, ordDateTime, showDateTime, time, toRecord};
export * from "./foreign.js";
