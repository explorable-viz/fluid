import * as $runtime from "../runtime.js";
import * as Data$dDate$dComponent from "../Data.Date.Component/index.js";
import * as Data$dEuclideanRing from "../Data.EuclideanRing/index.js";
import * as Data$dInt from "../Data.Int/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import {calcDiff, calcWeekday, canonicalDateImpl} from "./foreign.js";
const $$$Date = (_1, _2, _3) => ({tag: "Date", _1, _2, _3});
const $$Date = value0 => value1 => value2 => $$$Date(value0, value1, value2);
const year = v => v._1;
const weekday = v => {
  const n = calcWeekday(
    v._1,
    (() => {
      if (v._2 === "January") { return 1; }
      if (v._2 === "February") { return 2; }
      if (v._2 === "March") { return 3; }
      if (v._2 === "April") { return 4; }
      if (v._2 === "May") { return 5; }
      if (v._2 === "June") { return 6; }
      if (v._2 === "July") { return 7; }
      if (v._2 === "August") { return 8; }
      if (v._2 === "September") { return 9; }
      if (v._2 === "October") { return 10; }
      if (v._2 === "November") { return 11; }
      if (v._2 === "December") { return 12; }
      $runtime.fail();
    })(),
    v._3
  );
  if (n === 0) { return Data$dDate$dComponent.Sunday; }
  if (n === 1) { return Data$dDate$dComponent.Monday; }
  if (n === 2) { return Data$dDate$dComponent.Tuesday; }
  if (n === 3) { return Data$dDate$dComponent.Wednesday; }
  if (n === 4) { return Data$dDate$dComponent.Thursday; }
  if (n === 5) { return Data$dDate$dComponent.Friday; }
  if (n === 6) { return Data$dDate$dComponent.Saturday; }
  if (n === 7) { return Data$dDate$dComponent.Sunday; }
  $runtime.fail();
};
const showDate = {
  show: v => "(Date (Year " + Data$dShow.showIntImpl(v._1) + ") " + (() => {
    if (v._2 === "January") { return "January"; }
    if (v._2 === "February") { return "February"; }
    if (v._2 === "March") { return "March"; }
    if (v._2 === "April") { return "April"; }
    if (v._2 === "May") { return "May"; }
    if (v._2 === "June") { return "June"; }
    if (v._2 === "July") { return "July"; }
    if (v._2 === "August") { return "August"; }
    if (v._2 === "September") { return "September"; }
    if (v._2 === "October") { return "October"; }
    if (v._2 === "November") { return "November"; }
    if (v._2 === "December") { return "December"; }
    $runtime.fail();
  })() + " (Day " + Data$dShow.showIntImpl(v._3) + "))"
};
const month = v => v._2;
const isLeapYear = y => Data$dEuclideanRing.intMod(y)(4) === 0 && (Data$dEuclideanRing.intMod(y)(400) === 0 || Data$dEuclideanRing.intMod(y)(100) !== 0);
const lastDayOfMonth = y => m => {
  if (m === "January") { return 31; }
  if (m === "February") {
    if (isLeapYear(y)) { return 29; }
    return 28;
  }
  if (m === "March") { return 31; }
  if (m === "April") { return 30; }
  if (m === "May") { return 31; }
  if (m === "June") { return 30; }
  if (m === "July") { return 31; }
  if (m === "August") { return 31; }
  if (m === "September") { return 30; }
  if (m === "October") { return 31; }
  if (m === "November") { return 30; }
  if (m === "December") { return 31; }
  $runtime.fail();
};
const eqDate = {
  eq: x => y => x._1 === y._1 && (() => {
    if (x._2 === "January") { return y._2 === "January"; }
    if (x._2 === "February") { return y._2 === "February"; }
    if (x._2 === "March") { return y._2 === "March"; }
    if (x._2 === "April") { return y._2 === "April"; }
    if (x._2 === "May") { return y._2 === "May"; }
    if (x._2 === "June") { return y._2 === "June"; }
    if (x._2 === "July") { return y._2 === "July"; }
    if (x._2 === "August") { return y._2 === "August"; }
    if (x._2 === "September") { return y._2 === "September"; }
    if (x._2 === "October") { return y._2 === "October"; }
    if (x._2 === "November") { return y._2 === "November"; }
    return x._2 === "December" && y._2 === "December";
  })() && x._3 === y._3
};
const ordDate = {
  compare: x => y => {
    const v = Data$dOrd.ordInt.compare(x._1)(y._1);
    if (v === "LT") { return Data$dOrdering.LT; }
    if (v === "GT") { return Data$dOrdering.GT; }
    const v1 = Data$dDate$dComponent.ordMonth.compare(x._2)(y._2);
    if (v1 === "LT") { return Data$dOrdering.LT; }
    if (v1 === "GT") { return Data$dOrdering.GT; }
    return Data$dOrd.ordInt.compare(x._3)(y._3);
  },
  Eq0: () => eqDate
};
const enumDate = {
  succ: v => {
    const sm = Data$dDate$dComponent.enumMonth.succ(v._2);
    const $0 = v._3 + 1 | 0;
    const v1 = $0 >= 1 && $0 <= 31 ? Data$dMaybe.$Maybe("Just", $0) : Data$dMaybe.Nothing;
    if (
      (() => {
        const $1 = lastDayOfMonth(v._1)(v._2);
        if (v1.tag === "Nothing") { return false; }
        if (v1.tag === "Just") { return v1._1 > $1; }
        $runtime.fail();
      })()
    ) {
      if (
        (() => {
          if (sm.tag === "Nothing") { return true; }
          if (sm.tag === "Just") { return false; }
          $runtime.fail();
        })()
      ) {
        const $1 = v._1 + 1 | 0;
        if ($1 >= -271820 && $1 <= 275759) {
          return Data$dMaybe.$Maybe(
            "Just",
            $$$Date(
              $1,
              (() => {
                if (sm.tag === "Nothing") { return Data$dDate$dComponent.January; }
                if (sm.tag === "Just") { return sm._1; }
                $runtime.fail();
              })(),
              1
            )
          );
        }
        return Data$dMaybe.Nothing;
      }
      return Data$dMaybe.$Maybe(
        "Just",
        $$$Date(
          v._1,
          (() => {
            if (sm.tag === "Nothing") { return Data$dDate$dComponent.January; }
            if (sm.tag === "Just") { return sm._1; }
            $runtime.fail();
          })(),
          1
        )
      );
    }
    if (
      (() => {
        if (v1.tag === "Nothing") { return true; }
        if (v1.tag === "Just") { return false; }
        $runtime.fail();
      })() && (() => {
        if (sm.tag === "Nothing") { return true; }
        if (sm.tag === "Just") { return false; }
        $runtime.fail();
      })()
    ) {
      const $1 = v._1 + 1 | 0;
      if ($1 >= -271820 && $1 <= 275759) {
        const $2 = $$Date($1)((() => {
          if (
            (() => {
              if (v1.tag === "Nothing") { return true; }
              if (v1.tag === "Just") { return false; }
              $runtime.fail();
            })()
          ) {
            if (sm.tag === "Nothing") { return Data$dDate$dComponent.January; }
            if (sm.tag === "Just") { return sm._1; }
            $runtime.fail();
          }
          return v._2;
        })());
        if (
          (() => {
            if (v1.tag === "Nothing") { return true; }
            if (v1.tag === "Just") { return false; }
            $runtime.fail();
          })()
        ) {
          return Data$dMaybe.$Maybe("Just", $2(1));
        }
        if (v1.tag === "Just") { return Data$dMaybe.$Maybe("Just", $2(v1._1)); }
      }
      return Data$dMaybe.Nothing;
    }
    const $1 = $$Date(v._1)((() => {
      if (
        (() => {
          if (v1.tag === "Nothing") { return true; }
          if (v1.tag === "Just") { return false; }
          $runtime.fail();
        })()
      ) {
        if (sm.tag === "Nothing") { return Data$dDate$dComponent.January; }
        if (sm.tag === "Just") { return sm._1; }
        $runtime.fail();
      }
      return v._2;
    })());
    if (
      (() => {
        if (v1.tag === "Nothing") { return true; }
        if (v1.tag === "Just") { return false; }
        $runtime.fail();
      })()
    ) {
      return Data$dMaybe.$Maybe("Just", $1(1));
    }
    if (v1.tag === "Just") { return Data$dMaybe.$Maybe("Just", $1(v1._1)); }
    return Data$dMaybe.Nothing;
  },
  pred: v => {
    const pm = Data$dDate$dComponent.enumMonth.pred(v._2);
    const $0 = v._3 - 1 | 0;
    if ($0 >= 1 && $0 <= 31) { return Data$dMaybe.$Maybe("Just", $$$Date(v._1, v._2, $0)); }
    const m$p = (() => {
      if (pm.tag === "Nothing") { return Data$dDate$dComponent.December; }
      if (pm.tag === "Just") { return pm._1; }
      $runtime.fail();
    })();
    const l = lastDayOfMonth(v._1)(m$p);
    if (
      (() => {
        if (pm.tag === "Nothing") { return true; }
        if (pm.tag === "Just") { return false; }
        $runtime.fail();
      })()
    ) {
      const $1 = v._1 - 1 | 0;
      if ($1 >= -271820 && $1 <= 275759) { return Data$dMaybe.$Maybe("Just", $$$Date($1, m$p, l)); }
      return Data$dMaybe.Nothing;
    }
    return Data$dMaybe.$Maybe("Just", $$$Date(v._1, m$p, l));
  },
  Ord0: () => ordDate
};
const diff = dictDuration => v => v1 => dictDuration.toDuration(calcDiff(
  v._1,
  (() => {
    if (v._2 === "January") { return 1; }
    if (v._2 === "February") { return 2; }
    if (v._2 === "March") { return 3; }
    if (v._2 === "April") { return 4; }
    if (v._2 === "May") { return 5; }
    if (v._2 === "June") { return 6; }
    if (v._2 === "July") { return 7; }
    if (v._2 === "August") { return 8; }
    if (v._2 === "September") { return 9; }
    if (v._2 === "October") { return 10; }
    if (v._2 === "November") { return 11; }
    if (v._2 === "December") { return 12; }
    $runtime.fail();
  })(),
  v._3,
  v1._1,
  (() => {
    if (v1._2 === "January") { return 1; }
    if (v1._2 === "February") { return 2; }
    if (v1._2 === "March") { return 3; }
    if (v1._2 === "April") { return 4; }
    if (v1._2 === "May") { return 5; }
    if (v1._2 === "June") { return 6; }
    if (v1._2 === "July") { return 7; }
    if (v1._2 === "August") { return 8; }
    if (v1._2 === "September") { return 9; }
    if (v1._2 === "October") { return 10; }
    if (v1._2 === "November") { return 11; }
    if (v1._2 === "December") { return 12; }
    $runtime.fail();
  })(),
  v1._3
));
const day = v => v._3;
const canonicalDate = y => m => d => canonicalDateImpl(
  y$p => m$p => d$p => $$$Date(
    y$p,
    (() => {
      if (m$p === 1) { return Data$dDate$dComponent.January; }
      if (m$p === 2) { return Data$dDate$dComponent.February; }
      if (m$p === 3) { return Data$dDate$dComponent.March; }
      if (m$p === 4) { return Data$dDate$dComponent.April; }
      if (m$p === 5) { return Data$dDate$dComponent.May; }
      if (m$p === 6) { return Data$dDate$dComponent.June; }
      if (m$p === 7) { return Data$dDate$dComponent.July; }
      if (m$p === 8) { return Data$dDate$dComponent.August; }
      if (m$p === 9) { return Data$dDate$dComponent.September; }
      if (m$p === 10) { return Data$dDate$dComponent.October; }
      if (m$p === 11) { return Data$dDate$dComponent.November; }
      if (m$p === 12) { return Data$dDate$dComponent.December; }
      $runtime.fail();
    })(),
    d$p
  ),
  y,
  (() => {
    if (m === "January") { return 1; }
    if (m === "February") { return 2; }
    if (m === "March") { return 3; }
    if (m === "April") { return 4; }
    if (m === "May") { return 5; }
    if (m === "June") { return 6; }
    if (m === "July") { return 7; }
    if (m === "August") { return 8; }
    if (m === "September") { return 9; }
    if (m === "October") { return 10; }
    if (m === "November") { return 11; }
    if (m === "December") { return 12; }
    $runtime.fail();
  })(),
  d
);
const exactDate = y => m => d => {
  if (eqDate.eq(canonicalDate(y)(m)(d))($$$Date(y, m, d))) { return Data$dMaybe.$Maybe("Just", $$$Date(y, m, d)); }
  return Data$dMaybe.Nothing;
};
const boundedDate = {
  bottom: /* #__PURE__ */ $$$Date(-271820, Data$dDate$dComponent.January, 1),
  top: /* #__PURE__ */ $$$Date(275759, Data$dDate$dComponent.December, 31),
  Ord0: () => ordDate
};
const adjust = v => date => {
  const adj = v1 => v2 => {
    if (v1 === 0) { return Data$dMaybe.$Maybe("Just", v2); }
    const j = v1 + v2._3 | 0;
    const low = j < 1;
    const l = lastDayOfMonth(v2._1)((() => {
      if (low) {
        const $0 = Data$dDate$dComponent.enumMonth.pred(v2._2);
        if ($0.tag === "Nothing") { return Data$dDate$dComponent.December; }
        if ($0.tag === "Just") { return $0._1; }
        $runtime.fail();
      }
      return v2._2;
    })());
    const hi = j > l;
    const $0 = adj((() => {
      if (low) { return j; }
      if (hi) { return (j - l | 0) - 1 | 0; }
      return 0;
    })());
    const $1 = (() => {
      if (low) { return enumDate.pred($$$Date(v2._1, v2._2, 1)); }
      if (hi) { return enumDate.succ($$$Date(v2._1, v2._2, l)); }
      const $1 = $$Date(v2._1)(v2._2);
      if (j >= 1 && j <= 31) { return Data$dMaybe.$Maybe("Just", $1(j)); }
      return Data$dMaybe.Nothing;
    })();
    if ($1.tag === "Just") { return $0($1._1); }
    if ($1.tag === "Nothing") { return Data$dMaybe.Nothing; }
    $runtime.fail();
  };
  const $0 = Data$dInt.fromNumber(v);
  if ($0.tag === "Just") { return adj($0._1)(date); }
  if ($0.tag === "Nothing") { return Data$dMaybe.Nothing; }
  $runtime.fail();
};
export {$$$Date, $$Date as Date, adjust, boundedDate, canonicalDate, day, diff, enumDate, eqDate, exactDate, isLeapYear, lastDayOfMonth, month, ordDate, showDate, weekday, year};
export * from "./foreign.js";
