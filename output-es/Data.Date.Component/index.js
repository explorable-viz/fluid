import * as $runtime from "../runtime.js";
import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Data$dShow from "../Data.Show/index.js";
const $Month = tag => tag;
const $Weekday = tag => tag;
const Monday = /* #__PURE__ */ $Weekday("Monday");
const Tuesday = /* #__PURE__ */ $Weekday("Tuesday");
const Wednesday = /* #__PURE__ */ $Weekday("Wednesday");
const Thursday = /* #__PURE__ */ $Weekday("Thursday");
const Friday = /* #__PURE__ */ $Weekday("Friday");
const Saturday = /* #__PURE__ */ $Weekday("Saturday");
const Sunday = /* #__PURE__ */ $Weekday("Sunday");
const January = /* #__PURE__ */ $Month("January");
const February = /* #__PURE__ */ $Month("February");
const March = /* #__PURE__ */ $Month("March");
const April = /* #__PURE__ */ $Month("April");
const May = /* #__PURE__ */ $Month("May");
const June = /* #__PURE__ */ $Month("June");
const July = /* #__PURE__ */ $Month("July");
const August = /* #__PURE__ */ $Month("August");
const September = /* #__PURE__ */ $Month("September");
const October = /* #__PURE__ */ $Month("October");
const November = /* #__PURE__ */ $Month("November");
const December = /* #__PURE__ */ $Month("December");
const showYear = {show: v => "(Year " + Data$dShow.showIntImpl(v) + ")"};
const showWeekday = {
  show: v => {
    if (v === "Monday") { return "Monday"; }
    if (v === "Tuesday") { return "Tuesday"; }
    if (v === "Wednesday") { return "Wednesday"; }
    if (v === "Thursday") { return "Thursday"; }
    if (v === "Friday") { return "Friday"; }
    if (v === "Saturday") { return "Saturday"; }
    if (v === "Sunday") { return "Sunday"; }
    $runtime.fail();
  }
};
const showMonth = {
  show: v => {
    if (v === "January") { return "January"; }
    if (v === "February") { return "February"; }
    if (v === "March") { return "March"; }
    if (v === "April") { return "April"; }
    if (v === "May") { return "May"; }
    if (v === "June") { return "June"; }
    if (v === "July") { return "July"; }
    if (v === "August") { return "August"; }
    if (v === "September") { return "September"; }
    if (v === "October") { return "October"; }
    if (v === "November") { return "November"; }
    if (v === "December") { return "December"; }
    $runtime.fail();
  }
};
const showDay = {show: v => "(Day " + Data$dShow.showIntImpl(v) + ")"};
const ordYear = Data$dOrd.ordInt;
const ordDay = Data$dOrd.ordInt;
const eqYear = Data$dEq.eqInt;
const eqWeekday = {
  eq: x => y => {
    if (x === "Monday") { return y === "Monday"; }
    if (x === "Tuesday") { return y === "Tuesday"; }
    if (x === "Wednesday") { return y === "Wednesday"; }
    if (x === "Thursday") { return y === "Thursday"; }
    if (x === "Friday") { return y === "Friday"; }
    if (x === "Saturday") { return y === "Saturday"; }
    return x === "Sunday" && y === "Sunday";
  }
};
const ordWeekday = {
  compare: x => y => {
    if (x === "Monday") {
      if (y === "Monday") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Monday") { return Data$dOrdering.GT; }
    if (x === "Tuesday") {
      if (y === "Tuesday") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Tuesday") { return Data$dOrdering.GT; }
    if (x === "Wednesday") {
      if (y === "Wednesday") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Wednesday") { return Data$dOrdering.GT; }
    if (x === "Thursday") {
      if (y === "Thursday") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Thursday") { return Data$dOrdering.GT; }
    if (x === "Friday") {
      if (y === "Friday") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Friday") { return Data$dOrdering.GT; }
    if (x === "Saturday") {
      if (y === "Saturday") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Saturday") { return Data$dOrdering.GT; }
    if (x === "Sunday" && y === "Sunday") { return Data$dOrdering.EQ; }
    $runtime.fail();
  },
  Eq0: () => eqWeekday
};
const eqMonth = {
  eq: x => y => {
    if (x === "January") { return y === "January"; }
    if (x === "February") { return y === "February"; }
    if (x === "March") { return y === "March"; }
    if (x === "April") { return y === "April"; }
    if (x === "May") { return y === "May"; }
    if (x === "June") { return y === "June"; }
    if (x === "July") { return y === "July"; }
    if (x === "August") { return y === "August"; }
    if (x === "September") { return y === "September"; }
    if (x === "October") { return y === "October"; }
    if (x === "November") { return y === "November"; }
    return x === "December" && y === "December";
  }
};
const ordMonth = {
  compare: x => y => {
    if (x === "January") {
      if (y === "January") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "January") { return Data$dOrdering.GT; }
    if (x === "February") {
      if (y === "February") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "February") { return Data$dOrdering.GT; }
    if (x === "March") {
      if (y === "March") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "March") { return Data$dOrdering.GT; }
    if (x === "April") {
      if (y === "April") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "April") { return Data$dOrdering.GT; }
    if (x === "May") {
      if (y === "May") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "May") { return Data$dOrdering.GT; }
    if (x === "June") {
      if (y === "June") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "June") { return Data$dOrdering.GT; }
    if (x === "July") {
      if (y === "July") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "July") { return Data$dOrdering.GT; }
    if (x === "August") {
      if (y === "August") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "August") { return Data$dOrdering.GT; }
    if (x === "September") {
      if (y === "September") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "September") { return Data$dOrdering.GT; }
    if (x === "October") {
      if (y === "October") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "October") { return Data$dOrdering.GT; }
    if (x === "November") {
      if (y === "November") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "November") { return Data$dOrdering.GT; }
    if (x === "December" && y === "December") { return Data$dOrdering.EQ; }
    $runtime.fail();
  },
  Eq0: () => eqMonth
};
const eqDay = Data$dEq.eqInt;
const boundedYear = {bottom: -271820, top: 275759, Ord0: () => Data$dOrd.ordInt};
const boundedWeekday = {bottom: Monday, top: Sunday, Ord0: () => ordWeekday};
const boundedMonth = {bottom: January, top: December, Ord0: () => ordMonth};
const boundedEnumYear = {
  cardinality: 547580,
  toEnum: n => {
    if (n >= -271820 && n <= 275759) { return Data$dMaybe.$Maybe("Just", n); }
    return Data$dMaybe.Nothing;
  },
  fromEnum: v => v,
  Bounded0: () => boundedYear,
  Enum1: () => enumYear
};
const enumYear = {
  succ: x => {
    const $0 = x + 1 | 0;
    if ($0 >= -271820 && $0 <= 275759) { return Data$dMaybe.$Maybe("Just", $0); }
    return Data$dMaybe.Nothing;
  },
  pred: x => {
    const $0 = x - 1 | 0;
    if ($0 >= -271820 && $0 <= 275759) { return Data$dMaybe.$Maybe("Just", $0); }
    return Data$dMaybe.Nothing;
  },
  Ord0: () => Data$dOrd.ordInt
};
const boundedEnumWeekday = {
  cardinality: 7,
  toEnum: v => {
    if (v === 1) { return Data$dMaybe.$Maybe("Just", Monday); }
    if (v === 2) { return Data$dMaybe.$Maybe("Just", Tuesday); }
    if (v === 3) { return Data$dMaybe.$Maybe("Just", Wednesday); }
    if (v === 4) { return Data$dMaybe.$Maybe("Just", Thursday); }
    if (v === 5) { return Data$dMaybe.$Maybe("Just", Friday); }
    if (v === 6) { return Data$dMaybe.$Maybe("Just", Saturday); }
    if (v === 7) { return Data$dMaybe.$Maybe("Just", Sunday); }
    return Data$dMaybe.Nothing;
  },
  fromEnum: v => {
    if (v === "Monday") { return 1; }
    if (v === "Tuesday") { return 2; }
    if (v === "Wednesday") { return 3; }
    if (v === "Thursday") { return 4; }
    if (v === "Friday") { return 5; }
    if (v === "Saturday") { return 6; }
    if (v === "Sunday") { return 7; }
    $runtime.fail();
  },
  Bounded0: () => boundedWeekday,
  Enum1: () => enumWeekday
};
const enumWeekday = {
  succ: x => {
    const $0 = (() => {
      if (x === "Monday") { return 2; }
      if (x === "Tuesday") { return 3; }
      if (x === "Wednesday") { return 4; }
      if (x === "Thursday") { return 5; }
      if (x === "Friday") { return 6; }
      if (x === "Saturday") { return 7; }
      if (x === "Sunday") { return 8; }
      $runtime.fail();
    })();
    if ($0 === 1) { return Data$dMaybe.$Maybe("Just", Monday); }
    if ($0 === 2) { return Data$dMaybe.$Maybe("Just", Tuesday); }
    if ($0 === 3) { return Data$dMaybe.$Maybe("Just", Wednesday); }
    if ($0 === 4) { return Data$dMaybe.$Maybe("Just", Thursday); }
    if ($0 === 5) { return Data$dMaybe.$Maybe("Just", Friday); }
    if ($0 === 6) { return Data$dMaybe.$Maybe("Just", Saturday); }
    if ($0 === 7) { return Data$dMaybe.$Maybe("Just", Sunday); }
    return Data$dMaybe.Nothing;
  },
  pred: x => {
    const $0 = (() => {
      if (x === "Monday") { return 0; }
      if (x === "Tuesday") { return 1; }
      if (x === "Wednesday") { return 2; }
      if (x === "Thursday") { return 3; }
      if (x === "Friday") { return 4; }
      if (x === "Saturday") { return 5; }
      if (x === "Sunday") { return 6; }
      $runtime.fail();
    })();
    if ($0 === 1) { return Data$dMaybe.$Maybe("Just", Monday); }
    if ($0 === 2) { return Data$dMaybe.$Maybe("Just", Tuesday); }
    if ($0 === 3) { return Data$dMaybe.$Maybe("Just", Wednesday); }
    if ($0 === 4) { return Data$dMaybe.$Maybe("Just", Thursday); }
    if ($0 === 5) { return Data$dMaybe.$Maybe("Just", Friday); }
    if ($0 === 6) { return Data$dMaybe.$Maybe("Just", Saturday); }
    if ($0 === 7) { return Data$dMaybe.$Maybe("Just", Sunday); }
    return Data$dMaybe.Nothing;
  },
  Ord0: () => ordWeekday
};
const boundedEnumMonth = {
  cardinality: 12,
  toEnum: v => {
    if (v === 1) { return Data$dMaybe.$Maybe("Just", January); }
    if (v === 2) { return Data$dMaybe.$Maybe("Just", February); }
    if (v === 3) { return Data$dMaybe.$Maybe("Just", March); }
    if (v === 4) { return Data$dMaybe.$Maybe("Just", April); }
    if (v === 5) { return Data$dMaybe.$Maybe("Just", May); }
    if (v === 6) { return Data$dMaybe.$Maybe("Just", June); }
    if (v === 7) { return Data$dMaybe.$Maybe("Just", July); }
    if (v === 8) { return Data$dMaybe.$Maybe("Just", August); }
    if (v === 9) { return Data$dMaybe.$Maybe("Just", September); }
    if (v === 10) { return Data$dMaybe.$Maybe("Just", October); }
    if (v === 11) { return Data$dMaybe.$Maybe("Just", November); }
    if (v === 12) { return Data$dMaybe.$Maybe("Just", December); }
    return Data$dMaybe.Nothing;
  },
  fromEnum: v => {
    if (v === "January") { return 1; }
    if (v === "February") { return 2; }
    if (v === "March") { return 3; }
    if (v === "April") { return 4; }
    if (v === "May") { return 5; }
    if (v === "June") { return 6; }
    if (v === "July") { return 7; }
    if (v === "August") { return 8; }
    if (v === "September") { return 9; }
    if (v === "October") { return 10; }
    if (v === "November") { return 11; }
    if (v === "December") { return 12; }
    $runtime.fail();
  },
  Bounded0: () => boundedMonth,
  Enum1: () => enumMonth
};
const enumMonth = {
  succ: x => {
    const $0 = (() => {
      if (x === "January") { return 2; }
      if (x === "February") { return 3; }
      if (x === "March") { return 4; }
      if (x === "April") { return 5; }
      if (x === "May") { return 6; }
      if (x === "June") { return 7; }
      if (x === "July") { return 8; }
      if (x === "August") { return 9; }
      if (x === "September") { return 10; }
      if (x === "October") { return 11; }
      if (x === "November") { return 12; }
      if (x === "December") { return 13; }
      $runtime.fail();
    })();
    if ($0 === 1) { return Data$dMaybe.$Maybe("Just", January); }
    if ($0 === 2) { return Data$dMaybe.$Maybe("Just", February); }
    if ($0 === 3) { return Data$dMaybe.$Maybe("Just", March); }
    if ($0 === 4) { return Data$dMaybe.$Maybe("Just", April); }
    if ($0 === 5) { return Data$dMaybe.$Maybe("Just", May); }
    if ($0 === 6) { return Data$dMaybe.$Maybe("Just", June); }
    if ($0 === 7) { return Data$dMaybe.$Maybe("Just", July); }
    if ($0 === 8) { return Data$dMaybe.$Maybe("Just", August); }
    if ($0 === 9) { return Data$dMaybe.$Maybe("Just", September); }
    if ($0 === 10) { return Data$dMaybe.$Maybe("Just", October); }
    if ($0 === 11) { return Data$dMaybe.$Maybe("Just", November); }
    if ($0 === 12) { return Data$dMaybe.$Maybe("Just", December); }
    return Data$dMaybe.Nothing;
  },
  pred: x => {
    const $0 = (() => {
      if (x === "January") { return 0; }
      if (x === "February") { return 1; }
      if (x === "March") { return 2; }
      if (x === "April") { return 3; }
      if (x === "May") { return 4; }
      if (x === "June") { return 5; }
      if (x === "July") { return 6; }
      if (x === "August") { return 7; }
      if (x === "September") { return 8; }
      if (x === "October") { return 9; }
      if (x === "November") { return 10; }
      if (x === "December") { return 11; }
      $runtime.fail();
    })();
    if ($0 === 1) { return Data$dMaybe.$Maybe("Just", January); }
    if ($0 === 2) { return Data$dMaybe.$Maybe("Just", February); }
    if ($0 === 3) { return Data$dMaybe.$Maybe("Just", March); }
    if ($0 === 4) { return Data$dMaybe.$Maybe("Just", April); }
    if ($0 === 5) { return Data$dMaybe.$Maybe("Just", May); }
    if ($0 === 6) { return Data$dMaybe.$Maybe("Just", June); }
    if ($0 === 7) { return Data$dMaybe.$Maybe("Just", July); }
    if ($0 === 8) { return Data$dMaybe.$Maybe("Just", August); }
    if ($0 === 9) { return Data$dMaybe.$Maybe("Just", September); }
    if ($0 === 10) { return Data$dMaybe.$Maybe("Just", October); }
    if ($0 === 11) { return Data$dMaybe.$Maybe("Just", November); }
    if ($0 === 12) { return Data$dMaybe.$Maybe("Just", December); }
    return Data$dMaybe.Nothing;
  },
  Ord0: () => ordMonth
};
const boundedDay = {bottom: 1, top: 31, Ord0: () => Data$dOrd.ordInt};
const boundedEnumDay = {
  cardinality: 31,
  toEnum: n => {
    if (n >= 1 && n <= 31) { return Data$dMaybe.$Maybe("Just", n); }
    return Data$dMaybe.Nothing;
  },
  fromEnum: v => v,
  Bounded0: () => boundedDay,
  Enum1: () => enumDay
};
const enumDay = {
  succ: x => {
    const $0 = x + 1 | 0;
    if ($0 >= 1 && $0 <= 31) { return Data$dMaybe.$Maybe("Just", $0); }
    return Data$dMaybe.Nothing;
  },
  pred: x => {
    const $0 = x - 1 | 0;
    if ($0 >= 1 && $0 <= 31) { return Data$dMaybe.$Maybe("Just", $0); }
    return Data$dMaybe.Nothing;
  },
  Ord0: () => Data$dOrd.ordInt
};
export {
  $Month,
  $Weekday,
  April,
  August,
  December,
  February,
  Friday,
  January,
  July,
  June,
  March,
  May,
  Monday,
  November,
  October,
  Saturday,
  September,
  Sunday,
  Thursday,
  Tuesday,
  Wednesday,
  boundedDay,
  boundedEnumDay,
  boundedEnumMonth,
  boundedEnumWeekday,
  boundedEnumYear,
  boundedMonth,
  boundedWeekday,
  boundedYear,
  enumDay,
  enumMonth,
  enumWeekday,
  enumYear,
  eqDay,
  eqMonth,
  eqWeekday,
  eqYear,
  ordDay,
  ordMonth,
  ordWeekday,
  ordYear,
  showDay,
  showMonth,
  showWeekday,
  showYear
};
