// | A module providing a type and operations for the native JavaScript `Date`
// | object.
// |
// | The `JSDate` type and associated functions are provided for interop
// | purposes with JavaScript, but for working with dates in PureScript it is
// | recommended that `DateTime` representation is used instead - `DateTime`
// | offers greater type safety, a more PureScript-friendly interface, and has
// | a `Generic` instance.
import * as $runtime from "../runtime.js";
import * as Data$dDateTime$dInstant from "../Data.DateTime.Instant/index.js";
import * as Data$dIdentity from "../Data.Identity/index.js";
import * as Data$dInt from "../Data.Int/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Foreign from "../Foreign/index.js";
import {dateMethod, dateMethodEff, fromInstant, fromTime, isValid, jsdate, jsdateLocal, now, parse, toInstantImpl} from "./foreign.js";
const toUTCString = dt => dateMethod("toUTCString", dt);
const toTimeString = dt => dateMethod("toTimeString", dt);
const toString = dt => dateMethod("toString", dt);
const toInstant = /* #__PURE__ */ (() => {
  const $0 = toInstantImpl(Data$dMaybe.Just)(Data$dMaybe.Nothing);
  return a => {
    const $1 = $0(a);
    if ($1.tag === "Just") {
      if ($1._1 >= -8639977881600000.0 && $1._1 <= 8639977881599999.0) { return Data$dMaybe.$Maybe("Just", $1._1); }
      return Data$dMaybe.Nothing;
    }
    if ($1.tag === "Nothing") { return Data$dMaybe.Nothing; }
    $runtime.fail();
  };
})();
const toISOString = dt => dateMethodEff("toISOString", dt);
const toDateTime = x => {
  const $0 = toInstant(x);
  if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dDateTime$dInstant.toDateTime($0._1)); }
  return Data$dMaybe.Nothing;
};
const toDateString = dt => dateMethod("toDateString", dt);
const toDate = x => {
  const $0 = toInstant(x);
  if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dDateTime$dInstant.toDateTime($0._1)._1); }
  return Data$dMaybe.Nothing;
};
const readDate = /* #__PURE__ */ Foreign.unsafeReadTagged(Data$dIdentity.monadIdentity)("Date");
const getUTCSeconds = dt => dateMethod("getUTCSeconds", dt);
const getUTCMonth = dt => dateMethod("getUTCMonth", dt);
const getUTCMinutes = dt => dateMethod("getUTCMinutes", dt);
const getUTCMilliseconds = dt => dateMethod("getUTCMilliseconds", dt);
const getUTCHours = dt => dateMethod("getUTCHours", dt);
const getUTCFullYear = dt => dateMethod("getUTCFullYear", dt);
const getUTCDay = dt => dateMethod("getUTCDay", dt);
const getUTCDate = dt => dateMethod("getUTCDate", dt);
const getTimezoneOffset = dt => dateMethodEff("getTimezoneOffset", dt);
const getTime = dt => dateMethod("getTime", dt);
const showJSDate = {show: a => "(fromTime " + Data$dShow.showNumberImpl(dateMethod("getTime", a)) + ")"};
const getSeconds = dt => dateMethodEff("getSeconds", dt);
const getMonth = dt => dateMethodEff("getMonth", dt);
const getMinutes = dt => dateMethodEff("getMinutes", dt);
const getMilliseconds = dt => dateMethodEff("getMilliseconds", dt);
const getHours = dt => dateMethodEff("getHours", dt);
const getFullYear = dt => dateMethodEff("getFullYear", dt);
const getDay = dt => dateMethodEff("getDay", dt);
const getDate = dt => dateMethodEff("getDate", dt);
const fromDateTime = v => jsdate({
  year: Data$dInt.toNumber(v._1._1),
  month: Data$dInt.toNumber((() => {
    if (v._1._2 === "January") { return 0; }
    if (v._1._2 === "February") { return 1; }
    if (v._1._2 === "March") { return 2; }
    if (v._1._2 === "April") { return 3; }
    if (v._1._2 === "May") { return 4; }
    if (v._1._2 === "June") { return 5; }
    if (v._1._2 === "July") { return 6; }
    if (v._1._2 === "August") { return 7; }
    if (v._1._2 === "September") { return 8; }
    if (v._1._2 === "October") { return 9; }
    if (v._1._2 === "November") { return 10; }
    if (v._1._2 === "December") { return 11; }
    $runtime.fail();
  })()),
  day: Data$dInt.toNumber(v._1._3),
  hour: Data$dInt.toNumber(v._2._1),
  minute: Data$dInt.toNumber(v._2._2),
  second: Data$dInt.toNumber(v._2._3),
  millisecond: Data$dInt.toNumber(v._2._4)
});
const eqJSDate = {eq: a => b => dateMethod("getTime", a) === dateMethod("getTime", b)};
const ordJSDate = {compare: a => b => Data$dOrd.ordNumber.compare(dateMethod("getTime", a))(dateMethod("getTime", b)), Eq0: () => eqJSDate};
export {
  eqJSDate,
  fromDateTime,
  getDate,
  getDay,
  getFullYear,
  getHours,
  getMilliseconds,
  getMinutes,
  getMonth,
  getSeconds,
  getTime,
  getTimezoneOffset,
  getUTCDate,
  getUTCDay,
  getUTCFullYear,
  getUTCHours,
  getUTCMilliseconds,
  getUTCMinutes,
  getUTCMonth,
  getUTCSeconds,
  ordJSDate,
  readDate,
  showJSDate,
  toDate,
  toDateString,
  toDateTime,
  toISOString,
  toInstant,
  toString,
  toTimeString,
  toUTCString
};
export * from "./foreign.js";
