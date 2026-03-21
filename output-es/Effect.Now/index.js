import * as Data$dDateTime$dInstant from "../Data.DateTime.Instant/index.js";
import {getTimezoneOffset, now} from "./foreign.js";
const nowTime = () => {
  const a$p = now();
  return Data$dDateTime$dInstant.toDateTime(a$p)._2;
};
const nowDateTime = () => {
  const a$p = now();
  return Data$dDateTime$dInstant.toDateTime(a$p);
};
const nowDate = () => {
  const a$p = now();
  return Data$dDateTime$dInstant.toDateTime(a$p)._1;
};
export {nowDate, nowDateTime, nowTime};
export * from "./foreign.js";
