// | This module defines functions for comparing numbers.
import * as Data$dNumber from "../Data.Number/index.js";
const Tolerance = x => x;
const Fraction = x => x;
const eqRelative = v => v1 => v2 => {
  if (v1 === 0.0) { return Data$dNumber.abs(v2) <= v; }
  if (v2 === 0.0) { return Data$dNumber.abs(v1) <= v; }
  return Data$dNumber.abs(v1 - v2) <= v * Data$dNumber.abs(v1 + v2) / 2.0;
};
const eqApproximate = /* #__PURE__ */ eqRelative(0.000001);
const neqApproximate = x => y => !eqRelative(0.000001)(x)(y);
const eqAbsolute = v => x => y => Data$dNumber.abs(x - y) <= v;
export {Fraction, Tolerance, eqAbsolute, eqApproximate, eqRelative, neqApproximate};
