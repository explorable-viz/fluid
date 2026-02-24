// | Functions for working with PureScripts builtin `Number` type.
import * as Data$dMaybe from "../Data.Maybe/index.js";
import {abs, acos, asin, atan, atan2, ceil, cos, exp, floor, fromStringImpl, infinity, isFinite, isNaN, log, max, min, nan, pow, remainder, round, sign, sin, sqrt, tan, trunc} from "./foreign.js";
const tau = 6.283185307179586;
const sqrt2 = 1.4142135623730951;
const sqrt1_2 = 0.7071067811865476;
const pi = 3.141592653589793;
const log2e = 1.4426950408889634;
const log10e = 0.4342944819032518;
const ln2 = 0.6931471805599453;
const ln10 = 2.302585092994046;
const fromString = str => fromStringImpl(str, isFinite, Data$dMaybe.Just, Data$dMaybe.Nothing);
const e = 2.718281828459045;
;
export * from "./foreign.js";
