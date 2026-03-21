import * as Data$dInt from "../Data.Int/index.js";
import * as Data$dNumber from "../Data.Number/index.js";
import {random} from "./foreign.js";
const randomRange = min => max => () => {
  const n = random();
  return n * (max - min) + min;
};
const randomInt = low => high => () => {
  const n = random();
  return Data$dInt.unsafeClamp(Data$dNumber.floor((Data$dInt.toNumber(high) - Data$dInt.toNumber(low) + 1.0) * n + Data$dInt.toNumber(low)));
};
const randomBool = () => {
  const a$p = random();
  return a$p < 0.5;
};
export {randomBool, randomInt, randomRange};
export * from "./foreign.js";
