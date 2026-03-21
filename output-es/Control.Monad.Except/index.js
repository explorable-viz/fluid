import * as $runtime from "../runtime.js";
import * as Data$dEither from "../Data.Either/index.js";
const withExcept = f => v => {
  if (v.tag === "Right") { return Data$dEither.$Either("Right", v._1); }
  if (v.tag === "Left") { return Data$dEither.$Either("Left", f(v._1)); }
  $runtime.fail();
};
const runExcept = x => x;
const mapExcept = f => v => f(v);
export {mapExcept, runExcept, withExcept};
