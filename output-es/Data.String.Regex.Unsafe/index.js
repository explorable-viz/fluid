import * as $runtime from "../runtime.js";
import * as Data$dString$dRegex from "../Data.String.Regex/index.js";
import * as Partial from "../Partial/index.js";
const unsafeRegex = s => f => {
  const $0 = Data$dString$dRegex.regex(s)(f);
  if ($0.tag === "Left") { return Partial._crashWith($0._1); }
  if ($0.tag === "Right") { return $0._1; }
  $runtime.fail();
};
export {unsafeRegex};
