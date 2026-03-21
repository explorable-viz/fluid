import * as $runtime from "../runtime.js";
import * as Data$dFunction from "../Data.Function/index.js";
import * as Data$dString$dCodePoints from "../Data.String.CodePoints/index.js";
import * as Data$dString$dCommon from "../Data.String.Common/index.js";
import * as Data$dString$dRegex from "../Data.String.Regex/index.js";
import * as Data$dString$dRegex$dFlags from "../Data.String.Regex.Flags/index.js";
import * as Partial from "../Partial/index.js";
const whitespaceRegex = /* #__PURE__ */ (() => {
  const v = Data$dString$dRegex.regex("\\s+")(Data$dString$dRegex$dFlags.noFlags);
  if (v.tag === "Left") { return Partial._crashWith("whitespaceRegex: `\\s+` seems to be invlaid, err: " + v._1); }
  if (v.tag === "Right") { return v._1; }
  $runtime.fail();
})();
const words = v => {
  if (v === "") { return []; }
  return Data$dString$dRegex.split(whitespaceRegex)(v);
};
const unWords = dictFoldable => xs => dictFoldable.foldl(v => v1 => {
  if (v.init) { return {init: false, acc: v1}; }
  return {init: false, acc: v.acc + " " + v1};
})({init: true, acc: ""})(xs).acc;
const unLines = dictFoldable => xs => dictFoldable.foldl(v => v1 => {
  if (v.init) { return {init: false, acc: v1}; }
  return {init: false, acc: v.acc + "\n" + v1};
})({init: true, acc: ""})(xs).acc;
const startsWith = p => s => {
  const $0 = Data$dString$dCodePoints.indexOf(p)(s);
  if ($0.tag === "Nothing") { return false; }
  return $0.tag === "Just" && $0._1 === 0;
};
const lines = v => {
  if (v === "") { return []; }
  return Data$dString$dCommon.split("\n")(v);
};
const apApplyFlipped = dictApply => a => b => dictApply.apply(dictApply.Functor0().map(Data$dFunction.applyFlipped)(a))(b);
export {apApplyFlipped, lines, startsWith, unLines, unWords, whitespaceRegex, words};
