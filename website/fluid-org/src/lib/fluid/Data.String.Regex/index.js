// | Wraps Javascript's `RegExp` object that enables matching strings with
// | patterns defined by regular expressions.
// | For details of the underlying implementation, see [RegExp Reference at MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp).
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dString$dCodeUnits from "../Data.String.CodeUnits/index.js";
import {_match, _replaceBy, _search, flagsImpl, regexImpl, replace, showRegexImpl, source, split, test} from "./foreign.js";
const showRegex = {show: showRegexImpl};
const search = /* #__PURE__ */ _search(Data$dMaybe.Just)(Data$dMaybe.Nothing);
const replace$p = /* #__PURE__ */ _replaceBy(Data$dMaybe.Just)(Data$dMaybe.Nothing);
const renderFlags = v => (v.global ? "g" : "") + (v.ignoreCase ? "i" : "") + (v.multiline ? "m" : "") + (v.dotAll ? "s" : "") + (v.sticky ? "y" : "") + (v.unicode ? "u" : "");
const regex = s => f => regexImpl(Data$dEither.Left)(Data$dEither.Right)(s)((f.global ? "g" : "") + (f.ignoreCase ? "i" : "") + (f.multiline ? "m" : "") + (f.dotAll ? "s" : "") + (f.sticky
  ? "y"
  : "") + (f.unicode ? "u" : ""));
const parseFlags = s => (
  {
    global: Data$dString$dCodeUnits.contains("g")(s),
    ignoreCase: Data$dString$dCodeUnits.contains("i")(s),
    multiline: Data$dString$dCodeUnits.contains("m")(s),
    dotAll: Data$dString$dCodeUnits.contains("s")(s),
    sticky: Data$dString$dCodeUnits.contains("y")(s),
    unicode: Data$dString$dCodeUnits.contains("u")(s)
  }
);
const match = /* #__PURE__ */ _match(Data$dMaybe.Just)(Data$dMaybe.Nothing);
const flags = x => flagsImpl(x);
export { match,  regex,   search, };
export * from "./foreign.js";
