import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dString$dCommon from "../Data.String.Common/index.js";
const eq = /* #__PURE__ */ Data$dEq.eqArrayImpl(Data$dEq.eqStringImpl);
const RegexFlags = x => x;
const unicode = {global: false, ignoreCase: false, multiline: false, dotAll: false, sticky: false, unicode: true};
const sticky = {global: false, ignoreCase: false, multiline: false, dotAll: false, sticky: true, unicode: false};
const showRegexFlags = {
  show: v => {
    const usedFlags = [
      ...Data$dFunctor.arrayMap(v$1 => "global")(v.global ? [undefined] : []),
      ...Data$dFunctor.arrayMap(v$1 => "ignoreCase")(v.ignoreCase ? [undefined] : []),
      ...Data$dFunctor.arrayMap(v$1 => "multiline")(v.multiline ? [undefined] : []),
      ...Data$dFunctor.arrayMap(v$1 => "dotAll")(v.dotAll ? [undefined] : []),
      ...Data$dFunctor.arrayMap(v$1 => "sticky")(v.sticky ? [undefined] : []),
      ...Data$dFunctor.arrayMap(v$1 => "unicode")(v.unicode ? [undefined] : [])
    ];
    if (eq(usedFlags)([])) { return "noFlags"; }
    return "(" + Data$dString$dCommon.joinWith(" <> ")(usedFlags) + ")";
  }
};
const semigroupRegexFlags = {
  append: v => v1 => (
    {
      global: v.global || v1.global,
      ignoreCase: v.ignoreCase || v1.ignoreCase,
      multiline: v.multiline || v1.multiline,
      dotAll: v.dotAll || v1.dotAll,
      sticky: v.sticky || v1.sticky,
      unicode: v.unicode || v1.unicode
    }
  )
};
const noFlags = {global: false, ignoreCase: false, multiline: false, dotAll: false, sticky: false, unicode: false};
const newtypeRegexFlags = {Coercible0: () => {}};
const multiline = {global: false, ignoreCase: false, multiline: true, dotAll: false, sticky: false, unicode: false};
const monoidRegexFlags = {mempty: noFlags, Semigroup0: () => semigroupRegexFlags};
const ignoreCase = {global: false, ignoreCase: true, multiline: false, dotAll: false, sticky: false, unicode: false};
const global = {global: true, ignoreCase: false, multiline: false, dotAll: false, sticky: false, unicode: false};
const eqRegexFlags = {
  eq: ra => rb => ra.dotAll === rb.dotAll && ra.global === rb.global && ra.ignoreCase === rb.ignoreCase && ra.multiline === rb.multiline && ra.sticky === rb.sticky && ra.unicode === rb.unicode
};
const dotAll = {global: false, ignoreCase: false, multiline: false, dotAll: true, sticky: false, unicode: false};
export {         noFlags,    };
