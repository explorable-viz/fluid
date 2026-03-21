import * as Control$dBind from "../Control.Bind/index.js";
import * as Data$dCodePoint$dUnicode$dInternal from "../Data.CodePoint.Unicode.Internal/index.js";
import * as Data$dCodePoint$dUnicode$dInternal$dCasing from "../Data.CodePoint.Unicode.Internal.Casing/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dString$dCodePoints from "../Data.String.CodePoints/index.js";
const convertFull = f => x => Data$dString$dCodePoints.fromCodePointArray(Control$dBind.arrayBind(Data$dString$dCodePoints.toCodePointArray(x))(f));
const toLower = /* #__PURE__ */ convertFull(Data$dCodePoint$dUnicode$dInternal$dCasing.lower);
const toUpper = /* #__PURE__ */ convertFull(Data$dCodePoint$dUnicode$dInternal$dCasing.upper);
const convert = f => {
  const $0 = Data$dFunctor.arrayMap(f);
  return x => Data$dString$dCodePoints.fromCodePointArray($0(Data$dString$dCodePoints.toCodePointArray(x)));
};
const toLowerSimple = /* #__PURE__ */ convert(Data$dCodePoint$dUnicode$dInternal.uTowlower);
const toUpperSimple = /* #__PURE__ */ convert(Data$dCodePoint$dUnicode$dInternal.uTowupper);
const caseFoldSimple = /* #__PURE__ */ convert(Data$dCodePoint$dUnicode$dInternal$dCasing.fold);
const caseFold = /* #__PURE__ */ convertFull(Data$dCodePoint$dUnicode$dInternal$dCasing.foldFull);
const caselessMatch = s1 => s2 => Data$dString$dCodePoints.fromCodePointArray(Control$dBind.arrayBind(Data$dString$dCodePoints.toCodePointArray(s1))(Data$dCodePoint$dUnicode$dInternal$dCasing.foldFull)) === Data$dString$dCodePoints.fromCodePointArray(Control$dBind.arrayBind(Data$dString$dCodePoints.toCodePointArray(s2))(Data$dCodePoint$dUnicode$dInternal$dCasing.foldFull));
export {caseFold, caseFoldSimple, caselessMatch, convert, convertFull, toLower, toLowerSimple, toUpper, toUpperSimple};
