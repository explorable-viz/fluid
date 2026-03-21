import * as Control$dMonad$dGen from "../Control.Monad.Gen/index.js";
import * as Data$dEnum from "../Data.Enum/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dNonEmpty from "../Data.NonEmpty/index.js";
const toEnumWithDefaults = low => high => x => {
  if (x >= 0 && x <= 65535) { return Data$dEnum.fromCharCode(x); }
  if (x < 0) { return low; }
  return high;
};
const foldable1NonEmpty = /* #__PURE__ */ Data$dNonEmpty.foldable1NonEmpty(Data$dFoldable.foldableArray);
const genUnicodeChar = dictMonadGen => dictMonadGen.Monad0().Bind1().Apply0().Functor0().map(toEnumWithDefaults("\u0000")("￿"))(dictMonadGen.chooseInt(0)(65536));
const genDigitChar = dictMonadGen => dictMonadGen.Monad0().Bind1().Apply0().Functor0().map(toEnumWithDefaults("\u0000")("￿"))(dictMonadGen.chooseInt(48)(57));
const genAsciiChar$p = dictMonadGen => dictMonadGen.Monad0().Bind1().Apply0().Functor0().map(toEnumWithDefaults("\u0000")("￿"))(dictMonadGen.chooseInt(0)(127));
const genAsciiChar = dictMonadGen => dictMonadGen.Monad0().Bind1().Apply0().Functor0().map(toEnumWithDefaults("\u0000")("￿"))(dictMonadGen.chooseInt(32)(127));
const genAlphaUppercase = dictMonadGen => dictMonadGen.Monad0().Bind1().Apply0().Functor0().map(toEnumWithDefaults("\u0000")("￿"))(dictMonadGen.chooseInt(65)(90));
const genAlphaLowercase = dictMonadGen => dictMonadGen.Monad0().Bind1().Apply0().Functor0().map(toEnumWithDefaults("\u0000")("￿"))(dictMonadGen.chooseInt(97)(122));
const genAlpha = dictMonadGen => Control$dMonad$dGen.oneOf(dictMonadGen)(foldable1NonEmpty)(Data$dNonEmpty.$NonEmpty(
  genAlphaLowercase(dictMonadGen),
  [genAlphaUppercase(dictMonadGen)]
));
export {foldable1NonEmpty, genAlpha, genAlphaLowercase, genAlphaUppercase, genAsciiChar, genAsciiChar$p, genDigitChar, genUnicodeChar, toEnumWithDefaults};
