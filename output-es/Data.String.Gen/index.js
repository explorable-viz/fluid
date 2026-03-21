import * as $runtime from "../runtime.js";
import * as Control$dMonad$dGen from "../Control.Monad.Gen/index.js";
import * as Data$dChar$dGen from "../Data.Char.Gen/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dString$dCodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data$dUnfoldable from "../Data.Unfoldable/index.js";
const max = x => y => {
  const v = Data$dOrd.ordInt.compare(x)(y);
  if (v === "LT") { return y; }
  if (v === "EQ") { return x; }
  if (v === "GT") { return x; }
  $runtime.fail();
};
const genString = dictMonadRec => dictMonadGen => {
  const Bind1 = dictMonadGen.Monad0().Bind1();
  const unfoldable1 = Control$dMonad$dGen.unfoldable(dictMonadRec)(dictMonadGen)(Data$dUnfoldable.unfoldableArray);
  return genChar => dictMonadGen.sized(size => Bind1.bind(dictMonadGen.chooseInt(1)(max(1)(size)))(newSize => dictMonadGen.resize(v => newSize)(Bind1.Apply0().Functor0().map(Data$dString$dCodeUnits.fromCharArray)(unfoldable1(genChar)))));
};
const genUnicodeString = dictMonadRec => dictMonadGen => genString(dictMonadRec)(dictMonadGen)(Data$dChar$dGen.genUnicodeChar(dictMonadGen));
const genDigitString = dictMonadRec => dictMonadGen => genString(dictMonadRec)(dictMonadGen)(Data$dChar$dGen.genDigitChar(dictMonadGen));
const genAsciiString$p = dictMonadRec => dictMonadGen => genString(dictMonadRec)(dictMonadGen)(Data$dChar$dGen.genAsciiChar$p(dictMonadGen));
const genAsciiString = dictMonadRec => dictMonadGen => genString(dictMonadRec)(dictMonadGen)(Data$dChar$dGen.genAsciiChar(dictMonadGen));
const genAlphaUppercaseString = dictMonadRec => dictMonadGen => genString(dictMonadRec)(dictMonadGen)(Data$dChar$dGen.genAlphaUppercase(dictMonadGen));
const genAlphaString = dictMonadRec => dictMonadGen => genString(dictMonadRec)(dictMonadGen)(Data$dChar$dGen.genAlpha(dictMonadGen));
const genAlphaLowercaseString = dictMonadRec => dictMonadGen => genString(dictMonadRec)(dictMonadGen)(Data$dChar$dGen.genAlphaLowercase(dictMonadGen));
export {genAlphaLowercaseString, genAlphaString, genAlphaUppercaseString, genAsciiString, genAsciiString$p, genDigitString, genString, genUnicodeString, max};
