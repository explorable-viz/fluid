import * as $runtime from "../runtime.js";
import * as Data$dCodePoint$dUnicode$dInternal from "../Data.CodePoint.Unicode.Internal/index.js";
import * as Data$dCodePoint$dUnicode$dInternal$dCasing from "../Data.CodePoint.Unicode.Internal.Casing/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
const $GeneralCategory = tag => tag;
const UppercaseLetter = /* #__PURE__ */ $GeneralCategory("UppercaseLetter");
const LowercaseLetter = /* #__PURE__ */ $GeneralCategory("LowercaseLetter");
const TitlecaseLetter = /* #__PURE__ */ $GeneralCategory("TitlecaseLetter");
const ModifierLetter = /* #__PURE__ */ $GeneralCategory("ModifierLetter");
const OtherLetter = /* #__PURE__ */ $GeneralCategory("OtherLetter");
const NonSpacingMark = /* #__PURE__ */ $GeneralCategory("NonSpacingMark");
const SpacingCombiningMark = /* #__PURE__ */ $GeneralCategory("SpacingCombiningMark");
const EnclosingMark = /* #__PURE__ */ $GeneralCategory("EnclosingMark");
const DecimalNumber = /* #__PURE__ */ $GeneralCategory("DecimalNumber");
const LetterNumber = /* #__PURE__ */ $GeneralCategory("LetterNumber");
const OtherNumber = /* #__PURE__ */ $GeneralCategory("OtherNumber");
const ConnectorPunctuation = /* #__PURE__ */ $GeneralCategory("ConnectorPunctuation");
const DashPunctuation = /* #__PURE__ */ $GeneralCategory("DashPunctuation");
const OpenPunctuation = /* #__PURE__ */ $GeneralCategory("OpenPunctuation");
const ClosePunctuation = /* #__PURE__ */ $GeneralCategory("ClosePunctuation");
const InitialQuote = /* #__PURE__ */ $GeneralCategory("InitialQuote");
const FinalQuote = /* #__PURE__ */ $GeneralCategory("FinalQuote");
const OtherPunctuation = /* #__PURE__ */ $GeneralCategory("OtherPunctuation");
const MathSymbol = /* #__PURE__ */ $GeneralCategory("MathSymbol");
const CurrencySymbol = /* #__PURE__ */ $GeneralCategory("CurrencySymbol");
const ModifierSymbol = /* #__PURE__ */ $GeneralCategory("ModifierSymbol");
const OtherSymbol = /* #__PURE__ */ $GeneralCategory("OtherSymbol");
const Space = /* #__PURE__ */ $GeneralCategory("Space");
const LineSeparator = /* #__PURE__ */ $GeneralCategory("LineSeparator");
const ParagraphSeparator = /* #__PURE__ */ $GeneralCategory("ParagraphSeparator");
const Control = /* #__PURE__ */ $GeneralCategory("Control");
const Format = /* #__PURE__ */ $GeneralCategory("Format");
const Surrogate = /* #__PURE__ */ $GeneralCategory("Surrogate");
const PrivateUse = /* #__PURE__ */ $GeneralCategory("PrivateUse");
const NotAssigned = /* #__PURE__ */ $GeneralCategory("NotAssigned");
const unicodeCatToGeneralCat = v => {
  if (v === "NUMCAT_LU") { return UppercaseLetter; }
  if (v === "NUMCAT_LL") { return LowercaseLetter; }
  if (v === "NUMCAT_LT") { return TitlecaseLetter; }
  if (v === "NUMCAT_LM") { return ModifierLetter; }
  if (v === "NUMCAT_LO") { return OtherLetter; }
  if (v === "NUMCAT_MN") { return NonSpacingMark; }
  if (v === "NUMCAT_MC") { return SpacingCombiningMark; }
  if (v === "NUMCAT_ME") { return EnclosingMark; }
  if (v === "NUMCAT_ND") { return DecimalNumber; }
  if (v === "NUMCAT_NL") { return LetterNumber; }
  if (v === "NUMCAT_NO") { return OtherNumber; }
  if (v === "NUMCAT_PC") { return ConnectorPunctuation; }
  if (v === "NUMCAT_PD") { return DashPunctuation; }
  if (v === "NUMCAT_PS") { return OpenPunctuation; }
  if (v === "NUMCAT_PE") { return ClosePunctuation; }
  if (v === "NUMCAT_PI") { return InitialQuote; }
  if (v === "NUMCAT_PF") { return FinalQuote; }
  if (v === "NUMCAT_PO") { return OtherPunctuation; }
  if (v === "NUMCAT_SM") { return MathSymbol; }
  if (v === "NUMCAT_SC") { return CurrencySymbol; }
  if (v === "NUMCAT_SK") { return ModifierSymbol; }
  if (v === "NUMCAT_SO") { return OtherSymbol; }
  if (v === "NUMCAT_ZS") { return Space; }
  if (v === "NUMCAT_ZL") { return LineSeparator; }
  if (v === "NUMCAT_ZP") { return ParagraphSeparator; }
  if (v === "NUMCAT_CC") { return Control; }
  if (v === "NUMCAT_CF") { return Format; }
  if (v === "NUMCAT_CS") { return Surrogate; }
  if (v === "NUMCAT_CO") { return PrivateUse; }
  if (v === "NUMCAT_CN") { return NotAssigned; }
  $runtime.fail();
};
const showGeneralCategory = {
  show: v => {
    if (v === "UppercaseLetter") { return "UppercaseLetter"; }
    if (v === "LowercaseLetter") { return "LowercaseLetter"; }
    if (v === "TitlecaseLetter") { return "TitlecaseLetter"; }
    if (v === "ModifierLetter") { return "ModifierLetter"; }
    if (v === "OtherLetter") { return "OtherLetter"; }
    if (v === "NonSpacingMark") { return "NonSpacingMark"; }
    if (v === "SpacingCombiningMark") { return "SpacingCombiningMark"; }
    if (v === "EnclosingMark") { return "EnclosingMark"; }
    if (v === "DecimalNumber") { return "DecimalNumber"; }
    if (v === "LetterNumber") { return "LetterNumber"; }
    if (v === "OtherNumber") { return "OtherNumber"; }
    if (v === "ConnectorPunctuation") { return "ConnectorPunctuation"; }
    if (v === "DashPunctuation") { return "DashPunctuation"; }
    if (v === "OpenPunctuation") { return "OpenPunctuation"; }
    if (v === "ClosePunctuation") { return "ClosePunctuation"; }
    if (v === "InitialQuote") { return "InitialQuote"; }
    if (v === "FinalQuote") { return "FinalQuote"; }
    if (v === "OtherPunctuation") { return "OtherPunctuation"; }
    if (v === "MathSymbol") { return "MathSymbol"; }
    if (v === "CurrencySymbol") { return "CurrencySymbol"; }
    if (v === "ModifierSymbol") { return "ModifierSymbol"; }
    if (v === "OtherSymbol") { return "OtherSymbol"; }
    if (v === "Space") { return "Space"; }
    if (v === "LineSeparator") { return "LineSeparator"; }
    if (v === "ParagraphSeparator") { return "ParagraphSeparator"; }
    if (v === "Control") { return "Control"; }
    if (v === "Format") { return "Format"; }
    if (v === "Surrogate") { return "Surrogate"; }
    if (v === "PrivateUse") { return "PrivateUse"; }
    if (v === "NotAssigned") { return "NotAssigned"; }
    $runtime.fail();
  }
};
const toLower = Data$dCodePoint$dUnicode$dInternal$dCasing.lower;
const toTitle = Data$dCodePoint$dUnicode$dInternal$dCasing.title;
const toUpper = Data$dCodePoint$dUnicode$dInternal$dCasing.upper;
const toLowerSimple = Data$dCodePoint$dUnicode$dInternal.uTowlower;
const toTitleSimple = Data$dCodePoint$dUnicode$dInternal.uTowtitle;
const toUpperSimple = Data$dCodePoint$dUnicode$dInternal.uTowupper;
const isUpper = x => Data$dCodePoint$dUnicode$dInternal.checkAttr([512, 524288])(x);
const isSpace = c => {
  if (c <= 823) { return c === 32 || c >= 9 && c <= 13 || c === 160; }
  return Data$dCodePoint$dUnicode$dInternal.checkAttrS([2])(c);
};
const isPrint = x => Data$dCodePoint$dUnicode$dInternal.uIswprint(x);
const isOctDigit = c => {
  const diff = c - 48 | 0;
  return diff <= 7 && diff >= 0;
};
const octDigitToInt = c => {
  if (
    (() => {
      const diff = c - 48 | 0;
      return diff <= 7 && diff >= 0;
    })()
  ) {
    return Data$dMaybe.$Maybe("Just", c - 48 | 0);
  }
  return Data$dMaybe.Nothing;
};
const isLower = x => Data$dCodePoint$dUnicode$dInternal.checkAttr([4096])(x);
const isLatin1 = c => c <= 255;
const isDecDigit = c => {
  const diff = c - 48 | 0;
  return diff <= 9 && diff >= 0;
};
const isDigit = () => isDecDigit;
const isHexDigit = c => {
  const diff = c - 48 | 0;
  const diff$1 = c - 65 | 0;
  return diff <= 9 && diff >= 0 || (() => {
    const diff$2 = c - 97 | 0;
    return diff$1 <= 5 && diff$1 >= 0 || diff$2 <= 5 && diff$2 >= 0;
  })();
};
const isControl = x => Data$dCodePoint$dUnicode$dInternal.checkAttr([1])(x);
const isAsciiUpper = c => c >= 65 && c <= 90;
const isAsciiLower = c => c >= 97 && c <= 122;
const isAscii = c => c < 128;
const isAlphaNum = x => Data$dCodePoint$dUnicode$dInternal.checkAttr([524288, 512, 4096, 1048576, 16384, 8388608, 4194304, 2097152, 131072, 256, 16777216])(x);
const isAlpha = x => Data$dCodePoint$dUnicode$dInternal.checkAttr([4096, 512, 524288, 1048576, 16384])(x);
const hexDigitToInt = c => {
  const hexUpper = c - 65 | 0;
  const hexLower = c - 97 | 0;
  const dec = c - 48 | 0;
  if (dec <= 9 && dec >= 0) { return Data$dMaybe.$Maybe("Just", dec); }
  if (hexLower <= 5 && hexLower >= 0) { return Data$dMaybe.$Maybe("Just", hexLower + 10 | 0); }
  if (hexUpper <= 5 && hexUpper >= 0) { return Data$dMaybe.$Maybe("Just", hexUpper + 10 | 0); }
  return Data$dMaybe.Nothing;
};
const generalCategory = x => {
  const $0 = Data$dCodePoint$dUnicode$dInternal.getRule(Data$dCodePoint$dUnicode$dInternal.allchars)(x)(3396);
  if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", unicodeCatToGeneralCat($0._1.unicodeCat)); }
  return Data$dMaybe.Nothing;
};
const isLetter = c => {
  const v = generalCategory(c);
  return v.tag === "Just" && (v._1 === "UppercaseLetter" || v._1 === "LowercaseLetter" || v._1 === "TitlecaseLetter" || v._1 === "ModifierLetter" || v._1 === "OtherLetter");
};
const isMark = c => {
  const v = generalCategory(c);
  return v.tag === "Just" && (v._1 === "NonSpacingMark" || v._1 === "SpacingCombiningMark" || v._1 === "EnclosingMark");
};
const isNumber = c => {
  const v = generalCategory(c);
  return v.tag === "Just" && (v._1 === "DecimalNumber" || v._1 === "LetterNumber" || v._1 === "OtherNumber");
};
const isPunctuation = c => {
  const v = generalCategory(c);
  return v.tag === "Just" && (
    v._1 === "ConnectorPunctuation" || v._1 === "DashPunctuation" || v._1 === "OpenPunctuation" || v._1 === "ClosePunctuation" || v._1 === "InitialQuote" || v._1 === "FinalQuote" || v._1 === "OtherPunctuation"
  );
};
const isSeparator = c => {
  const v = generalCategory(c);
  return v.tag === "Just" && (v._1 === "Space" || v._1 === "LineSeparator" || v._1 === "ParagraphSeparator");
};
const isSymbol = c => {
  const v = generalCategory(c);
  return v.tag === "Just" && (v._1 === "MathSymbol" || v._1 === "CurrencySymbol" || v._1 === "ModifierSymbol" || v._1 === "OtherSymbol");
};
const generalCatToUnicodeCat = v => {
  if (v === "UppercaseLetter") { return Data$dCodePoint$dUnicode$dInternal.NUMCAT_LU; }
  if (v === "LowercaseLetter") { return Data$dCodePoint$dUnicode$dInternal.NUMCAT_LL; }
  if (v === "TitlecaseLetter") { return Data$dCodePoint$dUnicode$dInternal.NUMCAT_LT; }
  if (v === "ModifierLetter") { return Data$dCodePoint$dUnicode$dInternal.NUMCAT_LM; }
  if (v === "OtherLetter") { return Data$dCodePoint$dUnicode$dInternal.NUMCAT_LO; }
  if (v === "NonSpacingMark") { return Data$dCodePoint$dUnicode$dInternal.NUMCAT_MN; }
  if (v === "SpacingCombiningMark") { return Data$dCodePoint$dUnicode$dInternal.NUMCAT_MC; }
  if (v === "EnclosingMark") { return Data$dCodePoint$dUnicode$dInternal.NUMCAT_ME; }
  if (v === "DecimalNumber") { return Data$dCodePoint$dUnicode$dInternal.NUMCAT_ND; }
  if (v === "LetterNumber") { return Data$dCodePoint$dUnicode$dInternal.NUMCAT_NL; }
  if (v === "OtherNumber") { return Data$dCodePoint$dUnicode$dInternal.NUMCAT_NO; }
  if (v === "ConnectorPunctuation") { return Data$dCodePoint$dUnicode$dInternal.NUMCAT_PC; }
  if (v === "DashPunctuation") { return Data$dCodePoint$dUnicode$dInternal.NUMCAT_PD; }
  if (v === "OpenPunctuation") { return Data$dCodePoint$dUnicode$dInternal.NUMCAT_PS; }
  if (v === "ClosePunctuation") { return Data$dCodePoint$dUnicode$dInternal.NUMCAT_PE; }
  if (v === "InitialQuote") { return Data$dCodePoint$dUnicode$dInternal.NUMCAT_PI; }
  if (v === "FinalQuote") { return Data$dCodePoint$dUnicode$dInternal.NUMCAT_PF; }
  if (v === "OtherPunctuation") { return Data$dCodePoint$dUnicode$dInternal.NUMCAT_PO; }
  if (v === "MathSymbol") { return Data$dCodePoint$dUnicode$dInternal.NUMCAT_SM; }
  if (v === "CurrencySymbol") { return Data$dCodePoint$dUnicode$dInternal.NUMCAT_SC; }
  if (v === "ModifierSymbol") { return Data$dCodePoint$dUnicode$dInternal.NUMCAT_SK; }
  if (v === "OtherSymbol") { return Data$dCodePoint$dUnicode$dInternal.NUMCAT_SO; }
  if (v === "Space") { return Data$dCodePoint$dUnicode$dInternal.NUMCAT_ZS; }
  if (v === "LineSeparator") { return Data$dCodePoint$dUnicode$dInternal.NUMCAT_ZL; }
  if (v === "ParagraphSeparator") { return Data$dCodePoint$dUnicode$dInternal.NUMCAT_ZP; }
  if (v === "Control") { return Data$dCodePoint$dUnicode$dInternal.NUMCAT_CC; }
  if (v === "Format") { return Data$dCodePoint$dUnicode$dInternal.NUMCAT_CF; }
  if (v === "Surrogate") { return Data$dCodePoint$dUnicode$dInternal.NUMCAT_CS; }
  if (v === "PrivateUse") { return Data$dCodePoint$dUnicode$dInternal.NUMCAT_CO; }
  if (v === "NotAssigned") { return Data$dCodePoint$dUnicode$dInternal.NUMCAT_CN; }
  $runtime.fail();
};
const generalCatToInt = v => {
  if (v === "UppercaseLetter") { return 1; }
  if (v === "LowercaseLetter") { return 2; }
  if (v === "TitlecaseLetter") { return 3; }
  if (v === "ModifierLetter") { return 4; }
  if (v === "OtherLetter") { return 5; }
  if (v === "NonSpacingMark") { return 6; }
  if (v === "SpacingCombiningMark") { return 7; }
  if (v === "EnclosingMark") { return 8; }
  if (v === "DecimalNumber") { return 9; }
  if (v === "LetterNumber") { return 10; }
  if (v === "OtherNumber") { return 11; }
  if (v === "ConnectorPunctuation") { return 12; }
  if (v === "DashPunctuation") { return 13; }
  if (v === "OpenPunctuation") { return 14; }
  if (v === "ClosePunctuation") { return 15; }
  if (v === "InitialQuote") { return 16; }
  if (v === "FinalQuote") { return 17; }
  if (v === "OtherPunctuation") { return 18; }
  if (v === "MathSymbol") { return 19; }
  if (v === "CurrencySymbol") { return 20; }
  if (v === "ModifierSymbol") { return 21; }
  if (v === "OtherSymbol") { return 22; }
  if (v === "Space") { return 23; }
  if (v === "LineSeparator") { return 24; }
  if (v === "ParagraphSeparator") { return 25; }
  if (v === "Control") { return 26; }
  if (v === "Format") { return 27; }
  if (v === "Surrogate") { return 28; }
  if (v === "PrivateUse") { return 29; }
  if (v === "NotAssigned") { return 30; }
  $runtime.fail();
};
const eqGeneralCategory = {
  eq: v => v1 => {
    if (v === "UppercaseLetter") { return v1 === "UppercaseLetter"; }
    if (v === "LowercaseLetter") { return v1 === "LowercaseLetter"; }
    if (v === "TitlecaseLetter") { return v1 === "TitlecaseLetter"; }
    if (v === "ModifierLetter") { return v1 === "ModifierLetter"; }
    if (v === "OtherLetter") { return v1 === "OtherLetter"; }
    if (v === "NonSpacingMark") { return v1 === "NonSpacingMark"; }
    if (v === "SpacingCombiningMark") { return v1 === "SpacingCombiningMark"; }
    if (v === "EnclosingMark") { return v1 === "EnclosingMark"; }
    if (v === "DecimalNumber") { return v1 === "DecimalNumber"; }
    if (v === "LetterNumber") { return v1 === "LetterNumber"; }
    if (v === "OtherNumber") { return v1 === "OtherNumber"; }
    if (v === "ConnectorPunctuation") { return v1 === "ConnectorPunctuation"; }
    if (v === "DashPunctuation") { return v1 === "DashPunctuation"; }
    if (v === "OpenPunctuation") { return v1 === "OpenPunctuation"; }
    if (v === "ClosePunctuation") { return v1 === "ClosePunctuation"; }
    if (v === "InitialQuote") { return v1 === "InitialQuote"; }
    if (v === "FinalQuote") { return v1 === "FinalQuote"; }
    if (v === "OtherPunctuation") { return v1 === "OtherPunctuation"; }
    if (v === "MathSymbol") { return v1 === "MathSymbol"; }
    if (v === "CurrencySymbol") { return v1 === "CurrencySymbol"; }
    if (v === "ModifierSymbol") { return v1 === "ModifierSymbol"; }
    if (v === "OtherSymbol") { return v1 === "OtherSymbol"; }
    if (v === "Space") { return v1 === "Space"; }
    if (v === "LineSeparator") { return v1 === "LineSeparator"; }
    if (v === "ParagraphSeparator") { return v1 === "ParagraphSeparator"; }
    if (v === "Control") { return v1 === "Control"; }
    if (v === "Format") { return v1 === "Format"; }
    if (v === "Surrogate") { return v1 === "Surrogate"; }
    if (v === "PrivateUse") { return v1 === "PrivateUse"; }
    return v === "NotAssigned" && v1 === "NotAssigned";
  }
};
const ordGeneralCategory = {compare: catA => catB => Data$dOrd.ordInt.compare(generalCatToInt(catA))(generalCatToInt(catB)), Eq0: () => eqGeneralCategory};
const digitToInt = () => hexDigitToInt;
const decDigitToInt = c => {
  if (
    (() => {
      const diff = c - 48 | 0;
      return diff <= 9 && diff >= 0;
    })()
  ) {
    return Data$dMaybe.$Maybe("Just", c - 48 | 0);
  }
  return Data$dMaybe.Nothing;
};
const caseFoldSimple = Data$dCodePoint$dUnicode$dInternal$dCasing.fold;
const caseFold = Data$dCodePoint$dUnicode$dInternal$dCasing.foldFull;
const boundedGeneralCategory = {bottom: UppercaseLetter, top: NotAssigned, Ord0: () => ordGeneralCategory};
export {
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  isAlpha,
  isAlphaNum,
  
  
  
  
  isDecDigit,
  
  isHexDigit,
  
  
  isLower,
  
  
  isOctDigit,
  
  
  
  isSpace,
  
  isUpper,
  
  
  
  
  
  
  
  
  
  
};
