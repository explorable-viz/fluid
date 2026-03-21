// * Parser builders
//
// | This module contains utility functions and combinators to create parsers
// | for individual options.
// |
// | Each parser builder takes an option modifier. A modifier can be created by
// | composing the basic modifiers provided by this module using the 'Monoid'
// | operations 'mempty' and 'append', or their aliases 'idm' and '<>'.
// |
// | For example:
// |
// | ```purescript
// | out = strOption
// |     ( long "output"
// |    <> short 'o'
// |    <> metavar "FILENAME" )
// | ```
// |
// | creates a parser for an option called `output`.
import * as $runtime from "../runtime.js";
import * as Control$dMonad$dExcept$dTrans from "../Control.Monad.Except.Trans/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dIdentity from "../Data.Identity/index.js";
import * as Data$dInt from "../Data.Int/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNumber from "../Data.Number/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dString$dCommon from "../Data.String.Common/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as ExitCodes from "../ExitCodes/index.js";
import * as Options$dApplicative$dBuilder$dCompleter from "../Options.Applicative.Builder.Completer/index.js";
import * as Options$dApplicative$dBuilder$dInternal from "../Options.Applicative.Builder.Internal/index.js";
import * as Options$dApplicative$dHelp$dChunk from "../Options.Applicative.Help.Chunk/index.js";
import * as Options$dApplicative$dTypes from "../Options.Applicative.Types/index.js";
const identity = x => x;
const mempty2 = /* #__PURE__ */ (() => ({argCompleter: Options$dApplicative$dTypes.completerMonoid.mempty}))();
const fold = /* #__PURE__ */ (() => Data$dFoldable.foldableArray.foldMap(Options$dApplicative$dBuilder$dInternal.modMonoid)(Data$dFoldable.identity))();
const PrefsMod = x => x;
const InfoMod = x => x;
const value = dictHasValue => x => Options$dApplicative$dBuilder$dInternal.$Mod(
  identity,
  Options$dApplicative$dBuilder$dInternal.$DefaultProp(Data$dMaybe.$Maybe("Just", x), Data$dMaybe.Nothing),
  identity
);
const subparserInline = p => (
  {
    prefBacktrack: Options$dApplicative$dTypes.SubparserInline,
    prefColumns: p.prefColumns,
    prefDisambiguate: p.prefDisambiguate,
    prefMultiSuffix: p.prefMultiSuffix,
    prefShowHelpOnEmpty: p.prefShowHelpOnEmpty,
    prefShowHelpOnError: p.prefShowHelpOnError
  }
);
const style = x => Options$dApplicative$dBuilder$dInternal.optionMod(p => (
  {propDescMod: Data$dMaybe.$Maybe("Just", x), propHelp: p.propHelp, propMetaVar: p.propMetaVar, propShowDefault: p.propShowDefault, propVisibility: p.propVisibility}
));
const str = Options$dApplicative$dTypes.readerAsk;
const showHelpOnError = p => (
  {
    prefShowHelpOnError: true,
    prefBacktrack: p.prefBacktrack,
    prefColumns: p.prefColumns,
    prefDisambiguate: p.prefDisambiguate,
    prefMultiSuffix: p.prefMultiSuffix,
    prefShowHelpOnEmpty: p.prefShowHelpOnEmpty
  }
);
const showHelpOnEmpty = p => (
  {
    prefShowHelpOnEmpty: true,
    prefBacktrack: p.prefBacktrack,
    prefColumns: p.prefColumns,
    prefDisambiguate: p.prefDisambiguate,
    prefMultiSuffix: p.prefMultiSuffix,
    prefShowHelpOnError: p.prefShowHelpOnError
  }
);
const showDefaultWith = s => Options$dApplicative$dBuilder$dInternal.$Mod(
  identity,
  Options$dApplicative$dBuilder$dInternal.$DefaultProp(Data$dMaybe.Nothing, Data$dMaybe.$Maybe("Just", s)),
  identity
);
const showDefault = dictShow => Options$dApplicative$dBuilder$dInternal.$Mod(
  identity,
  Options$dApplicative$dBuilder$dInternal.$DefaultProp(Data$dMaybe.Nothing, Data$dMaybe.$Maybe("Just", dictShow.show)),
  identity
);
const $$short = dictHasName => x => Options$dApplicative$dBuilder$dInternal.$Mod(
  dictHasName.name(Options$dApplicative$dTypes.$OptName("OptShort", x)),
  Options$dApplicative$dBuilder$dInternal.$DefaultProp(Data$dMaybe.Nothing, Data$dMaybe.Nothing),
  Options$dApplicative$dBuilder$dInternal.identity
);
const progDescDoc = doc => i => (
  {
    infoProgDesc: doc,
    infoFailureCode: i.infoFailureCode,
    infoFooter: i.infoFooter,
    infoFullDesc: i.infoFullDesc,
    infoHeader: i.infoHeader,
    infoParser: i.infoParser,
    infoPolicy: i.infoPolicy
  }
);
const progDesc = s => i => (
  {
    infoProgDesc: Options$dApplicative$dHelp$dChunk.paragraph(s),
    infoFailureCode: i.infoFailureCode,
    infoFooter: i.infoFooter,
    infoFullDesc: i.infoFullDesc,
    infoHeader: i.infoHeader,
    infoParser: i.infoParser,
    infoPolicy: i.infoPolicy
  }
);
const noIntersperse = p => (
  {
    infoPolicy: Options$dApplicative$dTypes.NoIntersperse,
    infoFailureCode: p.infoFailureCode,
    infoFooter: p.infoFooter,
    infoFullDesc: p.infoFullDesc,
    infoHeader: p.infoHeader,
    infoParser: p.infoParser,
    infoProgDesc: p.infoProgDesc
  }
);
const noBacktrack = p => (
  {
    prefBacktrack: Options$dApplicative$dTypes.NoBacktrack,
    prefColumns: p.prefColumns,
    prefDisambiguate: p.prefDisambiguate,
    prefMultiSuffix: p.prefMultiSuffix,
    prefShowHelpOnEmpty: p.prefShowHelpOnEmpty,
    prefShowHelpOnError: p.prefShowHelpOnError
  }
);
const noArgError = e => Options$dApplicative$dBuilder$dInternal.$Mod(
  p => ({optNoArgError: v => e, optCompleter: p.optCompleter, optNames: p.optNames}),
  Options$dApplicative$dBuilder$dInternal.$DefaultProp(Data$dMaybe.Nothing, Data$dMaybe.Nothing),
  Options$dApplicative$dBuilder$dInternal.identity
);
const newtypePrefsMod = {Coercible0: () => {}};
const prefs = m => m({
  prefMultiSuffix: "",
  prefDisambiguate: false,
  prefShowHelpOnError: false,
  prefShowHelpOnEmpty: false,
  prefBacktrack: Options$dApplicative$dTypes.Backtrack,
  prefColumns: 80
});
const prefsModSemigroup = {append: m1 => m2 => x => m2(m1(x))};
const prefsModMonoid = {mempty: identity, Semigroup0: () => prefsModSemigroup};
const newtypeInfoMod = {Coercible0: () => {}};
const multiSuffix = s => p => (
  {
    prefMultiSuffix: s,
    prefBacktrack: p.prefBacktrack,
    prefColumns: p.prefColumns,
    prefDisambiguate: p.prefDisambiguate,
    prefShowHelpOnEmpty: p.prefShowHelpOnEmpty,
    prefShowHelpOnError: p.prefShowHelpOnError
  }
);
const metavar = dictHasMetavar => $$var => Options$dApplicative$dBuilder$dInternal.optionMod(p => (
  {propMetaVar: $$var, propDescMod: p.propDescMod, propHelp: p.propHelp, propShowDefault: p.propShowDefault, propVisibility: p.propVisibility}
));
const option = r => m => {
  const $0 = Options$dApplicative$dBuilder$dInternal.optionMod(p => (
    {propMetaVar: "ARG", propDescMod: p.propDescMod, propHelp: p.propHelp, propShowDefault: p.propShowDefault, propVisibility: p.propVisibility}
  ));
  const $1 = m._1($0._1({optNames: [], optCompleter: Options$dApplicative$dTypes.completerMonoid.mempty, optNoArgError: Options$dApplicative$dTypes.ExpectsArgError}));
  return Options$dApplicative$dBuilder$dInternal.mkParser(Options$dApplicative$dBuilder$dInternal.$DefaultProp(
    m._2._1.tag === "Nothing" ? $0._2._1 : m._2._1,
    m._2._2.tag === "Nothing" ? $0._2._2 : m._2._2
  ))(x => m._3($0._3(x)))(Options$dApplicative$dTypes.$OptReader("OptReader", $1.optNames, {crCompleter: $1.optCompleter, crReader: r}, $1.optNoArgError));
};
const strOption = /* #__PURE__ */ option(Options$dApplicative$dTypes.readerAsk);
const subparser = m => {
  const $0 = Options$dApplicative$dBuilder$dInternal.optionMod(p => (
    {propMetaVar: "COMMAND", propDescMod: p.propDescMod, propHelp: p.propHelp, propShowDefault: p.propShowDefault, propVisibility: p.propVisibility}
  ));
  const v1 = Options$dApplicative$dBuilder$dInternal.mkCommand(m);
  return Options$dApplicative$dBuilder$dInternal.mkParser(Options$dApplicative$dBuilder$dInternal.$DefaultProp(
    m._2._1.tag === "Nothing" ? $0._2._1 : m._2._1,
    m._2._2.tag === "Nothing" ? $0._2._2 : m._2._2
  ))(x => m._3($0._3(x)))(Options$dApplicative$dTypes.$OptReader("CmdReader", v1._1, v1._2._1, v1._2._2._1));
};
const maybeReader = f => Options$dApplicative$dTypes.bind(Options$dApplicative$dTypes.readerAsk)(x => {
  const $0 = Control$dMonad$dExcept$dTrans.monadThrowExceptT(Data$dIdentity.monadIdentity).throwError(Options$dApplicative$dTypes.$ParseError(
    "ErrorMsg",
    "cannot parse value `" + x + "'"
  ));
  const $1 = f(x);
  if ($1.tag === "Nothing") { return v => $0; }
  if ($1.tag === "Just") { return Options$dApplicative$dTypes.readMApplicative.pure($1._1); }
  $runtime.fail();
});
const $$long = dictHasName => x => Options$dApplicative$dBuilder$dInternal.$Mod(
  dictHasName.name(Options$dApplicative$dTypes.$OptName("OptLong", x)),
  Options$dApplicative$dBuilder$dInternal.$DefaultProp(Data$dMaybe.Nothing, Data$dMaybe.Nothing),
  Options$dApplicative$dBuilder$dInternal.identity
);
const infoModSemigroup = {append: m1 => m2 => x => m2(m1(x))};
const infoModMonoid = {mempty: identity, Semigroup0: () => infoModSemigroup};
const info = parser => m => m({
  infoParser: parser,
  infoFullDesc: true,
  infoProgDesc: Data$dMaybe.Nothing,
  infoHeader: Data$dMaybe.Nothing,
  infoFooter: Data$dMaybe.Nothing,
  infoFailureCode: ExitCodes.Error,
  infoPolicy: Options$dApplicative$dTypes.Intersperse
});
const idm = dictMonoid => dictMonoid.mempty;
const hidden = /* #__PURE__ */ Options$dApplicative$dBuilder$dInternal.optionMod(p => (
  {
    propVisibility: p.propVisibility === "Internal" ? p.propVisibility : Options$dApplicative$dTypes.Hidden,
    propDescMod: p.propDescMod,
    propHelp: p.propHelp,
    propMetaVar: p.propMetaVar,
    propShowDefault: p.propShowDefault
  }
));
const helpDoc = doc => Options$dApplicative$dBuilder$dInternal.optionMod(p => (
  {propHelp: doc, propDescMod: p.propDescMod, propMetaVar: p.propMetaVar, propShowDefault: p.propShowDefault, propVisibility: p.propVisibility}
));
const help = s => Options$dApplicative$dBuilder$dInternal.optionMod(p => (
  {
    propHelp: Options$dApplicative$dHelp$dChunk.paragraph(s),
    propDescMod: p.propDescMod,
    propMetaVar: p.propMetaVar,
    propShowDefault: p.propShowDefault,
    propVisibility: p.propVisibility
  }
));
const headerDoc = doc => i => (
  {
    infoHeader: doc,
    infoFailureCode: i.infoFailureCode,
    infoFooter: i.infoFooter,
    infoFullDesc: i.infoFullDesc,
    infoParser: i.infoParser,
    infoPolicy: i.infoPolicy,
    infoProgDesc: i.infoProgDesc
  }
);
const header = s => i => (
  {
    infoHeader: Options$dApplicative$dHelp$dChunk.paragraph(s),
    infoFailureCode: i.infoFailureCode,
    infoFooter: i.infoFooter,
    infoFullDesc: i.infoFullDesc,
    infoParser: i.infoParser,
    infoPolicy: i.infoPolicy,
    infoProgDesc: i.infoProgDesc
  }
);
const fullDesc = i => (
  {
    infoFullDesc: true,
    infoFailureCode: i.infoFailureCode,
    infoFooter: i.infoFooter,
    infoHeader: i.infoHeader,
    infoParser: i.infoParser,
    infoPolicy: i.infoPolicy,
    infoProgDesc: i.infoProgDesc
  }
);
const forwardOptions = p => (
  {
    infoPolicy: Options$dApplicative$dTypes.ForwardOptions,
    infoFailureCode: p.infoFailureCode,
    infoFooter: p.infoFooter,
    infoFullDesc: p.infoFullDesc,
    infoHeader: p.infoHeader,
    infoParser: p.infoParser,
    infoProgDesc: p.infoProgDesc
  }
);
const footerDoc = doc => i => (
  {
    infoFooter: doc,
    infoFailureCode: i.infoFailureCode,
    infoFullDesc: i.infoFullDesc,
    infoHeader: i.infoHeader,
    infoParser: i.infoParser,
    infoPolicy: i.infoPolicy,
    infoProgDesc: i.infoProgDesc
  }
);
const footer = s => i => (
  {
    infoFooter: Options$dApplicative$dHelp$dChunk.paragraph(s),
    infoFailureCode: i.infoFailureCode,
    infoFullDesc: i.infoFullDesc,
    infoHeader: i.infoHeader,
    infoParser: i.infoParser,
    infoPolicy: i.infoPolicy,
    infoProgDesc: i.infoProgDesc
  }
);
const flag$p = actv => v => Options$dApplicative$dBuilder$dInternal.mkParser(v._2)(v._3)((() => {
  const $0 = v._1({flagNames: [], flagActive: actv});
  return Options$dApplicative$dTypes.$OptReader("FlagReader", $0.flagNames, $0.flagActive);
})());
const flag = defv => actv => m => Options$dApplicative$dTypes.$Parser("AltP", flag$p(actv)(m), Options$dApplicative$dTypes.$Parser("NilP", defv));
const $$switch = /* #__PURE__ */ flag(false)(true);
const failureCode = n => i => (
  {
    infoFailureCode: n,
    infoFooter: i.infoFooter,
    infoFullDesc: i.infoFullDesc,
    infoHeader: i.infoHeader,
    infoParser: i.infoParser,
    infoPolicy: i.infoPolicy,
    infoProgDesc: i.infoProgDesc
  }
);
const eitherReader = f => Options$dApplicative$dTypes.bind(Options$dApplicative$dTypes.readerAsk)(x => {
  const $0 = f(x);
  if ($0.tag === "Left") {
    const $1 = Control$dMonad$dExcept$dTrans.monadThrowExceptT(Data$dIdentity.monadIdentity).throwError(Options$dApplicative$dTypes.$ParseError("ErrorMsg", $0._1));
    return v => $1;
  }
  if ($0.tag === "Right") { return Options$dApplicative$dTypes.readMApplicative.pure($0._1); }
  $runtime.fail();
});
const $$int = /* #__PURE__ */ eitherReader(s => {
  const v = Data$dInt.fromString(s);
  if (v.tag === "Nothing") { return Data$dEither.$Either("Left", "Can't parse as Int: `" + Data$dShow.showStringImpl(s) + "`"); }
  if (v.tag === "Just") { return Data$dEither.$Either("Right", v._1); }
  $runtime.fail();
});
const number = /* #__PURE__ */ eitherReader(s => {
  const v = Data$dNumber.fromStringImpl(s, Data$dNumber.isFinite, Data$dMaybe.Just, Data$dMaybe.Nothing);
  if (v.tag === "Nothing") { return Data$dEither.$Either("Left", "Can't parse as Number: `" + Data$dShow.showStringImpl(s) + "`"); }
  if (v.tag === "Just") { return Data$dEither.$Either("Right", v._1); }
  $runtime.fail();
});
const disambiguate = p => (
  {
    prefDisambiguate: true,
    prefBacktrack: p.prefBacktrack,
    prefColumns: p.prefColumns,
    prefMultiSuffix: p.prefMultiSuffix,
    prefShowHelpOnEmpty: p.prefShowHelpOnEmpty,
    prefShowHelpOnError: p.prefShowHelpOnError
  }
);
const disabled = /* #__PURE__ */ (() => {
  const $0 = Control$dMonad$dExcept$dTrans.monadThrowExceptT(Data$dIdentity.monadIdentity).throwError(Options$dApplicative$dTypes.$ParseError("ErrorMsg", "disabled option"));
  return v => $0;
})();
const defaultPrefs = {
  prefMultiSuffix: "",
  prefDisambiguate: false,
  prefShowHelpOnError: false,
  prefShowHelpOnEmpty: false,
  prefBacktrack: Options$dApplicative$dTypes.Backtrack,
  prefColumns: 80
};
const completer = dictHasCompleter => f => Options$dApplicative$dBuilder$dInternal.$Mod(
  dictHasCompleter.modCompleter(v => Options$dApplicative$dTypes.completerSemigroup.append(v)(f)),
  Options$dApplicative$dBuilder$dInternal.$DefaultProp(Data$dMaybe.Nothing, Data$dMaybe.Nothing),
  Options$dApplicative$dBuilder$dInternal.identity
);
const completeWith = dictHasCompleter => x => completer(dictHasCompleter)(Options$dApplicative$dBuilder$dCompleter.listCompleter(x));
const commandGroup = g => Options$dApplicative$dBuilder$dInternal.$Mod(
  p => ({cmdGroup: Data$dMaybe.$Maybe("Just", g), cmdCommands: p.cmdCommands}),
  Options$dApplicative$dBuilder$dInternal.$DefaultProp(Data$dMaybe.Nothing, Data$dMaybe.Nothing),
  Options$dApplicative$dBuilder$dInternal.identity
);
const command = cmd => pinfo => Options$dApplicative$dBuilder$dInternal.$Mod(
  p => ({cmdCommands: [Data$dTuple.$Tuple(cmd, pinfo), ...p.cmdCommands], cmdGroup: p.cmdGroup}),
  Options$dApplicative$dBuilder$dInternal.$DefaultProp(Data$dMaybe.Nothing, Data$dMaybe.Nothing),
  Options$dApplicative$dBuilder$dInternal.identity
);
const columns = cols => p => (
  {
    prefColumns: cols,
    prefBacktrack: p.prefBacktrack,
    prefDisambiguate: p.prefDisambiguate,
    prefMultiSuffix: p.prefMultiSuffix,
    prefShowHelpOnEmpty: p.prefShowHelpOnEmpty,
    prefShowHelpOnError: p.prefShowHelpOnError
  }
);
const briefDesc = i => (
  {
    infoFullDesc: false,
    infoFailureCode: i.infoFailureCode,
    infoFooter: i.infoFooter,
    infoHeader: i.infoHeader,
    infoParser: i.infoParser,
    infoPolicy: i.infoPolicy,
    infoProgDesc: i.infoProgDesc
  }
);
const $$boolean = /* #__PURE__ */ eitherReader(x => {
  const $0 = Data$dString$dCommon.toLower(x);
  if ($0 === "true") { return Data$dEither.$Either("Right", true); }
  if ($0 === "false") { return Data$dEither.$Either("Right", false); }
  return Data$dEither.$Either("Left", "Can't parse as Boolean: `" + Data$dShow.showStringImpl($0) + "`");
});
const argument = p => v => Options$dApplicative$dBuilder$dInternal.mkParser(v._2)(v._3)(Options$dApplicative$dTypes.$OptReader(
  "ArgReader",
  {crCompleter: v._1(mempty2).argCompleter, crReader: p}
));
const strArgument = /* #__PURE__ */ argument(Options$dApplicative$dTypes.readerAsk);
const action = dictHasCompleter => x => completer(dictHasCompleter)(Options$dApplicative$dBuilder$dCompleter.bashCompleter(x));
const abortOption = err => m => {
  const $0 = fold([
    Options$dApplicative$dBuilder$dInternal.$Mod(
      p => ({optNoArgError: v => err, optCompleter: p.optCompleter, optNames: p.optNames}),
      Options$dApplicative$dBuilder$dInternal.$DefaultProp(Data$dMaybe.Nothing, Data$dMaybe.Nothing),
      Options$dApplicative$dBuilder$dInternal.identity
    ),
    Options$dApplicative$dBuilder$dInternal.$Mod(
      identity,
      Options$dApplicative$dBuilder$dInternal.$DefaultProp(Data$dMaybe.$Maybe("Just", identity), Data$dMaybe.Nothing),
      identity
    ),
    Options$dApplicative$dBuilder$dInternal.optionMod(p => (
      {propMetaVar: "", propDescMod: p.propDescMod, propHelp: p.propHelp, propShowDefault: p.propShowDefault, propVisibility: p.propVisibility}
    ))
  ]);
  return option((() => {
    const $1 = Control$dMonad$dExcept$dTrans.monadThrowExceptT(Data$dIdentity.monadIdentity).throwError(err);
    return v => $1;
  })())(Options$dApplicative$dBuilder$dInternal.$Mod(
    x => m._1($0._1(x)),
    Options$dApplicative$dBuilder$dInternal.$DefaultProp(m._2._1.tag === "Nothing" ? $0._2._1 : m._2._1, m._2._2.tag === "Nothing" ? $0._2._2 : m._2._2),
    x => m._3($0._3(x))
  ));
};
const infoOption = x => abortOption(Options$dApplicative$dTypes.$ParseError("InfoMsg", x));
export {
  InfoMod,
  PrefsMod,
  abortOption,
  action,
  argument,
  $$boolean as boolean,
  briefDesc,
  columns,
  command,
  commandGroup,
  completeWith,
  completer,
  defaultPrefs,
  disabled,
  disambiguate,
  eitherReader,
  failureCode,
  flag,
  flag$p,
  fold,
  footer,
  footerDoc,
  forwardOptions,
  fullDesc,
  header,
  headerDoc,
  help,
  helpDoc,
  hidden,
  identity,
  idm,
  info,
  infoModMonoid,
  infoModSemigroup,
  infoOption,
  $$int as int,
  $$long as long,
  maybeReader,
  mempty2,
  metavar,
  multiSuffix,
  newtypeInfoMod,
  newtypePrefsMod,
  noArgError,
  noBacktrack,
  noIntersperse,
  number,
  option,
  prefs,
  prefsModMonoid,
  prefsModSemigroup,
  progDesc,
  progDescDoc,
  $$short as short,
  showDefault,
  showDefaultWith,
  showHelpOnEmpty,
  showHelpOnError,
  str,
  strArgument,
  strOption,
  style,
  subparser,
  subparserInline,
  $$switch as switch,
  value
};
