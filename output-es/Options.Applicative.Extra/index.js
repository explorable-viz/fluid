// * Extra parser utilities
//
// | This module contains high-level functions to run parsers.
import * as $runtime from "../runtime.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dMonoid from "../Data.Monoid/index.js";
import * as Data$dString$dCodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data$dString$dCommon from "../Data.String.Common/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as ExitCodes from "../ExitCodes/index.js";
import * as Node$dEncoding from "../Node.Encoding/index.js";
import * as Node$dProcess from "../Node.Process/index.js";
import * as Node$dStream from "../Node.Stream/index.js";
import * as Options$dApplicative$dBashCompletion from "../Options.Applicative.BashCompletion/index.js";
import * as Options$dApplicative$dBuilder from "../Options.Applicative.Builder/index.js";
import * as Options$dApplicative$dBuilder$dInternal from "../Options.Applicative.Builder.Internal/index.js";
import * as Options$dApplicative$dCommon from "../Options.Applicative.Common/index.js";
import * as Options$dApplicative$dHelp$dChunk from "../Options.Applicative.Help.Chunk/index.js";
import * as Options$dApplicative$dHelp$dCore from "../Options.Applicative.Help.Core/index.js";
import * as Options$dApplicative$dHelp$dLevenshtein from "../Options.Applicative.Help.Levenshtein/index.js";
import * as Options$dApplicative$dHelp$dTypes from "../Options.Applicative.Help.Types/index.js";
import * as Options$dApplicative$dInternal from "../Options.Applicative.Internal/index.js";
import * as Options$dApplicative$dInternal$dUtils from "../Options.Applicative.Internal.Utils/index.js";
import * as Options$dApplicative$dTypes from "../Options.Applicative.Types/index.js";
import * as Text$dPrettyPrint$dLeijen from "../Text.PrettyPrint.Leijen/index.js";
const unWords = xs => Data$dFoldable.foldlArray(v => v1 => {
  if (v.init) { return {init: false, acc: v1}; }
  return {init: false, acc: v.acc + " " + v1};
})({init: true, acc: ""})(xs).acc;
const fold = /* #__PURE__ */ (() => Data$dFoldable.foldableArray.foldMap(Data$dMonoid.monoidArray)(Data$dFoldable.identity))();
const fold1 = /* #__PURE__ */ (() => Data$dFoldable.foldableArray.foldMap(Options$dApplicative$dHelp$dTypes.parserHelpMonoid)(Data$dFoldable.identity))();
const fromFoldable = /* #__PURE__ */ Data$dFoldable.foldrArray(Data$dList$dTypes.Cons)(Data$dList$dTypes.Nil);
const renderFailure = failure => progn => {
  const v = failure(progn);
  return Data$dTuple.$Tuple(
    Text$dPrettyPrint$dLeijen.displayS(Text$dPrettyPrint$dLeijen.renderFits(Text$dPrettyPrint$dLeijen.fits1)(1.0)(v._2._2._1)(Options$dApplicative$dHelp$dTypes.helpText(v._1))),
    v._2._1
  );
};
const parserFailure = pprefs => pinfo => msg => ctx => {
  const suggestion_help = Options$dApplicative$dHelp$dCore.suggestionsHelp((() => {
    if (msg.tag === "UnexpectedError") {
      const $0 = msg._1;
      const good = Data$dArray.filterImpl(
        a => Options$dApplicative$dHelp$dLevenshtein.editDistance(Data$dEq.eqChar)(Data$dString$dCodeUnits.toCharArray(a))(Data$dString$dCodeUnits.toCharArray($0)) < 3,
        fold(Options$dApplicative$dCommon.mapParser(v => v1 => {
          if (v1.optMain.tag === "OptReader") { return Data$dFunctor.arrayMap(Options$dApplicative$dCommon.showOption)(v1.optMain._1); }
          if (v1.optMain.tag === "FlagReader") { return Data$dFunctor.arrayMap(Options$dApplicative$dCommon.showOption)(v1.optMain._1); }
          if (v1.optMain.tag === "ArgReader") { return []; }
          if (v1.optMain.tag === "CmdReader") {
            if (v.hinfoUnreachableArgs) { return []; }
            return v1.optMain._2;
          }
          $runtime.fail();
        })(msg._2._1))
      );
      const $1 = good.length < 2 ? Options$dApplicative$dHelp$dChunk.stringChunk("Did you mean this?") : Options$dApplicative$dHelp$dChunk.stringChunk("Did you mean one of these?");
      if ($1.tag === "Just") {
        const $2 = Options$dApplicative$dHelp$dChunk.vcatChunks(Data$dFunctor.arrayMap(Options$dApplicative$dHelp$dChunk.stringChunk)(good));
        if ($2.tag === "Just") {
          return Data$dMaybe.$Maybe(
            "Just",
            Text$dPrettyPrint$dLeijen.$Doc(
              "Cat",
              $1._1,
              Text$dPrettyPrint$dLeijen.$Doc(
                "Cat",
                Text$dPrettyPrint$dLeijen.$Doc("FlatAlt", Text$dPrettyPrint$dLeijen.Line, Text$dPrettyPrint$dLeijen.$Doc("Char", " ")),
                Text$dPrettyPrint$dLeijen.indent(4)($2._1)
              )
            )
          );
        }
      }
    }
    return Data$dMaybe.Nothing;
  })());
  const show_full_help = (() => {
    if (msg.tag === "ShowHelpText") { return true; }
    if (msg.tag === "MissingError" && msg._1 === "CmdStart" && pprefs.prefShowHelpOnEmpty) { return true; }
    return pprefs.prefShowHelpOnError;
  })();
  const exit_code = (() => {
    if (msg.tag === "ErrorMsg") { return pinfo.infoFailureCode; }
    if (msg.tag === "MissingError") { return pinfo.infoFailureCode; }
    if (msg.tag === "ExpectsArgError") { return pinfo.infoFailureCode; }
    if (msg.tag === "UnexpectedError") { return pinfo.infoFailureCode; }
    if (msg.tag === "ShowHelpText") { return ExitCodes.Success; }
    if (msg.tag === "InfoMsg") { return ExitCodes.Success; }
    $runtime.fail();
  })();
  const error_help = Options$dApplicative$dHelp$dCore.errorHelp((() => {
    if (msg.tag === "ShowHelpText") { return Data$dMaybe.Nothing; }
    if (msg.tag === "ErrorMsg") { return Options$dApplicative$dHelp$dChunk.stringChunk(msg._1); }
    if (msg.tag === "InfoMsg") { return Options$dApplicative$dHelp$dChunk.stringChunk(msg._1); }
    if (msg.tag === "MissingError") {
      if (msg._1 === "CmdStart" && pprefs.prefShowHelpOnEmpty) { return Data$dMaybe.Nothing; }
      const $0 = Options$dApplicative$dHelp$dChunk.stringChunk("Missing:");
      const $1 = Options$dApplicative$dHelp$dCore.briefDesc$p(false)(pprefs)(msg._2._1);
      if ($0.tag === "Nothing") { return $1; }
      if ($1.tag === "Nothing") { return $0; }
      if ($0.tag === "Just" && $1.tag === "Just") {
        return Data$dMaybe.$Maybe("Just", Text$dPrettyPrint$dLeijen.$Doc("Cat", $0._1, Text$dPrettyPrint$dLeijen.$Doc("Cat", Text$dPrettyPrint$dLeijen.$Doc("Char", " "), $1._1)));
      }
      $runtime.fail();
    }
    if (msg.tag === "ExpectsArgError") { return Options$dApplicative$dHelp$dChunk.stringChunk("The option `" + msg._1 + "` expects an argument."); }
    if (msg.tag === "UnexpectedError") {
      return Options$dApplicative$dHelp$dChunk.stringChunk(Options$dApplicative$dInternal$dUtils.startsWith("-")(msg._1)
        ? "Invalid option `" + msg._1 + "'"
        : "Invalid argument `" + msg._1 + "'");
    }
    $runtime.fail();
  })());
  return progn => Data$dTuple.$Tuple(
    (() => {
      const $0 = (names, pinfo$p) => fold1([
        (() => {
          const h = Options$dApplicative$dHelp$dCore.headerHelp(pinfo$p.infoHeader);
          const f = Options$dApplicative$dHelp$dCore.footerHelp(pinfo$p.infoFooter);
          if (show_full_help) { return fold1([h, f, Options$dApplicative$dHelp$dCore.parserHelp(pprefs)(pinfo$p.infoParser)]); }
          return Options$dApplicative$dHelp$dTypes.parserHelpMonoid.mempty;
        })(),
        msg.tag === "InfoMsg"
          ? Options$dApplicative$dHelp$dTypes.parserHelpMonoid.mempty
          : Options$dApplicative$dHelp$dCore.usageHelp(Options$dApplicative$dHelp$dChunk.vcatChunks([
              Data$dMaybe.$Maybe("Just", Options$dApplicative$dHelp$dCore.parserUsage(pprefs)(pinfo$p.infoParser)(unWords([progn, ...names]))),
              pinfo$p.infoProgDesc.tag === "Just" ? Data$dMaybe.$Maybe("Just", Text$dPrettyPrint$dLeijen.indent(2)(pinfo$p.infoProgDesc._1)) : Data$dMaybe.Nothing
            ])),
        suggestion_help,
        error_help
      ]);
      if (0 < ctx.length) { return $0(Data$dArray.reverse(Data$dFunctor.arrayMap(v => v._1)(ctx)), ctx[0]._2); }
      return $0([], pinfo);
    })(),
    Data$dTuple.$Tuple(exit_code, Data$dTuple.$Tuple(pprefs.prefColumns, undefined))
  );
};
const helper = /* #__PURE__ */ (() => Options$dApplicative$dBuilder.abortOption(Options$dApplicative$dTypes.ShowHelpText)(Data$dFoldable.foldableArray.foldMap(Options$dApplicative$dBuilder$dInternal.modMonoid)(Data$dFoldable.identity)([
  Options$dApplicative$dBuilder$dInternal.$Mod(
    Options$dApplicative$dBuilder$dInternal.optionFieldsHasName.name(Options$dApplicative$dTypes.$OptName("OptLong", "help")),
    Options$dApplicative$dBuilder$dInternal.$DefaultProp(Data$dMaybe.Nothing, Data$dMaybe.Nothing),
    Options$dApplicative$dBuilder$dInternal.identity
  ),
  Options$dApplicative$dBuilder$dInternal.$Mod(
    Options$dApplicative$dBuilder$dInternal.optionFieldsHasName.name(Options$dApplicative$dTypes.$OptName("OptShort", "h")),
    Options$dApplicative$dBuilder$dInternal.$DefaultProp(Data$dMaybe.Nothing, Data$dMaybe.Nothing),
    Options$dApplicative$dBuilder$dInternal.identity
  ),
  Options$dApplicative$dBuilder.help("Show this help text"),
  Options$dApplicative$dBuilder.hidden
])))();
const hsubparser = m => {
  const $0 = Options$dApplicative$dBuilder$dInternal.optionMod(p => (
    {propMetaVar: "COMMAND", propDescMod: p.propDescMod, propHelp: p.propHelp, propShowDefault: p.propShowDefault, propVisibility: p.propVisibility}
  ));
  const v1 = Options$dApplicative$dBuilder$dInternal.mkCommand(m);
  return Options$dApplicative$dBuilder$dInternal.mkParser(Options$dApplicative$dBuilder$dInternal.$DefaultProp(
    m._2._1.tag === "Nothing" ? $0._2._1 : m._2._1,
    m._2._2.tag === "Nothing" ? $0._2._2 : m._2._2
  ))(x => m._3($0._3(x)))(Options$dApplicative$dTypes.$OptReader(
    "CmdReader",
    v1._1,
    v1._2._1,
    x => {
      const $1 = v1._2._2._1(x);
      if ($1.tag === "Just") {
        return Data$dMaybe.$Maybe(
          "Just",
          {
            infoParser: Options$dApplicative$dInternal$dUtils.apApplyFlipped(Options$dApplicative$dTypes.parserApply)($1._1.infoParser)(helper),
            infoFailureCode: $1._1.infoFailureCode,
            infoFooter: $1._1.infoFooter,
            infoFullDesc: $1._1.infoFullDesc,
            infoHeader: $1._1.infoHeader,
            infoPolicy: $1._1.infoPolicy,
            infoProgDesc: $1._1.infoProgDesc
          }
        );
      }
      return Data$dMaybe.Nothing;
    }
  ));
};
const getProgName = () => {
  const a$p = Node$dProcess.argv();
  if (1 < a$p.length) {
    const $0 = Data$dString$dCommon.split("/")(a$p[1]);
    const $1 = $0.length - 1 | 0;
    if ($1 >= 0 && $1 < $0.length) { return $0[$1]; }
  }
  return "";
};
const getParseResult = v => {
  if (v.tag === "Success") { return Data$dMaybe.$Maybe("Just", v._1); }
  return Data$dMaybe.Nothing;
};
const getArgs = () => {
  const a$p = Node$dProcess.argv();
  return Data$dArray.sliceImpl(2, a$p.length, a$p);
};
const exitSuccess = /* #__PURE__ */ (() => {
  const $0 = ExitCodes.boundedEnumExitCode.fromEnum(ExitCodes.Success);
  return () => Node$dProcess.exitImpl($0);
})();
const handleParseResult = v => {
  if (v.tag === "Success") {
    const $0 = v._1;
    return () => $0;
  }
  if (v.tag === "Failure") {
    const $0 = v._1;
    return () => {
      const progn = getProgName();
      const v1 = renderFailure($0)(progn);
      Node$dStream.writeString(v1._2 === "Success" ? Node$dProcess.stdout : Node$dProcess.stderr)(Node$dEncoding.UTF8)(v1._1 + "\n")();
      return Node$dProcess.exitImpl(ExitCodes.boundedEnumExitCode.fromEnum(v1._2));
    };
  }
  if (v.tag === "CompletionInvoked") {
    const $0 = v._1;
    return () => {
      const progn = getProgName();
      const msg = $0.execCompletion(progn)();
      Node$dStream.writeString(Node$dProcess.stdout)(Node$dEncoding.UTF8)(msg)();
      return exitSuccess();
    };
  }
  $runtime.fail();
};
const execParserPure = pprefs => pinfo => args => {
  const v = Options$dApplicative$dCommon.runParserFully(Options$dApplicative$dInternal.pMonadP)(pinfo.infoPolicy)(Options$dApplicative$dTypes.$Parser(
    "AltP",
    Options$dApplicative$dTypes.parserFunctor.map(Data$dEither.Left)(Options$dApplicative$dBashCompletion.bashCompletionParser(pinfo)(pprefs)),
    Options$dApplicative$dTypes.parserFunctor.map(Data$dEither.Right)(pinfo.infoParser)
  ))(fromFoldable(args))([])(pprefs);
  if (v._1.tag === "Right") {
    if (v._1._1.tag === "Right") { return Options$dApplicative$dTypes.$ParserResult("Success", v._1._1._1); }
    if (v._1._1.tag === "Left") { return Options$dApplicative$dTypes.$ParserResult("CompletionInvoked", v._1._1._1); }
    $runtime.fail();
  }
  if (v._1.tag === "Left") { return Options$dApplicative$dTypes.$ParserResult("Failure", parserFailure(pprefs)(pinfo)(v._1._1)(v._2)); }
  $runtime.fail();
};
const customExecParser = pprefs => pinfo => () => {
  const a$p = getArgs();
  return handleParseResult(execParserPure(pprefs)(pinfo)(a$p))();
};
const execParser = /* #__PURE__ */ customExecParser(Options$dApplicative$dBuilder.defaultPrefs);
export {
  customExecParser,
  execParser,
  execParserPure,
  exitSuccess,
  fold,
  fold1,
  fromFoldable,
  getArgs,
  getParseResult,
  getProgName,
  handleParseResult,
  helper,
  hsubparser,
  parserFailure,
  renderFailure,
  unWords
};
