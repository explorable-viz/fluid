import * as $runtime from "../runtime.js";
import * as Control$dMonad$dReader$dTrans from "../Control.Monad.Reader.Trans/index.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dString$dCodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data$dString$dCommon from "../Data.String.Common/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Effect$dAff from "../Effect.Aff/index.js";
import * as Effect$dAff$dClass from "../Effect.Aff.Class/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
import * as Effect$dException from "../Effect.Exception/index.js";
import * as Eval from "../Eval/index.js";
import * as ExitCodes from "../ExitCodes/index.js";
import * as File from "../File/index.js";
import * as Module from "../Module/index.js";
import * as Module$dNode from "../Module.Node/index.js";
import * as Options$dApplicative$dBuilder from "../Options.Applicative.Builder/index.js";
import * as Options$dApplicative$dBuilder$dInternal from "../Options.Applicative.Builder.Internal/index.js";
import * as Options$dApplicative$dExtra from "../Options.Applicative.Extra/index.js";
import * as Options$dApplicative$dInternal$dUtils from "../Options.Applicative.Internal.Utils/index.js";
import * as Options$dApplicative$dTypes from "../Options.Applicative.Types/index.js";
import * as Parse from "../Parse/index.js";
import * as Pretty from "../Pretty/index.js";
import * as Pretty$dDoc from "../Pretty.Doc/index.js";
import * as Primitive$dDefs from "../Primitive.Defs/index.js";
import * as Val from "../Val/index.js";
const $Command = (tag, _1) => ({tag, _1});
const $EvalArgs = _1 => ({tag: "EvalArgs", _1});
const bind = /* #__PURE__ */ (() => Control$dMonad$dReader$dTrans.bindReaderT(Effect$dAff.bindAff).bind)();
const loadFileNodeT = /* #__PURE__ */ Module$dNode.loadFileNodeT(Effect$dAff.monadAff);
const monadErrorErrorNodeT = /* #__PURE__ */ Control$dMonad$dReader$dTrans.monadErrorReaderT(Effect$dAff.monadErrorAff);
const monadAffNodeT = /* #__PURE__ */ Effect$dAff$dClass.monadAffReader(Effect$dAff$dClass.monadAffAff);
const loadFile = /* #__PURE__ */ File.loadFile(loadFileNodeT)(/* #__PURE__ */ Control$dMonad$dReader$dTrans.monadReaderT(Effect$dAff.monadAff))(monadErrorErrorNodeT)(monadAffNodeT);
const monadReaderFileCxtNodeT = /* #__PURE__ */ Control$dMonad$dReader$dTrans.monadReaderReaderT(Effect$dAff.monadAff);
const prepConfig = /* #__PURE__ */ Module.prepConfig(monadAffNodeT)(monadErrorErrorNodeT)(monadReaderFileCxtNodeT)(loadFileNodeT);
const graphEval = /* #__PURE__ */ Eval.graphEval(monadAffNodeT)(monadReaderFileCxtNodeT)(loadFileNodeT)(monadErrorErrorNodeT);
const EvalArgs = value0 => $EvalArgs(value0);
const Evaluate = value0 => $Command("Evaluate", value0);
const Parse_ = value0 => $Command("Parse_", value0);
const parseLocal = /* #__PURE__ */ Options$dApplicative$dTypes.$Parser(
  "AltP",
  /* #__PURE__ */ Options$dApplicative$dBuilder.flag$p(true)(/* #__PURE__ */ (() => {
    const $0 = Options$dApplicative$dBuilder.help("Are you running fluid as a library?");
    const $1 = $0._2._1.tag === "Nothing" ? Data$dMaybe.Nothing : $0._2._1;
    const $2 = $0._2._2.tag === "Nothing" ? Data$dMaybe.Nothing : $0._2._2;
    return Options$dApplicative$dBuilder$dInternal.$Mod(
      x => $0._1({
        flagNames: [Options$dApplicative$dTypes.$OptName("OptShort", "l"), Options$dApplicative$dTypes.$OptName("OptLong", "local"), ...x.flagNames],
        flagActive: x.flagActive
      }),
      Options$dApplicative$dBuilder$dInternal.$DefaultProp($1.tag === "Nothing" ? Data$dMaybe.Nothing : $1, $2.tag === "Nothing" ? Data$dMaybe.Nothing : $2),
      x => $0._3(x)
    );
  })()),
  /* #__PURE__ */ Options$dApplicative$dTypes.$Parser("NilP", false)
);
const parseEvaluate = /* #__PURE__ */ (() => Options$dApplicative$dTypes.$Parser(
  "MultP",
  Options$dApplicative$dTypes.$MultPE(
    Options$dApplicative$dTypes.$Parser(
      "MultP",
      Options$dApplicative$dTypes.$MultPE(
        Options$dApplicative$dTypes.parserFunctor.map(v => v1 => v2 => $EvalArgs({local: v, fileName: v1, fluidSrcPath: v2}))(parseLocal),
        Options$dApplicative$dBuilder.option(Options$dApplicative$dTypes.readerAsk)((() => {
          const $0 = Options$dApplicative$dBuilder.help("The file to parse");
          const $1 = $0._2._1.tag === "Nothing" ? Data$dMaybe.Nothing : $0._2._1;
          const $2 = $0._2._2.tag === "Nothing" ? Data$dMaybe.Nothing : $0._2._2;
          return Options$dApplicative$dBuilder$dInternal.$Mod(
            x => $0._1({
              optNames: [Options$dApplicative$dTypes.$OptName("OptShort", "f"), Options$dApplicative$dTypes.$OptName("OptLong", "file"), ...x.optNames],
              optCompleter: x.optCompleter,
              optNoArgError: x.optNoArgError
            }),
            Options$dApplicative$dBuilder$dInternal.$DefaultProp($1.tag === "Nothing" ? Data$dMaybe.Nothing : $1, $2.tag === "Nothing" ? Data$dMaybe.Nothing : $2),
            x => $0._3(x)
          );
        })())
      )
    ),
    Options$dApplicative$dTypes.parserFunctor.map(File.Folder)(Options$dApplicative$dBuilder.option(Options$dApplicative$dTypes.readerAsk)((() => {
      const $0 = Options$dApplicative$dBuilder.help("The path containing the program files");
      const $1 = $0._2._1.tag === "Nothing" ? Data$dMaybe.Nothing : $0._2._1;
      const $2 = $0._2._2.tag === "Nothing" ? Data$dMaybe.Nothing : $0._2._2;
      return Options$dApplicative$dBuilder$dInternal.$Mod(
        x => $0._1({
          optNames: [Options$dApplicative$dTypes.$OptName("OptShort", "p"), Options$dApplicative$dTypes.$OptName("OptLong", "fluid-src-path"), ...x.optNames],
          optCompleter: x.optCompleter,
          optNoArgError: x.optNoArgError
        }),
        Options$dApplicative$dBuilder$dInternal.$DefaultProp($1.tag === "Nothing" ? Data$dMaybe.Nothing : $1, $2.tag === "Nothing" ? Data$dMaybe.Nothing : $2),
        x => $0._3(x)
      );
    })()))
  )
))();
const fluidLibraryPath = "node_modules/@explorable-viz/fluid";
const parse = v => {
  const fluidSrcPaths = [v._1.fluidSrcPath, ...v._1.local ? ["node_modules/@explorable-viz/fluid/dist/fluid/fluid"] : []];
  return bind(loadFile(fluidSrcPaths)(v._1.fileName))(fluidSrc => {
    const v1 = Parse.parse(Parse.withImports(Parse.expr))(fluidSrc);
    if (v1.tag === "Left") {
      const $0 = Effect$dAff._pure(v1._1);
      return v$1 => $0;
    }
    if (v1.tag === "Right") {
      const $0 = Effect$dAff._pure(Pretty$dDoc.renderWithIndent(Pretty$dDoc.Stmt)(0)(0)(Pretty.prettyExpr1(Val.annUnit).pretty(v1._1._1))._1);
      return v$1 => $0;
    }
    $runtime.fail();
  })({fluidSrcPaths});
};
const evaluate = v => {
  const fluidSrcPaths = [v._1.fluidSrcPath, ...v._1.local ? ["node_modules/@explorable-viz/fluid/dist/fluid/fluid"] : []];
  return bind(loadFile(fluidSrcPaths)(v._1.fileName))(fluidSrc => bind(prepConfig(Primitive$dDefs.primitives)(fluidSrc))(v1 => bind(graphEval(v1.gconfig)(v1.e))(v2 => {
    const $0 = Effect$dAff._pure(Val.functorVal.map(v$1 => {})(v2["outα"]));
    return v$1 => $0;
  })))({fluidSrcPaths});
};
const dispatchCommand = v => {
  if (v.tag === "Evaluate") {
    return Effect$dAff._bind(evaluate(v._1))(v1 => Effect$dAff._liftEffect(Effect$dConsole.log(Pretty$dDoc.renderWithIndent(Pretty$dDoc.Stmt)(0)(0)(Pretty.prettyVal(Val.highlightableUnit).pretty(v1))._1)));
  }
  if (v.tag === "Parse_") { return Effect$dAff._bind(parse(v._1))(r => Effect$dAff._liftEffect(Effect$dConsole.log(r))); }
  $runtime.fail();
};
const commands = /* #__PURE__ */ (() => (
  {evaluate: Options$dApplicative$dTypes.parserFunctor.map(Evaluate)(parseEvaluate), parse: Options$dApplicative$dTypes.parserFunctor.map(Parse_)(parseEvaluate)}
))();
const commandParser = /* #__PURE__ */ Options$dApplicative$dBuilder.subparser(/* #__PURE__ */ (() => {
  const $0 = Options$dApplicative$dBuilder.command("evaluate")(Options$dApplicative$dBuilder.progDesc("Evaluate a file")({
    infoParser: commands.evaluate,
    infoFullDesc: true,
    infoProgDesc: Data$dMaybe.Nothing,
    infoHeader: Data$dMaybe.Nothing,
    infoFooter: Data$dMaybe.Nothing,
    infoFailureCode: ExitCodes.Error,
    infoPolicy: Options$dApplicative$dTypes.Intersperse
  }));
  const $1 = Options$dApplicative$dBuilder.command("parse")(Options$dApplicative$dBuilder.progDesc("Parse a file")({
    infoParser: commands.parse,
    infoFullDesc: true,
    infoProgDesc: Data$dMaybe.Nothing,
    infoHeader: Data$dMaybe.Nothing,
    infoFooter: Data$dMaybe.Nothing,
    infoFailureCode: ExitCodes.Error,
    infoPolicy: Options$dApplicative$dTypes.Intersperse
  }));
  return Options$dApplicative$dBuilder$dInternal.$Mod(
    x => $1._1($0._1(x)),
    Options$dApplicative$dBuilder$dInternal.$DefaultProp($1._2._1.tag === "Nothing" ? $0._2._1 : $1._2._1, $1._2._2.tag === "Nothing" ? $0._2._2 : $1._2._2),
    x => $1._3($0._3(x))
  );
})());
const callback = v => {
  if (v.tag === "Left") { return Effect$dConsole.log(Effect$dException.showErrorImpl(v._1)); }
  if (v.tag === "Right") { return () => {}; }
  $runtime.fail();
};
const main = /* #__PURE__ */ (() => {
  const $0 = Effect$dAff.runAff(callback)(Effect$dAff._bind(Effect$dAff._liftEffect((() => {
    const $0 = Options$dApplicative$dBuilder.header("parse - a simple parser")(Options$dApplicative$dBuilder.progDesc("Parse a file")({
      infoFullDesc: true,
      infoFailureCode: ExitCodes.Error,
      infoFooter: Data$dMaybe.Nothing,
      infoHeader: Data$dMaybe.Nothing,
      infoParser: Options$dApplicative$dInternal$dUtils.apApplyFlipped(Options$dApplicative$dTypes.parserApply)(commandParser)(Options$dApplicative$dExtra.helper),
      infoPolicy: Options$dApplicative$dTypes.Intersperse,
      infoProgDesc: Data$dMaybe.Nothing
    }));
    return () => {
      const a$p = Options$dApplicative$dExtra.getArgs();
      return Options$dApplicative$dExtra.handleParseResult(Options$dApplicative$dExtra.execParserPure(Options$dApplicative$dBuilder.defaultPrefs)($0)(a$p))();
    };
  })()))(dispatchCommand));
  return () => {$0();};
})();
const between = p1 => p2 => f => s => {
  const $0 = Data$dString$dCodeUnits.stripPrefix(p1)(s);
  const v = (() => {
    if ($0.tag === "Just") { return Data$dString$dCodeUnits.stripSuffix(p2)($0._1); }
    if ($0.tag === "Nothing") { return Data$dMaybe.Nothing; }
    $runtime.fail();
  })();
  if (v.tag === "Just") { return f(v._1); }
  if (v.tag === "Nothing") {
    return Data$dEither.$Either("Left", "Expected (Pattern " + Data$dShow.showStringImpl(p1) + ")...(Pattern " + Data$dShow.showStringImpl(p2) + ") but got ...");
  }
  $runtime.fail();
};
const parseImports$p = open => close => between(open)(close)(s => Data$dEither.$Either(
  "Right",
  Data$dFunctor.arrayMap(Data$dString$dCommon.trim)(Data$dArray.filterImpl(x => x !== "", Data$dString$dCommon.split(",")(s)))
));
const parsePair = /* #__PURE__ */ between("(")(")")(s => {
  const v = Data$dString$dCommon.split(",")(s);
  if (v.length === 2) { return Data$dEither.$Either("Right", Data$dTuple.$Tuple(Data$dString$dCommon.trim(v[0]), Data$dString$dCommon.trim(v[1]))); }
  return Data$dEither.$Either("Left", "Expected a pair but got " + s);
});
export {
  $Command,
  $EvalArgs,
  EvalArgs,
  Evaluate,
  Parse_,
  between,
  bind,
  callback,
  commandParser,
  commands,
  dispatchCommand,
  evaluate,
  fluidLibraryPath,
  graphEval,
  loadFile,
  loadFileNodeT,
  main,
  monadAffNodeT,
  monadErrorErrorNodeT,
  monadReaderFileCxtNodeT,
  parse,
  parseEvaluate,
  parseImports$p,
  parseLocal,
  parsePair,
  prepConfig
};
