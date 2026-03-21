import * as $runtime from "../runtime.js";
import * as App$dUtil from "../App.Util/index.js";
import * as Control$dMonad$dWriter$dTrans from "../Control.Monad.Writer.Trans/index.js";
import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dHeytingAlgebra from "../Data.HeytingAlgebra/index.js";
import * as Data$dList$dLazy from "../Data.List.Lazy/index.js";
import * as Data$dMap$dInternal from "../Data.Map.Internal/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dString$dCommon from "../Data.String.Common/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Desug from "../Desug/index.js";
import * as Effect$dAff$dClass from "../Effect.Aff.Class/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
import * as Effect$dException from "../Effect.Exception/index.js";
import * as Eval from "../Eval/index.js";
import * as Expr from "../Expr/index.js";
import * as File from "../File/index.js";
import * as Graph$dGraphImpl from "../Graph.GraphImpl/index.js";
import * as Lattice from "../Lattice/index.js";
import * as Module from "../Module/index.js";
import * as Parse from "../Parse/index.js";
import * as Pretty from "../Pretty/index.js";
import * as Pretty$dDoc from "../Pretty.Doc/index.js";
import * as SExpr from "../SExpr/index.js";
import * as Test$dBenchmark$dUtil from "../Test.Benchmark.Util/index.js";
import * as Util from "../Util/index.js";
import * as Val from "../Val/index.js";
const eq = /* #__PURE__ */ (() => SExpr.eqExpr(Data$dEq.eqUnit).eq)();
const boundedLattice = {BoundedJoinSemilattice0: () => Lattice.boundedJoinSemilatticeBoo, BoundedMeetSemilattice1: () => Lattice.boundedMeetSemilatticeBoo};
const graphGC = /* #__PURE__ */ Eval.graphGC(Graph$dGraphImpl.graphGraphImpl);
const graphGC1 = /* #__PURE__ */ graphGC(Val.applyEnvExpr)(Val.applyVal)(Val.foldableEnvExpr)(Val.foldableVal);
const getPersistent = v => {
  if (v.tag === "Inert") { return false; }
  if (v.tag === "Reactive") { return v._1.persistent; }
  $runtime.fail();
};
const prettyVal = /* #__PURE__ */ Pretty.prettyVal(Val.highlightableBoolean);
const showPrettyShow = /* #__PURE__ */ (() => {
  const $0 = Pretty.prettyExpr(Val.highlightableBoolean);
  return {show: v => Pretty$dDoc.renderWithIndent(Pretty$dDoc.Stmt)(0)(0)($0.pretty(v))._1};
})();
const greaterThanOrEq = /* #__PURE__ */ (() => {
  const $0 = Expr.ordExpr(Data$dOrd.ordBoolean);
  return a1 => a2 => $0.compare(a1)(a2) !== "LT";
})();
const showPrettyShow1 = {show: v => Pretty$dDoc.renderWithIndent(Pretty$dDoc.Stmt)(0)(0)(prettyVal.pretty(v))._1};
const greaterThanOrEq1 = /* #__PURE__ */ (() => {
  const $0 = Val.ordVal(Data$dOrd.ordBoolean);
  return a1 => a2 => $0.compare(a1)(a2) !== "LT";
})();
const dual = v => (
  {
    fwd: x => Val.functorEnvExpr.map(Data$dHeytingAlgebra.boolNot)(v.bwd(Val.functorVal.map(Data$dHeytingAlgebra.boolNot)(x))),
    bwd: x => Val.functorVal.map(Data$dHeytingAlgebra.boolNot)(v.fwd(Val.functorEnvExpr.map(Data$dHeytingAlgebra.boolNot)(x)))
  }
);
const graphGC2 = /* #__PURE__ */ graphGC(Val.applyVal)(Val.applyEnvExpr)(Val.foldableVal)(Val.foldableEnvExpr);
const monadWriterT = /* #__PURE__ */ Control$dMonad$dWriter$dTrans.monadWriterT(Test$dBenchmark$dUtil.monoidBenchRow);
const monadReaderWriterT = /* #__PURE__ */ Control$dMonad$dWriter$dTrans.monadReaderWriterT(Test$dBenchmark$dUtil.monoidBenchRow);
const monadWriterWriterT = /* #__PURE__ */ Control$dMonad$dWriter$dTrans.monadWriterWriterT(Test$dBenchmark$dUtil.monoidBenchRow);
const monadErrorWriterT = /* #__PURE__ */ Control$dMonad$dWriter$dTrans.monadErrorWriterT(Test$dBenchmark$dUtil.monoidBenchRow);
const testPretty = dictAnn => {
  const prettyP2 = Pretty.prettyP(Pretty.prettyExpr1(dictAnn));
  return dictShow => s => dictMonadAff => {
    const MonadEffect0 = dictMonadAff.MonadEffect0();
    const Monad0 = MonadEffect0.Monad0();
    const Bind1 = Monad0.Bind1();
    const $0 = MonadEffect0.Monad0().Applicative0();
    const $1 = Monad0.Applicative0();
    return dictMonadError => {
      const throwLeft = Util.throwLeft(dictMonadError)(Data$dShow.showString);
      const withMsg = Util.withMsg(dictMonadError);
      const $$throw = Util.throw(dictMonadError.MonadThrow0());
      return Bind1.bind($0.pure())(() => Bind1.bind($0.pure())(() => Bind1.bind(withMsg("testPretty")(throwLeft(Parse.parse(Parse.withImports(Parse.expr))(prettyP2(s)))))(v => {
        const $2 = eq(SExpr.functorExpr.map(v$1 => {})(s))(SExpr.functorExpr.map(v$1 => {})(v._1));
        const $3 = $$throw("parse/prettyP round trip:\nOriginal\n" + Pretty$dDoc.renderWithIndent(Pretty$dDoc.Stmt)(0)(0)(Pretty.prettyExpr1(Val.annUnit).pretty(SExpr.functorExpr.map(v$1 => {})(s)))._1 + "\nNew\n" + Pretty$dDoc.renderWithIndent(Pretty$dDoc.Stmt)(0)(0)(Pretty.prettyExpr1(Val.annUnit).pretty(SExpr.functorExpr.map(v$1 => {})(v._1)))._1);
        if (!$2) { return $3; }
        if ($2) { return $1.pure(); }
        $runtime.fail();
      })));
    };
  };
};
const testPretty1 = /* #__PURE__ */ testPretty(Val.annUnit)(Data$dShow.showUnit);
const testOutcome = b => s => (b ? "\u001b[32m " : "\u001b[31m ") + (b ? "✔" : "✖") + "\u001b[0m " + s;
const testCondition = dictMonadThrow => dictMonadEffect => {
  const Monad0 = dictMonadEffect.Monad0();
  const $0 = Monad0.Applicative0();
  return testName => b => msg => Monad0.Bind1().bind(dictMonadEffect.liftEffect(Effect$dConsole.log((b ? "\u001b[32m " : "\u001b[31m ") + (b ? "✔" : "✖") + "\u001b[0m " + testName + ": " + msg)))(() => {
    const $1 = dictMonadThrow.throwError(Effect$dException.error("Test failed"));
    if (!b) { return $1; }
    return $0.pure();
  });
};
const graphBenchmark = dictMonadWriter => name => Test$dBenchmark$dUtil.benchmark(dictMonadWriter)("G-" + name);
const fluidSrcPaths = ["fluid", "test/fluid"];
const checkPretty = dictPretty => expect => x => dictMonadEffect => {
  const $0 = dictMonadEffect.Monad0().Applicative0();
  return dictMonadError => {
    const $1 = Data$dString$dCommon.trim(expect) === Pretty$dDoc.renderWithIndent(Pretty$dDoc.Stmt)(0)(0)(dictPretty.pretty(x))._1;
    const $2 = dictMonadError.MonadThrow0().throwError(Effect$dException.error("checkPretty:\nExpected\n" + expect + "\nReceived\n" + Pretty$dDoc.renderWithIndent(Pretty$dDoc.Stmt)(0)(0)(dictPretty.pretty(x))._1));
    if (!$1) { return $2; }
    if ($1) { return $0.pure(); }
    $runtime.fail();
  };
};
const checkEq = dictBotOf => dictNeg => dictMeetSemilattice => dictEq => dictPretty => dictMonadError => {
  const MonadThrow0 = dictMonadError.MonadThrow0();
  return op1 => op2 => x => y => {
    const v = Pretty.compare(dictBotOf)(dictNeg)(dictMeetSemilattice)(dictEq)(dictPretty)(op1)(op2)(x)(y);
    const $0 = v._2;
    return MonadThrow0.Monad0().Bind1().bind(Util.check(MonadThrow0)(v._1 === "")(v._1))(() => Util.check(MonadThrow0)($0 === "")($0));
  };
};
const benchNames = {eval: "Eval", bwd: "Demands", demBy: "DemBy", fwd: "Suffices", demBy_G_direct: "DemBy-Dir", demBy_G_suff_dual: "DemBy-Suff"};
const testProperties = dictMonadReader => dictLoadFile => dictMonadWriter => s => gconfig => v => dictMonadAff => {
  const MonadEffect0 = dictMonadAff.MonadEffect0();
  const Monad0 = MonadEffect0.Monad0();
  const Bind1 = Monad0.Bind1();
  const graphEval = Eval.graphEval(dictMonadAff)(dictMonadReader)(dictLoadFile);
  const Applicative0 = Monad0.Applicative0();
  return dictMonadError => {
    const graphEval1 = graphEval(dictMonadError);
    const checkSatisfies = Util.checkSatisfies(dictMonadError.MonadThrow0());
    const checkSatisfies1 = checkSatisfies(showPrettyShow);
    const checkSatisfies2 = checkSatisfies(showPrettyShow1);
    const withMsg = Util.withMsg(dictMonadError);
    const $0 = v.bwd_expect;
    const $1 = v.fwd_expect;
    return Bind1.bind(Desug.desugGC(dictMonadError)(Data$dEq.eqBoolean)(boundedLattice)(s))(v1 => {
      const $2 = v1.gc;
      const $3 = v1.e;
      return Bind1.bind(Test$dBenchmark$dUtil.benchmark$p(dictMonadWriter)("G-Eval")(v2 => graphEval1(gconfig)($3))(MonadEffect0)(dictMonadError))(v2 => {
        const $4 = v2.g;
        const $5 = graphGC1(v2);
        const $6 = {fwd: x => $5.fwd(x)._1, bwd: x => $5.bwd(x)._1};
        const out0 = Val.functorVal.map(getPersistent)(v["δv"](Val.functorVal.map(v$1 => App$dUtil.unselected)(Val.functorVal.map(v$1 => true)(v2["outα"])))._1);
        return Bind1.bind(Test$dBenchmark$dUtil.benchmark$p(dictMonadWriter)("G-Demands")(v5 => Applicative0.pure($6.bwd(Util.spyWhen(false)("Selection for bwd")(Pretty.prettyP(prettyVal))(out0))))(MonadEffect0)(dictMonadError))(v5 => {
          const $7 = v5._2;
          const $8 = v5._1;
          const in_s = $2.bwd($7);
          return Bind1.bind((() => {
            const in_e$p = $2.fwd(in_s);
            return Bind1.bind(checkSatisfies1("fwd ⚬ bwd round-trip (desugar)")(in_e$p)(x => greaterThanOrEq(x)($7)))(() => Test$dBenchmark$dUtil.benchmark$p(dictMonadWriter)("G-Suffices")(v6 => Applicative0.pure($6.fwd(Val.$EnvExpr(
              $8,
              in_e$p
            ))))(MonadEffect0)(dictMonadError));
          })())(out1 => Bind1.bind(checkSatisfies2("fwd ⚬ bwd round-trip (eval)")(out1)(x => greaterThanOrEq1(x)(out0)))(() => Bind1.bind((() => {
            const $9 = $0 === "";
            const $10 = withMsg("bwd_expect")(checkPretty(Pretty.prettyExpr1(Val.annBoolean))($0)(in_s)(MonadEffect0)(dictMonadError));
            if (!$9) { return $10; }
            if ($9) { return Applicative0.pure(); }
            $runtime.fail();
          })())(() => Bind1.bind((() => {
            const $9 = $1 === "";
            const $10 = withMsg("fwd_expect")(checkPretty(prettyVal)($1)(Util.spyWhen(false)("fwd ⚬ bwd")(Pretty.prettyP(prettyVal))(out1))(MonadEffect0)(dictMonadError));
            if (!$9) { return $10; }
            if ($9) { return Applicative0.pure(); }
            $runtime.fail();
          })())(() => Bind1.bind(Test$dBenchmark$dUtil.recordGraphSize(Graph$dGraphImpl.graphGraphImpl)(dictMonadWriter)($4))(() => Bind1.bind(Applicative0.pure())(() => {
            const $9 = dual($6);
            const $10 = graphGC2({
              g: Graph$dGraphImpl.$GraphImpl({out: v2.g._1.in_, in_: v2.g._1.out, sinks: v2.g._1.sources, sources: v2.g._1.sinks, vertices: v2.g._1.vertices}),
              graph_fwd: v2.graph_fwd,
              graph_bwd: v2.graph_bwd,
              "inα": v2["outα"],
              "outα": v2["inα"]
            });
            return Bind1.bind(Test$dBenchmark$dUtil.benchmark$p(dictMonadWriter)("G-DemBy-Dir")(v8 => Applicative0.pure($10.bwd(v5)._1))(MonadEffect0)(dictMonadError))(out2 => Bind1.bind(Test$dBenchmark$dUtil.benchmark$p(dictMonadWriter)("G-DemBy-Suff")(v8 => Applicative0.pure($9.bwd(v5)))(MonadEffect0)(dictMonadError))(out3 => Applicative0.pure()));
          }))))));
        });
      });
    });
  };
};
const test = dictMonadReader => {
  const testProperties1 = testProperties(monadReaderWriterT(dictMonadReader));
  return dictLoadFile => file => primitives => spec => v => dictMonadAff => {
    const MonadEffect0 = dictMonadAff.MonadEffect0();
    const Monad0 = MonadEffect0.Monad0();
    const Bind1 = Monad0.Bind1();
    const loadFile1 = File.loadFile(dictLoadFile)(Monad0);
    const $0 = MonadEffect0.Monad0().Applicative0();
    const prepConfig = Module.prepConfig(dictMonadAff);
    const replicateM = Data$dList$dLazy.replicateM(monadWriterT(Monad0));
    const monadWriterWriterT1 = monadWriterWriterT(Monad0);
    const monadAffWriter = Effect$dAff$dClass.monadAffWriter(dictMonadAff)(Test$dBenchmark$dUtil.monoidBenchRow);
    return dictMonadError => {
      const prepConfig1 = prepConfig(dictMonadError)(dictMonadReader)(dictLoadFile);
      const testProperties2 = testProperties1((() => {
        const $1 = dictMonadAff.MonadEffect0().Monad0();
        const loadFileFromPath1 = dictLoadFile.loadFileFromPath(dictMonadError)(dictMonadAff);
        return {
          loadFileFromPath: dictMonadError1 => dictMonadAff1 => x => $1.Bind1().bind(loadFileFromPath1(x))(a => $1.Applicative0().pure(Data$dTuple.$Tuple(
            a,
            Data$dMap$dInternal.Leaf
          )))
        };
      })())(monadWriterWriterT1);
      const monadErrorWriterT1 = monadErrorWriterT(dictMonadError);
      const $1 = v._1;
      return Bind1.bind(loadFile1(dictMonadError)(dictMonadAff)(fluidSrcPaths)(file))(fluidSrc => Bind1.bind($0.pure())(() => Bind1.bind(prepConfig1(primitives)(fluidSrc))(v1 => {
        const $2 = v1.gconfig;
        const $3 = v1.s;
        return Bind1.bind(testPretty1($3)(dictMonadAff)(dictMonadError))(() => Bind1.bind(replicateM($1)(testProperties2($3)($2)(spec)(monadAffWriter)(monadErrorWriterT1)))(v2 => Monad0.Applicative0().pure(Test$dBenchmark$dUtil.divRow(v2._2)($1))));
      })));
    };
  };
};
export {
  benchNames,
  boundedLattice,
  checkEq,
  checkPretty,
  dual,
  eq,
  fluidSrcPaths,
  getPersistent,
  graphBenchmark,
  graphGC,
  graphGC1,
  graphGC2,
  greaterThanOrEq,
  greaterThanOrEq1,
  monadErrorWriterT,
  monadReaderWriterT,
  monadWriterT,
  monadWriterWriterT,
  prettyVal,
  showPrettyShow,
  showPrettyShow1,
  test,
  testCondition,
  testOutcome,
  testPretty,
  testPretty1,
  testProperties
};
