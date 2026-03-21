import * as $runtime from "../runtime.js";
import * as Control$dMonad$dReader$dTrans from "../Control.Monad.Reader.Trans/index.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dTraversable from "../Data.Traversable/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Effect$dAff from "../Effect.Aff/index.js";
import * as Effect$dAff$dClass from "../Effect.Aff.Class/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
import * as Effect$dException from "../Effect.Exception/index.js";
import * as Module$dNode from "../Module.Node/index.js";
import * as Node$dEncoding from "../Node.Encoding/index.js";
import * as Node$dFS$dSync from "../Node.FS.Sync/index.js";
import * as Test$dBenchmark$dUtil from "../Test.Benchmark.Util/index.js";
import * as Test$dSpecs$dBwd from "../Test.Specs.Bwd/index.js";
import * as Test$dSpecs$dDesugar from "../Test.Specs.Desugar/index.js";
import * as Test$dSpecs$dGraphics from "../Test.Specs.Graphics/index.js";
import * as Test$dSpecs$dMisc from "../Test.Specs.Misc/index.js";
import * as Test$dUtil from "../Test.Util/index.js";
import * as Test$dUtil$dSuite from "../Test.Util.Suite/index.js";
import * as Util from "../Util/index.js";
const handleBench = v => {
  if (v.tag === "Left") { return Effect$dException.throwException(Effect$dException.error(Effect$dException.showErrorImpl(v._1)))(); }
  if (v.tag === "Right") {
    const $0 = Node$dFS$dSync.writeTextFile(Node$dEncoding.ASCII)("benchmark/benchmarks_artifact.csv")(Test$dBenchmark$dUtil.showBenchAcc.show(v._1));
    return () => {
      $0();
      return Effect$dConsole.log("Benchmarking data written to benchmark/benchmarks_artifact.csv")();
    };
  }
  $runtime.fail();
};
const benchmarks = dictMonadAff => {
  const bwdSuite = Test$dUtil$dSuite.bwdSuite(dictMonadAff);
  return dictMonadError => {
    const bwdSuite1 = bwdSuite(dictMonadError);
    return dictMonadReader => {
      const suite2 = Test$dUtil$dSuite.suite(dictMonadAff)(dictMonadError)(dictMonadReader);
      const bwdSuite2 = bwdSuite1(dictMonadReader);
      return dictLoadFile => {
        const suite3 = suite2(dictLoadFile);
        return [
          suite3(Test$dSpecs$dDesugar.desugar_cases),
          suite3(Test$dSpecs$dMisc.misc_cases),
          bwdSuite2(dictLoadFile)(Test$dSpecs$dBwd.bwd_cases),
          suite3(Test$dSpecs$dGraphics.graphics_cases)
        ];
      };
    };
  };
};
const main = /* #__PURE__ */ (() => {
  const $0 = Effect$dAff.runAff(handleBench)(Effect$dAff._bind(Data$dTraversable.traversableArray.traverse(Effect$dAff.applicativeAff)(Data$dTraversable.identity)(Data$dFunctor.arrayMap(x => {
    const $0 = x._1;
    const $1 = x._2({fluidSrcPaths: Test$dUtil.fluidSrcPaths});
    return Effect$dAff._bind(Effect$dAff._liftEffect(Effect$dConsole.log("Benchmarking: " + $0)))(() => Effect$dAff._map(v1 => Data$dTuple.$Tuple($0, v1))($1));
  })(Data$dArray.concat(Data$dFunctor.arrayMap(f => f(Data$dTuple.$Tuple(10, true)))(benchmarks(Effect$dAff$dClass.monadAffReader(Effect$dAff$dClass.monadAffAff))(Control$dMonad$dReader$dTrans.monadErrorReaderT(Effect$dAff.monadErrorAff))(Control$dMonad$dReader$dTrans.monadReaderReaderT(Effect$dAff.monadAff))(Module$dNode.loadFileNodeT(Effect$dAff.monadAff)))))))(outs => Effect$dAff._pure(Util.definitely("More than one benchmark")(outs.length > 0
    ? Data$dMaybe.$Maybe("Just", outs)
    : Data$dMaybe.Nothing))));
  return () => {$0();};
})();
export {benchmarks, handleBench, main};
