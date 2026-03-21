import * as Control$dMonad$dReader$dTrans from "../Control.Monad.Reader.Trans/index.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Effect$dAff from "../Effect.Aff/index.js";
import * as Effect$dAff$dClass from "../Effect.Aff.Class/index.js";
import * as File from "../File/index.js";
import * as Test$dSpecs$dBwd from "../Test.Specs.Bwd/index.js";
import * as Test$dSpecs$dComments from "../Test.Specs.Comments/index.js";
import * as Test$dSpecs$dDesugar from "../Test.Specs.Desugar/index.js";
import * as Test$dSpecs$dGraphics from "../Test.Specs.Graphics/index.js";
import * as Test$dSpecs$dLinkedInputs from "../Test.Specs.LinkedInputs/index.js";
import * as Test$dSpecs$dLinkedOutputs from "../Test.Specs.LinkedOutputs/index.js";
import * as Test$dSpecs$dMisc from "../Test.Specs.Misc/index.js";
import * as Test$dSpecs$dParagraph from "../Test.Specs.Paragraph/index.js";
import * as Test$dUtil from "../Test.Util/index.js";
import * as Test$dUtil$dMocha from "../Test.Util.Mocha/index.js";
import * as Test$dUtil$dSuite from "../Test.Util.Suite/index.js";
const scratchpad = dictMonadAff => {
  const $$void = dictMonadAff.MonadEffect0().Monad0().Bind1().Apply0().Functor0().map(v => {});
  return dictMonadError => dictMonadReader => {
    const suite2 = Test$dUtil$dSuite.suite(dictMonadAff)(dictMonadError)(dictMonadReader);
    return dictLoadFile => Data$dFunctor.arrayMap(m => Data$dTuple.$Tuple(m._1, $$void(m._2)))(suite2(dictLoadFile)(Test$dSpecs$dParagraph.paragraph_cases)(Data$dTuple.$Tuple(
      1,
      false
    )));
  };
};
const linkingTests = dictMonadAff => {
  const linkedOutputsSuite = Test$dUtil$dSuite.linkedOutputsSuite(dictMonadAff);
  const linkedInputsSuite = Test$dUtil$dSuite.linkedInputsSuite(dictMonadAff);
  return dictMonadError => {
    const linkedOutputsSuite1 = linkedOutputsSuite(dictMonadError);
    const linkedInputsSuite1 = linkedInputsSuite(dictMonadError);
    return dictMonadReader => {
      const linkedOutputsSuite2 = linkedOutputsSuite1(dictMonadReader);
      const linkedInputsSuite2 = linkedInputsSuite1(dictMonadReader);
      return dictLoadFile => [
        ...linkedOutputsSuite2(dictLoadFile)(Test$dSpecs$dLinkedOutputs.linkedOutputs_cases),
        ...linkedInputsSuite2(dictLoadFile)(Test$dSpecs$dLinkedInputs.linkedInputs_cases)
      ];
    };
  };
};
const filterSuite = dictMonadAff => {
  const $$void = dictMonadAff.MonadEffect0().Monad0().Bind1().Apply0().Functor0().map(v => {});
  return dictMonadError => dictMonadReader => dictLoadFile => files => cases => makeSuite => Data$dFunctor.arrayMap(m => Data$dTuple.$Tuple(m._1, $$void(m._2)))(makeSuite(dictMonadError)(dictMonadReader)(dictLoadFile)(Data$dArray.filterImpl(
    c => Data$dArray.elem(Data$dEq.eqString)(c.file)(files),
    cases
  ))(Data$dTuple.$Tuple(1, false)));
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
          suite3(Test$dSpecs$dComments.comments_cases),
          suite3(Test$dSpecs$dParagraph.paragraph_cases),
          bwdSuite2(dictLoadFile)(Test$dSpecs$dBwd.bwd_cases),
          suite3(Test$dSpecs$dGraphics.graphics_cases)
        ];
      };
    };
  };
};
const asTestSuite = dictMonadAff => {
  const $$void = dictMonadAff.MonadEffect0().Monad0().Bind1().Apply0().Functor0().map(v => {});
  return dictMonadError => dictLoadFile => suite => Data$dFunctor.arrayMap(m => Data$dTuple.$Tuple(m._1, $$void(m._2)))(suite(Data$dTuple.$Tuple(1, false)));
};
const allTests = dictMonadAff => {
  const benchmarks1 = benchmarks(dictMonadAff);
  const asTestSuite1 = asTestSuite(dictMonadAff);
  const linkingTests1 = linkingTests(dictMonadAff);
  return dictMonadError => {
    const benchmarks2 = benchmarks1(dictMonadError);
    const asTestSuite2 = asTestSuite1(dictMonadError);
    const linkingTests2 = linkingTests1(dictMonadError);
    return dictMonadReader => {
      const benchmarks3 = benchmarks2(dictMonadReader);
      const linkingTests3 = linkingTests2(dictMonadReader);
      return dictLoadFile => [...Data$dArray.concat(Data$dFunctor.arrayMap(asTestSuite2(dictLoadFile))(benchmarks3(dictLoadFile))), ...linkingTests3(dictLoadFile)];
    };
  };
};
const main = () => {
  for (
    const $0 of Data$dFunctor.arrayMap(m => Data$dTuple.$Tuple(m._1, m._2({fluidSrcPaths: Test$dUtil.fluidSrcPaths})))(allTests(Effect$dAff$dClass.monadAffReader(Effect$dAff$dClass.monadAffAff))(Control$dMonad$dReader$dTrans.monadErrorReaderT(Effect$dAff.monadErrorAff))(Control$dMonad$dReader$dTrans.monadReaderReaderT(Effect$dAff.monadAff))({
      loadFileFromPath: dictMonadError1 => dictMonadAff1 => x => {
        const $1 = File.loadFileAff.loadFileFromPath(Effect$dAff.monadErrorAff)(Effect$dAff$dClass.monadAffAff)(x);
        return v => $1;
      }
    }))
  ) {
    Test$dUtil$dMocha.executeTest($0)();
  }
};
const tests = dictMonadAff => allTests(dictMonadAff);
export {allTests, asTestSuite, benchmarks, filterSuite, linkingTests, main, scratchpad, tests};
