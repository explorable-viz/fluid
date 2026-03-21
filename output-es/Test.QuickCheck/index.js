// | This module is a partial port of the Haskell QuickCheck library.
// |
// | QuickCheck provides a way to write _property-based_ tests.
// |
// | The `Arbitrary` and `CoArbitrary` type classes allow us to create
// | random data with which we can run our tests. This module provides
// | instances of both classes for PureScript's core data structures,
// | as well as functions for writing new instances.
// |
// | Test suites can use the `quickCheck` and `quickCheckPure` functions
// | to test properties.
// |
// | For example:
// |
// | ```purescript
// | main = quickCheck \n -> n + 1 > n
// | ```
import * as $runtime from "../runtime.js";
import * as Control$dMonad$dRec$dClass from "../Control.Monad.Rec.Class/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Effect from "../Effect/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
import * as Effect$dException from "../Effect.Exception/index.js";
import * as Random$dLCG from "../Random.LCG/index.js";
import * as Test$dQuickCheck$dGen from "../Test.QuickCheck.Gen/index.js";
const $Result = (tag, _1) => ({tag, _1});
const for_ = /* #__PURE__ */ Data$dFoldable.for_(Effect.applicativeEffect)(Data$dFoldable.foldableFirst);
const Success = /* #__PURE__ */ $Result("Success");
const Failed = value0 => $Result("Failed", value0);
const withHelp = v => v1 => {
  if (v) { return Success; }
  return $Result("Failed", v1);
};
const testableResult = /* #__PURE__ */ (() => ({test: Test$dQuickCheck$dGen.applicativeGen.pure}))();
const testableBoolean = {
  test: v => {
    if (v) { return Test$dQuickCheck$dGen.applicativeGen.pure(Success); }
    return Test$dQuickCheck$dGen.applicativeGen.pure($Result("Failed", "Test returned false"));
  }
};
const test = dict => dict.test;
const testableFunction = dictArbitrary => {
  const arbitrary = dictArbitrary.arbitrary;
  return dictTestable => ({test: f => Test$dQuickCheck$dGen.bindStateT.bind(arbitrary)(x => dictTestable.test(f(x)))});
};
const testableGen = dictTestable => (
  {
    test: (() => {
      const $0 = dictTestable.test;
      return a => Test$dQuickCheck$dGen.bindStateT.bind(a)($0);
    })()
  }
);
const showResult = {
  show: v => {
    if (v.tag === "Success") { return "Success"; }
    if (v.tag === "Failed") { return "Failed: " + v._1; }
    $runtime.fail();
  }
};
const quickCheckWithSeed = dictTestable => initialSeed => n => prop => {
  const $0 = v => {
    if (v.index === n) { return Control$dMonad$dRec$dClass.$Step("Done", v); }
    const v1 = dictTestable.test(prop)({newSeed: v.seed, size: 10});
    if (v1._1.tag === "Success") {
      return Control$dMonad$dRec$dClass.$Step("Loop", {seed: v1._2.newSeed, index: v.index + 1 | 0, successes: v.successes + 1 | 0, firstFailure: v.firstFailure});
    }
    if (v1._1.tag === "Failed") {
      return Control$dMonad$dRec$dClass.$Step(
        "Loop",
        {
          seed: v1._2.newSeed,
          index: v.index + 1 | 0,
          successes: v.successes,
          firstFailure: v.firstFailure.tag === "Just" ? v.firstFailure : Data$dMaybe.$Maybe("Just", {index: v.index, message: v1._1._1, seed: v.seed})
        }
      );
    }
    $runtime.fail();
  };
  const go = go$a0$copy => {
    let go$a0 = go$a0$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0;
      if (v.tag === "Loop") {
        go$a0 = $0(v._1);
        continue;
      }
      if (v.tag === "Done") {
        go$c = false;
        go$r = v._1;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  const result = go($0({seed: initialSeed, index: 0, successes: 0, firstFailure: Data$dMaybe.Nothing}));
  const $1 = Effect$dConsole.log(Data$dShow.showIntImpl(result.successes) + "/" + Data$dShow.showIntImpl(n) + " test(s) passed.");
  return () => {
    $1();
    return for_(result.firstFailure)(v => Effect$dException.throwException(Effect$dException.error("Test " + Data$dShow.showIntImpl(v.index + 1 | 0) + " (seed " + Data$dShow.showIntImpl(v.seed) + ") failed: \n" + v.message)))();
  };
};
const quickCheckPure$p = dictTestable => s => n => prop => {
  const $0 = v => {
    if (v.index === n) {
      return Control$dMonad$dRec$dClass.$Step(
        "Done",
        (() => {
          const go = go$a0$copy => go$a1$copy => {
            let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
            while (go$c) {
              const v$1 = go$a0, v1 = go$a1;
              if (v1.tag === "Nil") {
                go$c = false;
                go$r = v$1;
                continue;
              }
              if (v1.tag === "Cons") {
                go$a0 = Data$dList$dTypes.$List("Cons", v1._1, v$1);
                go$a1 = v1._2;
                continue;
              }
              $runtime.fail();
            }
            return go$r;
          };
          return go(Data$dList$dTypes.Nil)(v.results);
        })()
      );
    }
    const v1 = dictTestable.test(prop)({newSeed: v.seed, size: 10});
    return Control$dMonad$dRec$dClass.$Step(
      "Loop",
      {seed: v1._2.newSeed, index: v.index + 1 | 0, results: Data$dList$dTypes.$List("Cons", Data$dTuple.$Tuple(v.seed, v1._1), v.results)}
    );
  };
  const go = go$a0$copy => {
    let go$a0 = go$a0$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0;
      if (v.tag === "Loop") {
        go$a0 = $0(v._1);
        continue;
      }
      if (v.tag === "Done") {
        go$c = false;
        go$r = v._1;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go($0({seed: s, index: 0, results: Data$dList$dTypes.Nil}));
};
const quickCheckPure = dictTestable => s => n => prop => Data$dList$dTypes.listMap(Data$dTuple.snd)(quickCheckPure$p(dictTestable)(s)(n)(prop));
const quickCheckGenWithSeed = dictTestable => quickCheckWithSeed({
  test: (() => {
    const $0 = dictTestable.test;
    return a => Test$dQuickCheck$dGen.bindStateT.bind(a)($0);
  })()
});
const quickCheckGenPure$p = dictTestable => quickCheckPure$p({
  test: (() => {
    const $0 = dictTestable.test;
    return a => Test$dQuickCheck$dGen.bindStateT.bind(a)($0);
  })()
});
const quickCheckGenPure = dictTestable => quickCheckPure({
  test: (() => {
    const $0 = dictTestable.test;
    return a => Test$dQuickCheck$dGen.bindStateT.bind(a)($0);
  })()
});
const quickCheck$p = dictTestable => n => prop => () => {
  const seed = Random$dLCG.randomSeed();
  return quickCheckWithSeed(dictTestable)(seed)(n)(prop)();
};
const quickCheckGen$p = dictTestable => quickCheck$p({
  test: (() => {
    const $0 = dictTestable.test;
    return a => Test$dQuickCheck$dGen.bindStateT.bind(a)($0);
  })()
});
const quickCheck = dictTestable => prop => () => {
  const seed = Random$dLCG.randomSeed();
  return quickCheckWithSeed(dictTestable)(seed)(100)(prop)();
};
const quickCheckGen = dictTestable => quickCheck({
  test: (() => {
    const $0 = dictTestable.test;
    return a => Test$dQuickCheck$dGen.bindStateT.bind(a)($0);
  })()
});
const printSummary = summary => Data$dShow.showIntImpl(summary.successes) + "/" + Data$dShow.showIntImpl(summary.total) + (summary.total === 1 ? " test passed." : " tests passed.");
const checkResults = /* #__PURE__ */ (() => {
  const go = go$a0$copy => go$a1$copy => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
    while (go$c) {
      const b = go$a0, v = go$a1;
      if (v.tag === "Nil") {
        go$c = false;
        go$r = b;
        continue;
      }
      if (v.tag === "Cons") {
        go$a0 = Data$dTuple.$Tuple(
          b._1 + 1 | 0,
          (() => {
            if (v._1._2.tag === "Success") { return {total: b._2.total + 1 | 0, successes: b._2.successes + 1 | 0, failures: b._2.failures}; }
            if (v._1._2.tag === "Failed") {
              return {
                total: b._2.total + 1 | 0,
                successes: b._2.successes,
                failures: Data$dList$dTypes.$List("Cons", {index: b._1, seed: v._1._1, message: v._1._2._1}, b._2.failures)
              };
            }
            $runtime.fail();
          })()
        );
        go$a1 = v._2;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  const $0 = go(Data$dTuple.$Tuple(0, {total: 0, successes: 0, failures: Data$dList$dTypes.Nil}));
  return x => $0(x)._2;
})();
const assertNotEquals = dictEq => dictShow => a => b => {
  const $0 = dictShow.show(a) + " == " + dictShow.show(b);
  if (!dictEq.eq(a)(b)) { return Success; }
  return $Result("Failed", $0);
};
const assertLessThanEq = dictOrd => dictShow => a => b => {
  const $0 = dictShow.show(a) + " > " + dictShow.show(b);
  if (dictOrd.compare(a)(b) !== "GT") { return Success; }
  return $Result("Failed", $0);
};
const assertLessThan = dictOrd => dictShow => a => b => {
  const $0 = dictShow.show(a) + " >= " + dictShow.show(b);
  if (dictOrd.compare(a)(b) === "LT") { return Success; }
  return $Result("Failed", $0);
};
const assertGreaterThanEq = dictOrd => dictShow => a => b => {
  const $0 = dictShow.show(a) + " < " + dictShow.show(b);
  if (dictOrd.compare(a)(b) !== "LT") { return Success; }
  return $Result("Failed", $0);
};
const assertGreaterThan = dictOrd => dictShow => a => b => {
  const $0 = dictShow.show(a) + " <= " + dictShow.show(b);
  if (dictOrd.compare(a)(b) === "GT") { return Success; }
  return $Result("Failed", $0);
};
const assertEquals = dictEq => dictShow => a => b => {
  const $0 = dictShow.show(a) + " /= " + dictShow.show(b);
  if (dictEq.eq(a)(b)) { return Success; }
  return $Result("Failed", $0);
};
export {
  $Result,
  Failed,
  Success,
  assertEquals,
  assertGreaterThan,
  assertGreaterThanEq,
  assertLessThan,
  assertLessThanEq,
  assertNotEquals,
  checkResults,
  for_,
  printSummary,
  quickCheck,
  quickCheck$p,
  quickCheckGen,
  quickCheckGen$p,
  quickCheckGenPure,
  quickCheckGenPure$p,
  quickCheckGenWithSeed,
  quickCheckPure,
  quickCheckPure$p,
  quickCheckWithSeed,
  showResult,
  test,
  testableBoolean,
  testableFunction,
  testableGen,
  testableResult,
  withHelp
};
