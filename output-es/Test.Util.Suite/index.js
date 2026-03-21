import * as $runtime from "../runtime.js";
import * as App$dFig from "../App.Fig/index.js";
import * as App$dUtil from "../App.Util/index.js";
import * as Control$dCategory from "../Control.Category/index.js";
import * as Data$dFunction from "../Data.Function/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dProfunctor$dStrong from "../Data.Profunctor.Strong/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as File from "../File/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
import * as Lattice from "../Lattice/index.js";
import * as Pretty from "../Pretty/index.js";
import * as Primitive$dDefs from "../Primitive.Defs/index.js";
import * as Test$dBenchmark$dUtil from "../Test.Benchmark.Util/index.js";
import * as Test$dUtil from "../Test.Util/index.js";
import * as Util$dMap from "../Util.Map/index.js";
import * as Val from "../Val/index.js";
const fanout = /* #__PURE__ */ Data$dProfunctor$dStrong.fanout(Control$dCategory.categoryFn)(Data$dProfunctor$dStrong.strongFn);
const botOf2 = {
  botOf: m => {
    if (m.tag === "Inert") { return App$dUtil.Inert; }
    if (m.tag === "Reactive") { return App$dUtil.$SelState("Reactive", {persistent: false, transient: false}); }
    $runtime.fail();
  }
};
const neg1 = {
  neg: x => {
    if (x.tag === "Inert") { return App$dUtil.Inert; }
    if (x.tag === "Reactive") { return App$dUtil.$SelState("Reactive", {persistent: !x._1.persistent, transient: !x._1.transient}); }
    $runtime.fail();
  }
};
const meetSemilatticeSelStates = {
  meet: v => v1 => {
    if (v1.tag === "Inert") { return App$dUtil.Inert; }
    if (v.tag === "Inert") { return App$dUtil.Inert; }
    if (v.tag === "Reactive" && v1.tag === "Reactive") {
      return App$dUtil.$SelState("Reactive", {persistent: v._1.persistent && v1._1.persistent, transient: v._1.transient && v1._1.transient});
    }
    $runtime.fail();
  }
};
const eqSelStates = {
  eq: x => y => {
    if (x.tag === "Inert") { return y.tag === "Inert"; }
    return x.tag === "Reactive" && y.tag === "Reactive" && x._1.persistent === y._1.persistent && x._1.transient === y._1.transient;
  }
};
const highlightableSelStates = /* #__PURE__ */ App$dUtil.highlightableSelStates(Val.highlightableBoolean)(Lattice.joinSemilatticeBoolean);
const checkEq = /* #__PURE__ */ (() => Test$dUtil.checkEq({botOf: Val.functorVal.map(botOf2.botOf)})({neg: x => Val.functorVal.map(neg1.neg)(x)})(Val.meetSemilatticeVal(meetSemilatticeSelStates))(Val.eqVal(eqSelStates))(Pretty.prettyVal(highlightableSelStates)))();
const checkEq1 = /* #__PURE__ */ (() => Test$dUtil.checkEq({botOf: Val.functorEnv.map(botOf2.botOf)})({neg: x => Foreign$dObject._fmapObject(x, Val.functorVal.map(neg1.neg))})(Val.meetSemilatticeEnv(meetSemilatticeSelStates))({
  eq: x => y => Foreign$dObject.eqObject(Val.eqVal(eqSelStates)).eq(x)(y)
})(Pretty.prettyEnv(highlightableSelStates)))();
const suite = dictMonadAff => dictMonadError => dictMonadReader => {
  const test = Test$dUtil.test(dictMonadReader);
  return dictLoadFile => {
    const test1 = test(dictLoadFile);
    return specs => v => {
      const $0 = v._2;
      const $1 = v._1;
      return Data$dFunctor.arrayMap(fanout(v1 => v1.file)(v1 => test1(v1.file)(Primitive$dDefs.primitives)({
        "δv": x => Data$dTuple.$Tuple(x, App$dUtil.Persistent),
        fwd_expect: v1.fwd_expect,
        bwd_expect: ""
      })(Data$dTuple.$Tuple($1, $0))(dictMonadAff)(dictMonadError)))(specs);
    };
  };
};
const linkedOutputsTest = dictMonadAff => {
  const MonadEffect0 = dictMonadAff.MonadEffect0();
  const Monad0 = MonadEffect0.Monad0();
  const Bind1 = Monad0.Bind1();
  const $0 = Bind1.Apply0().Functor0();
  const loadFig = App$dFig.loadFig(dictMonadAff);
  const logTimeWhen = Test$dBenchmark$dUtil.logTimeWhen(MonadEffect0);
  const $1 = Monad0.Applicative0();
  return dictMonadError => {
    const loadFig1 = loadFig(dictMonadError);
    const checkEq2 = checkEq(dictMonadError);
    return dictMonadReader => {
      const loadFig2 = loadFig1(dictMonadReader);
      return dictLoadFile => {
        const loadFile = File.loadFile(dictLoadFile)(Monad0)(dictMonadError)(dictMonadAff);
        const loadFig3 = loadFig2(dictLoadFile);
        return v => {
          const $2 = v.file;
          const $3 = v.spec;
          const $4 = v["δ_out"];
          return Bind1.bind(loadFile($3.fluidSrcPaths)($2))(fluidSrc => Bind1.bind($0.map(App$dFig.selectOutput($4))(loadFig3($3)(fluidSrc)))(fig => Bind1.bind(logTimeWhen(false)($2)(v1 => $1.pure(App$dFig.selectionResult(fig).v)))(v1 => Bind1.bind(checkEq2("selected")("expected")(Val.applyVal.apply(Val.applyVal.apply(Val.functorVal.map(App$dUtil.selStates)(Val.functorVal.map(App$dUtil.isInert)(v1)))(Val.functorVal.map(App$dUtil.isPersistent)(v1)))(Val.functorVal.map(App$dUtil.isTransient)(v1)))(v.out_expect(Val.functorVal.map(botOf2.botOf)(v1))._1))(() => $1.pure(fig)))));
        };
      };
    };
  };
};
const linkedOutputsSuite = dictMonadAff => {
  const linkedOutputsTest1 = linkedOutputsTest(dictMonadAff);
  const $$void = dictMonadAff.MonadEffect0().Monad0().Bind1().Apply0().Functor0().map(v => {});
  return dictMonadError => {
    const linkedOutputsTest2 = linkedOutputsTest1(dictMonadError);
    return dictMonadReader => {
      const linkedOutputsTest3 = linkedOutputsTest2(dictMonadReader);
      return dictLoadFile => {
        const linkedOutputsTest4 = linkedOutputsTest3(dictLoadFile);
        return testSpecs => Data$dFunctor.arrayMap(fanout(v => v.file)(x => $$void(linkedOutputsTest4(x))))(testSpecs);
      };
    };
  };
};
const linkedInputsTest = dictMonadAff => {
  const MonadEffect0 = dictMonadAff.MonadEffect0();
  const Monad0 = MonadEffect0.Monad0();
  const Bind1 = Monad0.Bind1();
  const $0 = Bind1.Apply0().Functor0();
  const loadFig = App$dFig.loadFig(dictMonadAff);
  const logTimeWhen = Test$dBenchmark$dUtil.logTimeWhen(MonadEffect0);
  const $1 = Monad0.Applicative0();
  return dictMonadError => {
    const loadFig1 = loadFig(dictMonadError);
    const checkEq2 = checkEq1(dictMonadError);
    return dictMonadReader => {
      const loadFig2 = loadFig1(dictMonadReader);
      return dictLoadFile => {
        const loadFile = File.loadFile(dictLoadFile)(Monad0)(dictMonadError)(dictMonadAff);
        const loadFig3 = loadFig2(dictLoadFile);
        return v => {
          const $2 = v.file;
          const $3 = v.spec;
          const $4 = v["δ_in"];
          return Bind1.bind(loadFile($3.fluidSrcPaths)($2))(fluidSrc => Bind1.bind($0.map(App$dFig.selectInput($4._1)($4._2))(loadFig3($3)(fluidSrc)))(fig => Bind1.bind(logTimeWhen(false)($2)(v1 => $1.pure(App$dFig.selectionResult(fig)[
            "γ"
          ])))(γ => Bind1.bind(checkEq2("selected")("expected")(Util$dMap.intersectionWith_Object(Data$dFunction.apply)(Foreign$dObject._fmapObject(
            Util$dMap.intersectionWith_Object(Data$dFunction.apply)(Foreign$dObject._fmapObject(
              Foreign$dObject._fmapObject(Foreign$dObject._fmapObject(γ, Val.functorVal.map(App$dUtil.isInert)), Val.functorVal.map(App$dUtil.selStates)),
              Val.applyVal.apply
            ))(Foreign$dObject._fmapObject(γ, Val.functorVal.map(App$dUtil.isPersistent))),
            Val.applyVal.apply
          ))(Foreign$dObject._fmapObject(γ, Val.functorVal.map(App$dUtil.isTransient))))(v.in_expect(Foreign$dObject._fmapObject(γ, Val.functorVal.map(botOf2.botOf)))._1))(() => $1.pure(fig)))));
        };
      };
    };
  };
};
const linkedInputsSuite = dictMonadAff => {
  const linkedInputsTest1 = linkedInputsTest(dictMonadAff);
  const $$void = dictMonadAff.MonadEffect0().Monad0().Bind1().Apply0().Functor0().map(v => {});
  return dictMonadError => {
    const linkedInputsTest2 = linkedInputsTest1(dictMonadError);
    return dictMonadReader => {
      const linkedInputsTest3 = linkedInputsTest2(dictMonadReader);
      return dictLoadFile => {
        const linkedInputsTest4 = linkedInputsTest3(dictLoadFile);
        return testSpecs => Data$dFunctor.arrayMap(fanout(v => v.file)(x => $$void(linkedInputsTest4(x))))(testSpecs);
      };
    };
  };
};
const bwdSuite = dictMonadAff => {
  const Monad0 = dictMonadAff.MonadEffect0().Monad0();
  return dictMonadError => dictMonadReader => {
    const test = Test$dUtil.test(dictMonadReader);
    return dictLoadFile => {
      const loadFile = File.loadFile(dictLoadFile)(Monad0)(dictMonadError)(dictMonadAff);
      const test1 = test(dictLoadFile);
      return specs => v => {
        const $0 = v._2;
        const $1 = v._1;
        return Data$dFunctor.arrayMap(fanout(x => Data$dShow.showStringImpl("slicing/" + x.file))(v1 => {
          const $2 = v1.file;
          const $3 = v1.fwd_expect;
          const $4 = v1["δv"];
          return Monad0.Bind1().bind(loadFile(["test/fluid"])("slicing/" + v1.bwd_expect_file))(bwd_expect => test1("slicing/" + $2)(Primitive$dDefs.primitives)({
            "δv": $4,
            fwd_expect: $3,
            bwd_expect
          })(Data$dTuple.$Tuple($1, $0))(dictMonadAff)(dictMonadError));
        }))(specs);
      };
    };
  };
};
export {
  botOf2,
  bwdSuite,
  checkEq,
  checkEq1,
  eqSelStates,
  fanout,
  highlightableSelStates,
  linkedInputsSuite,
  linkedInputsTest,
  linkedOutputsSuite,
  linkedOutputsTest,
  meetSemilatticeSelStates,
  neg1,
  suite
};
