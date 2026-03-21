import * as $runtime from "../runtime.js";
import * as App$dUtil from "../App.Util/index.js";
import * as App$dUtil$dSelector from "../App.Util.Selector/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Val from "../Val/index.js";
const select = b => Data$dTuple.$Tuple(
  Val.functorVal.map(v => {
    if (v.tag === "Inert") { return App$dUtil.Inert; }
    if (v.tag === "Reactive") { return App$dUtil.$SelState("Reactive", {persistent: !v._1.persistent, transient: v._1.transient}); }
    $runtime.fail();
  })(b),
  App$dUtil.Persistent
);
const movingAverages_spec = {
  spec: {fluidSrcPaths: ["fluid", "test/fluid"], inputs: ["methane"], query: Data$dMaybe.Nothing, linking: true, rowFilter: Data$dMaybe.Nothing},
  "δ_out": x => Data$dTuple.$Tuple(x, App$dUtil.Persistent),
  out_expect: x => Data$dTuple.$Tuple(x, App$dUtil.Persistent),
  file: "linkedOutputs/moving-average.fld"
};
const linkedOutputs_spec2 = {
  spec: {fluidSrcPaths: ["fluid", "test/fluid"], inputs: ["nonRenewables"], query: Data$dMaybe.Nothing, linking: true, rowFilter: Data$dMaybe.Nothing},
  "δ_out": /* #__PURE__ */ App$dUtil$dSelector.multiViewEntry(0)(/* #__PURE__ */ App$dUtil$dSelector.constrArg("BarChart")(0)(x => App$dUtil$dSelector.barSegment(4)(3)(select)(App$dUtil$dSelector.barSegment(4)(1)(select)(App$dUtil$dSelector.barSegment(3)(2)(select)(x)._1)._1))),
  out_expect: /* #__PURE__ */ (() => {
    const $0 = App$dUtil$dSelector.multiViewEntry(0)(App$dUtil$dSelector.constrArg("BarChart")(0)(x => App$dUtil$dSelector.barSegment(4)(3)(select)(App$dUtil$dSelector.barSegment(4)(1)(select)(App$dUtil$dSelector.barSegment(3)(2)(select)(x)._1)._1)));
    const $1 = App$dUtil$dSelector.multiViewEntry(1)(App$dUtil$dSelector.constrArg("ScatterPlot")(0)(x => App$dUtil$dSelector.dictVal("points")(App$dUtil$dSelector.listElement(6)(App$dUtil$dSelector.dictVal("y")(select)))(App$dUtil$dSelector.dictVal("points")(App$dUtil$dSelector.listElement(4)(App$dUtil$dSelector.dictVal("y")(select)))(x)._1)));
    return x => $1($0(x)._1);
  })(),
  file: "slicing/linkedOutputs/stacked-bar-scatter-plot.fld"
};
const linkedOutputs_spec1 = {
  spec: {fluidSrcPaths: ["fluid", "test/fluid"], inputs: ["renewables"], query: Data$dMaybe.Nothing, linking: true, rowFilter: Data$dMaybe.Nothing},
  "δ_out": /* #__PURE__ */ App$dUtil$dSelector.multiViewEntry(0)(/* #__PURE__ */ App$dUtil$dSelector.constrArg("BarChart")(0)(/* #__PURE__ */ App$dUtil$dSelector.barSegment(1)(0)(select))),
  out_expect: /* #__PURE__ */ (() => {
    const $0 = App$dUtil$dSelector.multiViewEntry(1)(App$dUtil$dSelector.constrArg("LineChart")(0)(App$dUtil$dSelector.dictVal("plots")(x => App$dUtil$dSelector.listElement(3)(App$dUtil$dSelector.linePoint(2)(App$dUtil$dSelector.dictVal("y")(select)))(App$dUtil$dSelector.listElement(2)(App$dUtil$dSelector.linePoint(2)(App$dUtil$dSelector.dictVal("y")(select)))(App$dUtil$dSelector.listElement(1)(App$dUtil$dSelector.linePoint(2)(App$dUtil$dSelector.dictVal("y")(select)))(App$dUtil$dSelector.listElement(0)(App$dUtil$dSelector.linePoint(2)(App$dUtil$dSelector.dictVal("y")(select)))(x)._1)._1)._1))));
    return x => $0(App$dUtil$dSelector.constrArg("MultiView")(0)(App$dUtil$dSelector.listElement(0)(App$dUtil$dSelector.constrArg("BarChart")(0)(App$dUtil$dSelector.barSegment(1)(0)(select))))(x)._1);
  })(),
  file: "slicing/linkedOutputs/bar-chart-line-chart.fld"
};
const linkedOutputs_cases = [
  {
    spec: {fluidSrcPaths: ["fluid", "test/fluid"], inputs: ["data"], query: Data$dMaybe.Nothing, linking: true, rowFilter: Data$dMaybe.Nothing},
    "δ_out": /* #__PURE__ */ App$dUtil$dSelector.constrArg("Pair")(1)(select),
    out_expect: select,
    file: "linkedOutputs/pairs.fld"
  },
  {
    spec: {fluidSrcPaths: ["fluid", "test/fluid"], inputs: ["data"], query: Data$dMaybe.Nothing, linking: true, rowFilter: Data$dMaybe.Nothing},
    "δ_out": /* #__PURE__ */ App$dUtil$dSelector.constrArg("Pair")(0)(/* #__PURE__ */ App$dUtil$dSelector.matrixElement(1)(1)(select)),
    out_expect: /* #__PURE__ */ (() => {
      const $0 = App$dUtil$dSelector.constrArg("Pair")(0)(x => App$dUtil$dSelector.matrixElement(1)(4)(select)(App$dUtil$dSelector.matrixElement(1)(3)(select)(App$dUtil$dSelector.matrixElement(1)(2)(select)(App$dUtil$dSelector.matrixElement(1)(1)(select)(App$dUtil$dSelector.matrixElement(1)(0)(select)(x)._1)._1)._1)._1));
      const $1 = App$dUtil$dSelector.constrArg("Pair")(1)(x => App$dUtil$dSelector.matrixElement(2)(2)(select)(App$dUtil$dSelector.matrixElement(2)(1)(select)(App$dUtil$dSelector.matrixElement(2)(0)(select)(App$dUtil$dSelector.matrixElement(1)(2)(select)(App$dUtil$dSelector.matrixElement(1)(1)(select)(App$dUtil$dSelector.matrixElement(1)(0)(select)(App$dUtil$dSelector.matrixElement(0)(2)(select)(App$dUtil$dSelector.matrixElement(0)(1)(select)(App$dUtil$dSelector.matrixElement(0)(0)(select)(x)._1)._1)._1)._1)._1)._1)._1)._1));
      return x => $1($0(x)._1);
    })(),
    file: "linkedOutputs/convolution.fld"
  },
  linkedOutputs_spec1,
  linkedOutputs_spec2,
  movingAverages_spec
];
export {linkedOutputs_cases, linkedOutputs_spec1, linkedOutputs_spec2, movingAverages_spec, select};
