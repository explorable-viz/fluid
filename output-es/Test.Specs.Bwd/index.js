import * as $runtime from "../runtime.js";
import * as App$dUtil from "../App.Util/index.js";
import * as App$dUtil$dSelector from "../App.Util.Selector/index.js";
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
const select$p = x => Data$dTuple.$Tuple(!x, App$dUtil.Persistent);
const bwd_cases = [
  {file: "add.fld", bwd_expect_file: "add.expect.fld", "δv": select, fwd_expect: "⸨8⸩"},
  {file: "array/lookup.fld", bwd_expect_file: "array/lookup.expect.fld", "δv": select, fwd_expect: "⸨14⸩"},
  {file: "array/dims.fld", bwd_expect_file: "array/dims.expect.fld", "δv": select, fwd_expect: "⸨(⸨3⸩, ⸨3⸩)⸩"},
  {
    file: "convolution/edgeDetect.fld",
    bwd_expect_file: "convolution/edgeDetect.expect.fld",
    "δv": /* #__PURE__ */ App$dUtil$dSelector.matrixElement(0)(0)(select),
    fwd_expect: "⸨0⸩, -1, 2, 0, -1,\n0, 3, -2, 3, -2,\n-1, 1, -5, 0, 4,\n1, -1, 4, 0, -4,\n1, 0, -3, 2, 0"
  },
  {
    file: "convolution/emboss.fld",
    bwd_expect_file: "convolution/emboss.expect.fld",
    "δv": /* #__PURE__ */ App$dUtil$dSelector.matrixElement(0)(0)(select),
    fwd_expect: "⸨5⸩, 4, 2, 5, 2,\n3, 1, 2, -1, -2,\n3, 0, 1, 0, -1,\n2, 1, -2, 0, 0,\n1, 0, -1, -1, -2"
  },
  {
    file: "convolution/gaussian.fld",
    bwd_expect_file: "convolution/gaussian.expect.fld",
    "δv": /* #__PURE__ */ App$dUtil$dSelector.matrixElement(0)(0)(select),
    fwd_expect: "⸨38⸩, 37, 28, 30, 38,\n38, 36, 46, 31, 34,\n37, 41, 54, 34, 20,\n21, 35, 31, 31, 42,\n13, 32, 35, 19, 26"
  },
  {
    file: "matrix/matmul.fld",
    bwd_expect_file: "matrix/matmul.expect.fld",
    "δv": /* #__PURE__ */ App$dUtil$dSelector.constrArg("Pair")(0)(/* #__PURE__ */ App$dUtil$dSelector.matrixElement(0)(0)(select)),
    fwd_expect: "(@doc(Paragraph(\"Intermediate\" :| \"matrix\" :| [])) ⸨22⸩, 28,\n49, 64, @doc(Paragraph(\"Intermediate\" :| \"matrix\" :| [])) 9, 12, 15,\n19, 26, 33,\n29, 40, 51)"
  },
  {file: "dict/create.fld", bwd_expect_file: "dict/create.expect.fld", "δv": /* #__PURE__ */ App$dUtil$dSelector.dictKey("ab")(select$p), fwd_expect: "{ a: 5, ⸨ab⸩: 6 }"},
  {
    file: "dict/difference.fld",
    bwd_expect_file: "dict/difference.expect.fld",
    "δv": v => {
      if (v._3.tag === "Dictionary") {
        return Data$dTuple.$Tuple(
          Val.$Val(
            (() => {
              if (v._1.tag === "Inert") { return App$dUtil.Inert; }
              if (v._1.tag === "Reactive") { return App$dUtil.$SelState("Reactive", {persistent: !v._1._1.persistent, transient: v._1._1.transient}); }
              $runtime.fail();
            })(),
            v._2,
            Val.$BaseVal("Dictionary", v._3._1)
          ),
          App$dUtil.Persistent
        );
      }
      $runtime.fail();
    },
    fwd_expect: "⸨{ a: 5 }⸩"
  },
  {
    file: "dict/disjointUnion.fld",
    bwd_expect_file: "dict/disjointUnion.expect.fld",
    "δv": x => App$dUtil$dSelector.dictVal("c")(select)(App$dUtil$dSelector.dictKey("a")(select$p)(x)._1),
    fwd_expect: "{ ⸨a⸩: 5, b: 6, c: ⸨7⸩ }"
  },
  {file: "dict/foldl_with_index.fld", bwd_expect_file: "dict/foldl_with_index.expect.fld", "δv": select, fwd_expect: "⸨0⸩"},
  {
    file: "dict/intersectionWith.fld",
    bwd_expect_file: "dict/intersectionWith.expect.fld",
    "δv": x => App$dUtil$dSelector.dictVal("c")(select)(App$dUtil$dSelector.dictVal("b")(select)(x)._1),
    fwd_expect: "{ b: ⸨0⸩, c: ⸨20⸩ }"
  },
  {file: "dict/get.fld", bwd_expect_file: "dict/get.expect.fld", "δv": select, fwd_expect: "⸨0⸩"},
  {file: "dict/map.fld", bwd_expect_file: "dict/map.expect.fld", "δv": select, fwd_expect: "⸨20⸩"},
  {file: "divide.fld", bwd_expect_file: "divide.expect.fld", "δv": select, fwd_expect: "⸨40.22222222222222⸩"},
  {file: "dict/match.fld", bwd_expect_file: "dict/match.expect.fld", "δv": select, fwd_expect: ""},
  {
    file: "dtw/compute-dtw.fld",
    bwd_expect_file: "dtw/compute-dtw.expect.fld",
    fwd_expect: "(0, 0) :| ⸨(⸨1⸩, ⸨1⸩)⸩ :| (1, 2) :| (2, 3) :| (3, 4) :| (4, 5) :| (4, 6) :| []",
    "δv": /* #__PURE__ */ App$dUtil$dSelector.listElement(1)(select)
  },
  {
    file: "dtw/average-series.fld",
    bwd_expect_file: "dtw/average-series.expect.fld",
    fwd_expect: "2.5 :| 0.5 :| ⸨0.5⸩ :| 2.5 :| 2.5 :| 1.0 :| 0.5 :| []",
    "δv": /* #__PURE__ */ App$dUtil$dSelector.listElement(2)(select)
  },
  {file: "filter.fld", bwd_expect_file: "filter.expect.fld", "δv": /* #__PURE__ */ App$dUtil$dSelector.listCell(0)(select$p), fwd_expect: "⸨⸨8⸩ :| 7 :| []⸩"},
  {
    file: "intersperse.fld",
    bwd_expect_file: "intersperse-1.expect.fld",
    "δv": /* #__PURE__ */ App$dUtil$dSelector.listCell(1)(select$p),
    fwd_expect: "1 :| ⸨0 :| 2 :| 0 :| 3 :| []⸩"
  },
  {
    file: "intersperse.fld",
    bwd_expect_file: "intersperse-2.expect.fld",
    "δv": /* #__PURE__ */ App$dUtil$dSelector.listCell(2)(select$p),
    fwd_expect: "⸨1 :| 0 :| ⸨2 :| 0 :| 3 :| []⸩⸩"
  },
  {file: "length.fld", bwd_expect_file: "length.expect.fld", "δv": select, fwd_expect: "⸨5⸩"},
  {
    file: "list-comp.fld",
    bwd_expect_file: "list-comp-1.expect.fld",
    "δv": /* #__PURE__ */ App$dUtil$dSelector.listCell(1)(select$p),
    fwd_expect: "6.2 :| ⸨260 :| 19.9 :| 91 :| []⸩"
  },
  {
    file: "list-comp.fld",
    bwd_expect_file: "list-comp-2.expect.fld",
    "δv": /* #__PURE__ */ App$dUtil$dSelector.listCell(2)(select$p),
    fwd_expect: "6.2 :| 260 :| ⸨19.9 :| 91 :| []⸩"
  },
  {
    file: "lookup.fld",
    bwd_expect_file: "lookup.expect.fld",
    "δv": v => {
      if (v._3.tag === "Constr" && v._3._1 === "Some") {
        return Data$dTuple.$Tuple(
          Val.$Val(
            (() => {
              if (v._1.tag === "Inert") { return App$dUtil.Inert; }
              if (v._1.tag === "Reactive") { return App$dUtil.$SelState("Reactive", {persistent: !v._1._1.persistent, transient: v._1._1.transient}); }
              $runtime.fail();
            })(),
            v._2,
            Val.$BaseVal("Constr", v._3._1, v._3._2)
          ),
          App$dUtil.Persistent
        );
      }
      $runtime.fail();
    },
    fwd_expect: "⸨Some(\"Germany\")⸩"
  },
  {
    file: "map.fld",
    bwd_expect_file: "map.expect.fld",
    "δv": x => App$dUtil$dSelector.listCell(1)(select$p)(App$dUtil$dSelector.listCell(0)(select$p)(x)._1),
    fwd_expect: "⸨5 :| ⸨6 :| []⸩⸩"
  },
  {
    file: "matrix-update.fld",
    bwd_expect_file: "matrix-update.expect.fld",
    fwd_expect: "15, 13, 6, 9, 16,\n12, ⸨4000⸩, 15, 4, 13,\n14, 9, 20, 8, 1,\n4, 10, 3, 7, 19,\n3, 11, 15, 2, 9",
    "δv": /* #__PURE__ */ App$dUtil$dSelector.matrixElement(1)(1)(select)
  },
  {file: "multiply.fld", bwd_expect_file: "multiply.expect.fld", "δv": select, fwd_expect: "⸨0⸩"},
  {file: "nth.fld", bwd_expect_file: "nth.expect.fld", "δv": select, fwd_expect: "⸨4⸩"},
  {
    file: "output-not-source.fld",
    bwd_expect_file: "output-not-source.expect.fld",
    fwd_expect: "(⸨3⸩, ⸨True⸩)",
    "δv": /* #__PURE__ */ App$dUtil$dSelector.constrArg("Pair")(1)(select)
  },
  {
    file: "section-5-example.fld",
    bwd_expect_file: "section-5-example-1.expect.fld",
    "δv": /* #__PURE__ */ App$dUtil$dSelector.listCell(0)(select$p),
    fwd_expect: "⸨88 :| 6 :| 4 :| []⸩"
  },
  {
    file: "section-5-example.fld",
    bwd_expect_file: "section-5-example-2.expect.fld",
    "δv": /* #__PURE__ */ App$dUtil$dSelector.listElement(1)(select),
    fwd_expect: "⸨88⸩ :| ⸨6⸩ :| ⸨4⸩ :| []"
  },
  {
    file: "section-5-example.fld",
    bwd_expect_file: "section-5-example-3.expect.fld",
    "δv": /* #__PURE__ */ App$dUtil$dSelector.listCell(2)(select$p),
    fwd_expect: "88 :| 6 :| ⸨4 :| []⸩"
  },
  {
    file: "zeros.fld",
    bwd_expect_file: "zeros-1.expect.fld",
    "δv": x => App$dUtil$dSelector.listCell(2)(select$p)(App$dUtil$dSelector.listCell(0)(select$p)(x)._1),
    fwd_expect: "⸨0 :| 0 :| ⸨[]⸩⸩"
  },
  {file: "zeros.fld", bwd_expect_file: "zeros-2.expect.fld", "δv": /* #__PURE__ */ App$dUtil$dSelector.listCell(2)(select$p), fwd_expect: "0 :| 0 :| ⸨[]⸩"},
  {
    file: "zipWith.fld",
    bwd_expect_file: "zipWith-1.expect.fld",
    "δv": /* #__PURE__ */ App$dUtil$dSelector.listElement(1)(x => Data$dTuple.$Tuple(
      Val.functorVal.map(x$1 => {
        if (x$1.tag === "Inert") { return App$dUtil.Inert; }
        if (x$1.tag === "Reactive") { return App$dUtil.$SelState("Reactive", {persistent: !x$1._1.persistent, transient: !x$1._1.transient}); }
        $runtime.fail();
      })(x),
      App$dUtil.Persistent
    )),
    fwd_expect: "13.0 :| ⸨25.0⸩ :| 41.0 :| []"
  },
  {
    file: "linkedOutputs/bar-chart-line-chart.fld",
    bwd_expect_file: "linkedOutputs/bar-chart-line-chart.expect.fld",
    "δv": /* #__PURE__ */ App$dUtil$dSelector.multiViewEntry(0)(/* #__PURE__ */ App$dUtil$dSelector.constrArg("BarChart")(0)(/* #__PURE__ */ App$dUtil$dSelector.barSegment(1)(0)(select))),
    fwd_expect: "MultiView(BarChart({\n  caption: \"Total output by country\",\n  legend: True,\n  size: { height: 185, width: 275 },\n  stackedBars: { segments: { y: \"output\", z: 295.3 } :| [], x: \"China\" } :| {\n    segments: { y: \"output\", z: ⸨196.7⸩ } :| [],\n    x: \"USA\"\n  } :| { segments: { y: \"output\", z: 97.69999999999999 } :| [], x: \"Germany\" } :| [],\n  tickLabels: { x: Default, y: Default }\n}) :| LineChart({\n  caption: \"Output of USA relative to China\",\n  plots: LinePlot({\n    name: \"Bio\",\n    points: { x: 2013, y: 2.5483870967741935 } :| { x: 2014, y: 1.61 } :| {\n      x: 2015,\n      y: 1.6213592233009706\n    } :| { x: 2016, y: 1.4000000000000001 } :| {\n      x: 2017,\n      y: 1.1208053691275166\n    } :| { x: 2018, y: 0.9101123595505617 } :| []\n  }) :| LinePlot({\n    name: \"Hydro\",\n    points: { x: 2013, y: 0.3 } :| { x: 2014, y: 0.28214285714285714 } :| {\n      x: 2015,\n      y: 0.8333333333333334\n    } :| { x: 2016, y: 0.26229508196721313 } :| {\n      x: 2017,\n      y: 0.25559105431309903\n    } :| { x: 2018, y: 0.2484472049689441 } :| []\n  }) :| LinePlot({\n    name: \"Solar\",\n    points: { x: 2013, y: 0.6080402010050252 } :| {\n      x: 2014,\n      y: 0.6428571428571429\n    } :| { x: 2015, y: 0.5909090909090909 } :| {\n      x: 2016,\n      y: 0.5324675324675324\n    } :| { x: 2017, y: 0.3893129770992366 } :| {\n      x: 2018,\n      y: 0.3522727272727273\n    } :| []\n  }) :| LinePlot({\n    name: \"Wind\",\n    points: { x: 2013, y: 0.6703296703296703 } :| {\n      x: 2014,\n      y: 0.5739130434782609\n    } :| { x: 2015, y: 0.5103448275862069 } :| {\n      x: 2016,\n      y: 0.48520710059171596\n    } :| { x: 2017, y: 0.4734042553191489 } :| {\n      x: 2018,\n      y: 0.45714285714285713\n    } :| []\n  }) :| [],\n  size: { height: 285, width: 330 },\n  tickLabels: { x: Default, y: Default }\n}) :| [])"
  },
  {
    file: "linkedOutputs/stacked-bar-scatter-plot.fld",
    bwd_expect_file: "linkedOutputs/stacked-bar-scatter-plot.expect.fld",
    "δv": /* #__PURE__ */ App$dUtil$dSelector.multiViewEntry(0)(/* #__PURE__ */ App$dUtil$dSelector.constrArg("BarChart")(0)(x => App$dUtil$dSelector.barSegment(4)(3)(select)(App$dUtil$dSelector.barSegment(4)(1)(select)(App$dUtil$dSelector.barSegment(3)(2)(select)(x)._1)._1))),
    fwd_expect: "MultiView(BarChart({\n  caption: \"Non-renewables by country\",\n  legend: True,\n  size: { height: 185, width: 275 },\n  stackedBars: {\n    segments: { y: \"BRA\", z: 151.05 } :| { y: \"EGY\", z: 159.93 } :| {\n      y: \"IND\",\n      z: 1060.1799999999998\n    } :| { y: \"JPN\", z: 928.82 } :| [],\n    x: \"2014\"\n  } :| {\n    segments: { y: \"BRA\", z: 142.76 } :| { y: \"EGY\", z: 170.68 } :| {\n      y: \"IND\",\n      z: 1118.8899999999999\n    } :| { y: \"JPN\", z: 876.0999999999999 } :| [],\n    x: \"2015\"\n  } :| {\n    segments: { y: \"BRA\", z: 108.03 } :| { y: \"EGY\", z: 174.07999999999998 } :| {\n      y: \"IND\",\n      z: 1193.53\n    } :| { y: \"JPN\", z: 883.3299999999999 } :| [],\n    x: \"2016\"\n  } :| {\n    segments: { y: \"BRA\", z: 116.76 } :| { y: \"EGY\", z: 181.31 } :| {\n      y: \"IND\",\n      z: ⸨1236.43⸩\n    } :| { y: \"JPN\", z: 875.32 } :| [],\n    x: \"2017\"\n  } :| {\n    segments: { y: \"BRA\", z: 101.48 } :| { y: \"EGY\", z: ⸨182.31⸩ } :| {\n      y: \"IND\",\n      z: 1315.57\n    } :| { y: \"JPN\", z: ⸨873.39⸩ } :| [],\n    x: \"2018\"\n  } :| [],\n  tickLabels: { x: Default, y: Default }\n}) :| ScatterPlot({\n  caption: \"Clean energy efficiency vs proportion of renewable energy capacity\",\n  labels: { x: \"Renewables/TotalEnergyCap\", y: \"Clean Capacity Factor\" },\n  points: { x: 0.8723185510332055, y: 0.4180741155728385 } :| {\n    x: 0.383891020964826,\n    y: 0.3306374135311273\n  } :| { x: 0.5685559399722339, y: 0.2651713517303818 } :| {\n    x: 0.39179907463864283,\n    y: 0.5311676111397315\n  } :| { x: 0.0886691179578209, y: 0.4125357483317445 } :| {\n    x: 0.3167847396421975,\n    y: 0.2767379556904734\n  } :| { x: 0.3129857171819161, y: 0.20426921772653447 } :| {\n    x: 0.29687029792356306,\n    y: 0.3462200657379872\n  } :| { x: 0.16239390265026848, y: 0.4128 } :| {\n    x: 0.2115752867627615,\n    y: 0.5086651868096602\n  } :| []\n}) :| [])"
  },
  {
    file: "qcut.fld",
    bwd_expect_file: "qcut.expect.fld",
    "δv": v => Data$dTuple.$Tuple(v, App$dUtil.Persistent),
    fwd_expect: "(1.01 :| 1.05 :| [], 0.051000000000000156) :| (1.07 :| 1.09 :| 1.22 :| 1.23 :| 1.24 :| 1.24 :| 1.25 :| 1.32 :| 1.32 :| 1.35 :| 1.39 :| 1.47 :| 1.57 :| 1.72 :| [], 0.6639999999999999) :| (1.73 :| 1.75 :| 1.76 :| 1.83 :| 1.87 :| 1.94 :| 2.04 :| 2.14 :| 2.18 :| 2.36 :| 2.37 :| 2.38 :| 2.52 :| 2.54 :| [], 0.8464999999999998) :| (2.61 :| 2.67 :| [], 0.09850000000000003) :| []"
  }
];
export {bwd_cases, select, select$p};
