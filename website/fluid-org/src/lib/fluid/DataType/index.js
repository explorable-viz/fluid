import * as $runtime from "../runtime.js";
import * as Data$dCodePoint$dUnicode$dInternal from "../Data.CodePoint.Unicode.Internal/index.js";
import * as Data$dEnum from "../Data.Enum/index.js";
import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dList from "../Data.List/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMap$dInternal from "../Data.Map.Internal/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dSet from "../Data.Set/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dString$dCodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Effect$dException from "../Effect.Exception/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
import * as Util from "../Util/index.js";
import * as Util$dMap from "../Util.Map/index.js";
const $DataType = (_1, _2) => ({tag: "DataType", _1, _2});
const fromFoldable = /* #__PURE__ */ Foreign$dObject.fromFoldable(Data$dFoldable.foldableArray);
const fromFoldable1 = /* #__PURE__ */ (() => {
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
        go$a0 = Data$dMap$dInternal.insert(Data$dOrd.ordString)(v._1)()(b);
        go$a1 = v._2;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  const $0 = go(Data$dMap$dInternal.Leaf);
  return x => $0((() => {
    const go$1 = (m$p, z$p) => {
      if (m$p.tag === "Leaf") { return z$p; }
      if (m$p.tag === "Node") { return go$1(m$p._5, Data$dList$dTypes.$List("Cons", m$p._3, go$1(m$p._6, z$p))); }
      $runtime.fail();
    };
    return go$1(x, Data$dList$dTypes.Nil);
  })());
})();
const toUnfoldable = x => {
  const go = (m$p, z$p) => {
    if (m$p.tag === "Leaf") { return z$p; }
    if (m$p.tag === "Node") { return go(m$p._5, Data$dList$dTypes.$List("Cons", m$p._3, go(m$p._6, z$p))); }
    $runtime.fail();
  };
  const go$1 = go$1$a0$copy => go$1$a1$copy => {
    let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
    while (go$1$c) {
      const source = go$1$a0, memo = go$1$a1;
      if (source.tag === "Nil") {
        const go$2 = go$2$a0$copy => go$2$a1$copy => {
          let go$2$a0 = go$2$a0$copy, go$2$a1 = go$2$a1$copy, go$2$c = true, go$2$r;
          while (go$2$c) {
            const b = go$2$a0, v = go$2$a1;
            if (v.tag === "Nil") {
              go$2$c = false;
              go$2$r = b;
              continue;
            }
            if (v.tag === "Cons") {
              go$2$a0 = Data$dList$dTypes.$List("Cons", v._1, b);
              go$2$a1 = v._2;
              continue;
            }
            $runtime.fail();
          }
          return go$2$r;
        };
        go$1$c = false;
        go$1$r = go$2(Data$dList$dTypes.Nil)(memo);
        continue;
      }
      if (source.tag === "Cons") {
        go$1$a0 = source._2;
        go$1$a1 = Data$dList$dTypes.$List("Cons", source._1, memo);
        continue;
      }
      $runtime.fail();
    }
    return go$1$r;
  };
  return go$1(go(x, Data$dList$dTypes.Nil))(Data$dList$dTypes.Nil);
};
const show = /* #__PURE__ */ (() => Data$dSet.showSet(Data$dShow.showString).show)();
const DataType = value0 => value1 => $DataType(value0, value1);
const typeName = v => v._1;
const eqDataType = {eq: x => y => x._1 === y._1};
const showDataType = {show: typeName};
const isCtrOp = str => ":" === Util.definitely("absurd")(Data$dString$dCodeUnits.charAt(0)(str));
const isCtrName = str => Data$dCodePoint$dUnicode$dInternal.checkAttr([512, 524288])(Data$dEnum.toCharCode(Util.definitely("absurd")(Data$dString$dCodeUnits.charAt(0)(str))));
const showCtr = c => {
  if (isCtrName(c)) { return c; }
  if (":" === Util.definitely("absurd")(Data$dString$dCodeUnits.charAt(0)(c))) { return "(" + c + ")"; }
  return Effect$dException.throwException(Effect$dException.error("absurd"))();
};
const f_z = "z";
const f_y = "y";
const f_x = "x";
const f_width = "width";
const f_tickLabels = "tickLabels";
const f_stackedBars = "stackedBars";
const f_size = "size";
const f_segments = "segments";
const f_points = "points";
const f_plots = "plots";
const f_name = "name";
const f_legend = "legend";
const f_labels = "labels";
const f_height = "height";
const f_colour = "c";
const f_caption = "caption";
const dataTypeFor = dict => dict.dataTypeFor;
const dataType = name => {
  const $0 = Data$dFunctor.arrayMap(v => Data$dTuple.$Tuple(v._1, v._2));
  const $1 = DataType(name);
  return x => $1(fromFoldable($0(x)));
};
const ctrs = v => fromFoldable1(Util$dMap.mapObjectString.keys(v._2));
const cTrue = "True";
const cText = "Text";
const cSome = "Some";
const cScatterPlot = "ScatterPlot";
const cRotated = "Rotated";
const cParagraph = "Paragraph";
const cPair = "Pair";
const cNone = "None";
const cNil = "Nil";
const cMultiView = "MultiView";
const cLink = "Link";
const cLinePlot = "LinePlot";
const cLineChart = "LineChart";
const cFalse = "False";
const cDefault = "Default";
const cCons = ":";
const cBarChart = "BarChart";
const dataTypes = /* #__PURE__ */ Data$dFoldable.foldrArray(Data$dList$dTypes.Cons)(Data$dList$dTypes.Nil)([
  /* #__PURE__ */ dataType("Bool")([/* #__PURE__ */ Data$dTuple.$Tuple("True", 0), /* #__PURE__ */ Data$dTuple.$Tuple("False", 0)]),
  /* #__PURE__ */ dataType("InfNum")([/* #__PURE__ */ Data$dTuple.$Tuple("FNum", 1), /* #__PURE__ */ Data$dTuple.$Tuple("Infty", 0)]),
  /* #__PURE__ */ dataType("List")([/* #__PURE__ */ Data$dTuple.$Tuple("Nil", 0), /* #__PURE__ */ Data$dTuple.$Tuple(":", 2)]),
  /* #__PURE__ */ dataType("Option")([/* #__PURE__ */ Data$dTuple.$Tuple("None", 0), /* #__PURE__ */ Data$dTuple.$Tuple("Some", 1)]),
  /* #__PURE__ */ dataType("Ordering")([/* #__PURE__ */ Data$dTuple.$Tuple("GT", 0), /* #__PURE__ */ Data$dTuple.$Tuple("LT", 0), /* #__PURE__ */ Data$dTuple.$Tuple("EQ", 0)]),
  /* #__PURE__ */ dataType("Pair")([/* #__PURE__ */ Data$dTuple.$Tuple("Pair", 2)]),
  /* #__PURE__ */ dataType("Tree")([/* #__PURE__ */ Data$dTuple.$Tuple("Empty", 0), /* #__PURE__ */ Data$dTuple.$Tuple("NonEmpty", 3)]),
  /* #__PURE__ */ dataType("LinePlot")([/* #__PURE__ */ Data$dTuple.$Tuple("LinePlot", 1)]),
  /* #__PURE__ */ dataType("Orientation")([/* #__PURE__ */ Data$dTuple.$Tuple("Default", 0), /* #__PURE__ */ Data$dTuple.$Tuple("Rotated", 0)]),
  /* #__PURE__ */ dataType("View")([
    /* #__PURE__ */ Data$dTuple.$Tuple("BarChart", 1),
    /* #__PURE__ */ Data$dTuple.$Tuple("LineChart", 1),
    /* #__PURE__ */ Data$dTuple.$Tuple("MultiView", 1),
    /* #__PURE__ */ Data$dTuple.$Tuple("Paragraph", 1),
    /* #__PURE__ */ Data$dTuple.$Tuple("ScatterPlot", 1)
  ]),
  /* #__PURE__ */ dataType("Point")([/* #__PURE__ */ Data$dTuple.$Tuple("Point", 2)]),
  /* #__PURE__ */ dataType("Orient")([/* #__PURE__ */ Data$dTuple.$Tuple("Horiz", 0), /* #__PURE__ */ Data$dTuple.$Tuple("Vert", 0)]),
  /* #__PURE__ */ dataType("GraphicsElement")([
    /* #__PURE__ */ Data$dTuple.$Tuple("Circle", 4),
    /* #__PURE__ */ Data$dTuple.$Tuple("Group", 1),
    /* #__PURE__ */ Data$dTuple.$Tuple("Line", 4),
    /* #__PURE__ */ Data$dTuple.$Tuple("Polyline", 3),
    /* #__PURE__ */ Data$dTuple.$Tuple("Polymarkers", 2),
    /* #__PURE__ */ Data$dTuple.$Tuple("Rect", 5),
    /* #__PURE__ */ Data$dTuple.$Tuple("String", 5),
    /* #__PURE__ */ Data$dTuple.$Tuple("Viewport", 9)
  ]),
  /* #__PURE__ */ dataType("Transform")([/* #__PURE__ */ Data$dTuple.$Tuple("Scale", 2), /* #__PURE__ */ Data$dTuple.$Tuple("Translate", 2)]),
  /* #__PURE__ */ dataType("Marker")([/* #__PURE__ */ Data$dTuple.$Tuple("Arrowhead", 0)]),
  /* #__PURE__ */ dataType("ParaFragment")([/* #__PURE__ */ Data$dTuple.$Tuple("Text", 1), /* #__PURE__ */ Data$dTuple.$Tuple("Link", 2)])
]);
const ctrToDataType = /* #__PURE__ */ (() => Foreign$dObject.fromFoldable(Data$dList$dTypes.foldableList)(Data$dList$dTypes.bindList.bind(Data$dList$dTypes.listMap(d => Data$dList$dTypes.listMap(v => Data$dTuple.$Tuple(
  v,
  d
))(toUnfoldable(fromFoldable1(Util$dMap.mapObjectString.keys(d._2)))))(dataTypes))(Data$dList.identity)))();
const dataTypeForCtr = {
  dataTypeFor: dictMonadThrow => c => Util.orElse(dictMonadThrow)("Unknown constructor " + showCtr(c))(Foreign$dObject._lookup(
    Data$dMaybe.Nothing,
    Data$dMaybe.Just,
    c,
    ctrToDataType
  ))
};
const dataTypeForSetCtr = {
  dataTypeFor: dictMonadThrow => cs => {
    const v = toUnfoldable(cs);
    if (v.tag === "Cons") { return dataTypeForCtr.dataTypeFor(dictMonadThrow)(v._1); }
    $runtime.fail();
  }
};
const consistentWith = dictMonadError => {
  const MonadThrow0 = dictMonadError.MonadThrow0();
  const Bind1 = MonadThrow0.Monad0().Bind1();
  const $$void = Bind1.Apply0().Functor0().map(v => {});
  const withMsg = Util.withMsg(dictMonadError);
  return cs => cs$p => $$void(Bind1.bind(dataTypeForSetCtr.dataTypeFor(MonadThrow0)(cs$p))(d => Bind1.bind(dataTypeForSetCtr.dataTypeFor(MonadThrow0)(cs$p))(d$p => withMsg("constructors of " + d$p._1 + " do not include " + show(Data$dSet.map(Data$dOrd.ordString)(showCtr)(cs)))(Util.mayFailEq(MonadThrow0)(showDataType)(eqDataType)(d)(d$p)))));
};
const arity = dictMonadThrow => c => dictMonadThrow.Monad0().Bind1().bind(dataTypeForCtr.dataTypeFor(dictMonadThrow)(c))(v => Util.orElse(dictMonadThrow)("absurd")(Foreign$dObject._lookup(
  Data$dMaybe.Nothing,
  Data$dMaybe.Just,
  c,
  v._2
)));
const checkArity = dictMonadError => {
  const MonadThrow0 = dictMonadError.MonadThrow0();
  const Monad0 = MonadThrow0.Monad0();
  const $$void = Monad0.Bind1().Apply0().Functor0().map(v => {});
  const withMsg = Util.withMsg(dictMonadError);
  const bind2Flipped = Util.bind2Flipped(Monad0);
  return c => n => $$void(withMsg("Checking arity of " + showCtr(c))(bind2Flipped(Util.mayFailEq(MonadThrow0)(Data$dShow.showInt)(Data$dEq.eqInt))(arity(MonadThrow0)(c))(Monad0.Applicative0().pure(n))));
};
export {
  
  
  arity,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  checkArity,
  consistentWith,
  
  
  
  
  dataTypeForCtr,
  dataTypeForSetCtr,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  fromFoldable1,
  
  
  
  showCtr,
  
  
  
};
