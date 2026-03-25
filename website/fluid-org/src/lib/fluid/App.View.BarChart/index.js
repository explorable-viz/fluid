import * as $runtime from "../runtime.js";
import * as App$dUtil from "../App.Util/index.js";
import * as App$dUtil$dSelector from "../App.Util.Selector/index.js";
import * as App$dView$dSegment from "../App.View.Segment/index.js";
import * as App$dView$dStackedBar from "../App.View.StackedBar/index.js";
import * as App$dView$dUtil$dAxes from "../App.View.Util.Axes/index.js";
import * as App$dView$dUtil$dD3 from "../App.View.Util.D3/index.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dArray$dNonEmpty$dInternal from "../Data.Array.NonEmpty.Internal/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFoldableWithIndex from "../Data.FoldableWithIndex/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dInt from "../Data.Int/index.js";
import * as Data$dNumber from "../Data.Number/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dSemigroup$dFoldable from "../Data.Semigroup.Foldable/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Effect from "../Effect/index.js";
import * as Util from "../Util/index.js";
const maximum = /* #__PURE__ */ Data$dSemigroup$dFoldable.maximum(Data$dOrd.ordNumber)(Data$dArray$dNonEmpty$dInternal.foldable1NonEmptyArray);
const forWithIndex_ = /* #__PURE__ */ Data$dFoldableWithIndex.forWithIndex_(Effect.applicativeEffect);
const forWithIndex_1 = /* #__PURE__ */ forWithIndex_(Data$dFoldableWithIndex.foldableWithIndexArray);
const maximum1 = /* #__PURE__ */ Data$dSemigroup$dFoldable.maximum(Data$dOrd.ordInt)(Data$dArray$dNonEmpty$dInternal.foldable1NonEmptyArray);
const length = /* #__PURE__ */ Data$dFoldable.foldlArray(c => v => 1 + c | 0)(0);
const max = x => y => {
  const v = Data$dOrd.ordInt.compare(x)(y);
  if (v === "LT") { return y; }
  if (v === "EQ") { return x; }
  if (v === "GT") { return x; }
  $runtime.fail();
};
const forWithIndex_2 = /* #__PURE__ */ forWithIndex_(Data$dFoldableWithIndex.foldableWithIndexArray);
const BarChart = x => x;
const strokeWidth = 2;
const barChartProps = v => {
  const margin = {top: 3, right: 20, bottom: 30, left: 20};
  const caption_height = App$dView$dUtil$dD3.textDimensions("title-text")(v.caption._1).height * 2 | 0;
  const interior = {width: (v.size.width._1 - margin.left | 0) - margin.right | 0, height: ((v.size.height._1 - margin.top | 0) - margin.bottom | 0) - caption_height | 0};
  const scales = {
    x: App$dView$dUtil$dD3.scaleBand(interior.width)(Data$dFunctor.arrayMap(v3 => v3.x._1)(v.stackedBars)),
    y: App$dView$dUtil$dD3.scaleLinear({min: 0.0, max: Data$dNumber.ceil(10.0 * (maximum(Data$dFunctor.arrayMap(App$dView$dStackedBar.barHeight)(v.stackedBars)) / 10.0))})({
      min: Data$dInt.toNumber(interior.height),
      max: 0.0
    })
  };
  return {
    width: v.size.width._1,
    height: v.size.height._1,
    xs: Data$dFunctor.arrayMap(v1 => v1.x._1)(v.stackedBars),
    ys: Data$dFunctor.arrayMap(v1 => v1.y._1)((() => {
      if (0 < v.stackedBars.length) { return v.stackedBars[0].segments; }
      $runtime.fail();
    })()),
    margin,
    interior,
    scales,
    caption_height,
    caption_class: "title-text",
    stackedBarContext: {interior, scales, strokeWidth: 2}
  };
};
const viewableBarChartUnit = {
  isLeaf: v => false,
  setSelection: v => v1 => select => barChart$p => {
    const $0 = v1.stackedBars;
    const props = barChartProps(v1);
    const $1 = App$dView$dUtil$dD3.selectAll(".stack")(barChart$p);
    return () => {
      const stackedBars$p = $1();
      return forWithIndex_1(stackedBars$p)(i => stack => App$dView$dStackedBar.viewableStackedBarStacked.setSelection(props.stackedBarContext)(Util.unsafeArrayNonEmptyArray.unsafeIndex($0)(i))(x => select(App$dUtil$dSelector.constrArg("BarChart")(0)(App$dUtil$dSelector.dictVal("stackedBars")(App$dUtil$dSelector.listElement(i)(x)))))(stack))();
    };
  },
  createElement: v => v1 => parent => {
    const $0 = v1.caption;
    const $1 = v1.legend;
    const $2 = v1.stackedBars;
    const $3 = v1.tickLabels;
    const props = barChartProps(v1);
    const $4 = App$dView$dUtil$dD3.createChild(parent)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.SVG))(App$dView$dUtil$dD3.fromFoldable([
      Data$dTuple.$Tuple("width", Data$dShow.showIntImpl(props.width)),
      Data$dTuple.$Tuple("height", Data$dShow.showIntImpl(props.height))
    ]));
    return () => {
      const svg = $4();
      const g = App$dView$dUtil$dD3.createChild(svg)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.G))(App$dView$dUtil$dD3.fromFoldable([
        App$dView$dUtil$dD3.translate({x: props.margin.left, y: props.margin.top})
      ]))();
      App$dView$dUtil$dAxes.create_xAxis(g)(props.scales)(props.xs)(props.interior.height)($3.x._1)();
      App$dView$dUtil$dAxes.create_yAxis(g)(props.scales)(3.0)(0)($3.y._1)();
      for (const stackedBar of $2) {
        App$dView$dStackedBar.viewableStackedBarStacked.createElement(props.stackedBarContext)(stackedBar)(g)();
      }
      for (const y_index of Data$dArray.rangeImpl(0, length(props.ys) - 1 | 0)) {
        App$dView$dUtil$dD3.addHatchPattern(g)(y_index)(App$dView$dSegment.indexCol(y_index))();
      }
      const $5 = App$dView$dUtil$dD3.createChild(svg)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.Text))(App$dView$dUtil$dD3.fromFoldable([
        Data$dTuple.$Tuple("x", Data$dShow.showIntImpl($runtime.intDiv(props.width, 2))),
        Data$dTuple.$Tuple("y", Data$dShow.showIntImpl(props.height - $runtime.intDiv(props.caption_height, 2) | 0)),
        App$dUtil.classes([props.caption_class]),
        Data$dTuple.$Tuple("dominant-baseline", "central"),
        Data$dTuple.$Tuple("text-anchor", "middle")
      ]))();
      App$dView$dUtil$dD3.setText($0._1)($5)();
      const height = 15 * length(props.ys) | 0;
      const width = (
        15 + maximum1(Data$dFunctor.arrayMap((() => {
          const $6 = App$dView$dUtil$dD3.textDimensions("legend-text");
          return x => $6(x).width;
        })())(props.ys)) | 0
      ) + 4 | 0;
      const $6 = (() => {
        const $6 = App$dView$dUtil$dD3.createChild(g)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.G))(App$dView$dUtil$dD3.fromFoldable([
          App$dView$dUtil$dD3.translate({x: props.interior.width + 0 | 0, y: max(0)($runtime.intDiv(props.interior.height - height | 0, 2))})
        ]));
        return () => {
          const legend$p = $6();
          App$dView$dUtil$dD3.createChild(legend$p)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.Rect))(App$dView$dUtil$dD3.fromFoldable([
            App$dUtil.classes(["legend-box"]),
            Data$dTuple.$Tuple("x", "0"),
            Data$dTuple.$Tuple("y", "0"),
            Data$dTuple.$Tuple("height", Data$dShow.showIntImpl(height)),
            Data$dTuple.$Tuple("width", Data$dShow.showIntImpl(width))
          ]))();
          return forWithIndex_2(props.ys)(y_index => y => {
            const $7 = App$dView$dUtil$dD3.createChild(legend$p)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.G))(App$dView$dUtil$dD3.fromFoldable([
              App$dUtil.classes(["legend-entry"]),
              App$dView$dUtil$dD3.translate({x: 0, y: (y_index * 15 | 0) + 2 | 0})
            ]));
            return () => {
              const g$1 = $7();
              const $8 = App$dView$dUtil$dD3.createChild(g$1)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.Text))(App$dView$dUtil$dD3.fromFoldable([
                App$dUtil.classes(["legend-text"]),
                App$dView$dUtil$dD3.translate({x: 15, y: 9})
              ]))();
              App$dView$dUtil$dD3.setText(y)($8)();
              return App$dView$dUtil$dD3.createChild(g$1)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.Rect))(App$dView$dUtil$dD3.fromFoldable([
                Data$dTuple.$Tuple("fill", App$dView$dSegment.indexCol(y_index)),
                Data$dTuple.$Tuple("width", "4"),
                Data$dTuple.$Tuple("height", "4"),
                Data$dTuple.$Tuple("x", "5"),
                Data$dTuple.$Tuple("y", "3")
              ]))();
            };
          })();
        };
      })();
      if ($1._1) { $6(); }
      return g;
    };
  }
};
export {          viewableBarChartUnit};
