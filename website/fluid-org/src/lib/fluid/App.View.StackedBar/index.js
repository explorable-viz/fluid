import * as $runtime from "../runtime.js";
import * as App$dUtil from "../App.Util/index.js";
import * as App$dUtil$dSelector from "../App.Util.Selector/index.js";
import * as App$dView$dSegment from "../App.View.Segment/index.js";
import * as App$dView$dUtil$dD3 from "../App.View.Util.D3/index.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFoldableWithIndex from "../Data.FoldableWithIndex/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dSemiring from "../Data.Semiring/index.js";
import * as Effect from "../Effect/index.js";
import * as Util from "../Util/index.js";
const forWithIndex_ = /* #__PURE__ */ Data$dFoldableWithIndex.forWithIndex_(Effect.applicativeEffect);
const forWithIndex_1 = /* #__PURE__ */ forWithIndex_(Data$dFoldableWithIndex.foldableWithIndexArray);
const forWithIndex_2 = /* #__PURE__ */ forWithIndex_(Data$dFoldableWithIndex.foldableWithIndexArray);
const sum = /* #__PURE__ */ Data$dFoldable.foldlArray(Data$dSemiring.numAdd)(0.0);
const StackedBar = x => x;
const newtypeStackedBar_ = {Coercible0: () => {}};
const segmentContext = v => v1 => {
  const $0 = v.interior;
  const $1 = v.scales;
  const $2 = v.strokeWidth;
  const $3 = v1.x;
  const $4 = [0.0, ...Data$dArray.scanlImpl(Data$dSemiring.numAdd, 0.0, Data$dFunctor.arrayMap(v2 => v2.z._1)(v1.segments))];
  const ys = (() => {
    if ($4.length === 0) { $runtime.fail(); }
    return Data$dArray.sliceImpl(0, $4.length - 1 | 0, $4);
  })();
  return y_index => ({interior: $0, scales: $1, strokeWidth: $2, x: $3._1, y: Util.unsafeArrayArray.unsafeIndex(ys)(y_index), y_index});
};
const viewableStackedBarStacked = {
  isLeaf: v => false,
  createElement: context => v => parent => {
    const $0 = v.segments;
    const $1 = App$dView$dUtil$dD3.createChild(parent)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.G))(App$dView$dUtil$dD3.fromFoldable([
      App$dUtil.classes(["stack"])
    ]));
    return () => {
      const g = $1();
      forWithIndex_1($0)(y_index => segment => App$dView$dSegment.viewableSegmentSegmentCon.createElement(segmentContext(context)(v)(y_index))(segment)(g))();
      return g;
    };
  },
  setSelection: context => v => select => root => {
    const $0 = v.segments;
    const $1 = App$dView$dUtil$dD3.selectAll(".bar")(root);
    return () => {
      const segments$p = $1();
      return forWithIndex_2(segments$p)(y_index => segment => App$dView$dSegment.viewableSegmentSegmentCon.setSelection(segmentContext(context)(v)(y_index))(Util.unsafeArrayNonEmptyArray.unsafeIndex($0)(y_index))(x => select(App$dUtil$dSelector.dictVal("segments")(x)))(segment))();
    };
  }
};
const barHeight = v => sum(Data$dFunctor.arrayMap(v1 => v1.z._1)(v.segments));
export { barHeight,       viewableStackedBarStacked};
