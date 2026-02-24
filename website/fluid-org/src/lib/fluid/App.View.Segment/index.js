import * as $runtime from "../runtime.js";
import * as App$dUtil from "../App.Util/index.js";
import * as App$dUtil$dSelector from "../App.Util.Selector/index.js";
import * as App$dView$dUtil from "../App.View.Util/index.js";
import * as App$dView$dUtil$dD3 from "../App.View.Util.D3/index.js";
import * as Data$dInt from "../Data.Int/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Web$dEvent$dEventTarget from "../Web.Event.EventTarget/index.js";
const max = x => y => {
  const v = Data$dOrd.ordNumber.compare(x)(y);
  if (v === "LT") { return y; }
  if (v === "EQ") { return x; }
  if (v === "GT") { return x; }
  $runtime.fail();
};
const Segment = x => x;
const newtypeSegment_ = {Coercible0: () => {}};
const indexCol = /* #__PURE__ */ App$dView$dUtil$dD3.colorScale("schemeAccent");
const viewableSegmentSegmentCon = {
  isLeaf: v => false,
  createElement: v => v1 => parent => App$dView$dUtil$dD3.createChild(parent)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.Rect))(App$dView$dUtil$dD3.fromFoldable([
    Data$dTuple.$Tuple("x", Data$dShow.showNumberImpl(v.scales.x(v.x))),
    Data$dTuple.$Tuple("y", Data$dShow.showNumberImpl(v.scales.y(v1.z._1 + v.y))),
    Data$dTuple.$Tuple("height", Data$dShow.showNumberImpl(max(Data$dInt.toNumber(v.interior.height - $runtime.intDiv(v.strokeWidth, 2) | 0) - v.scales.y(v1.z._1))(0.0))),
    Data$dTuple.$Tuple("stroke-width", Data$dShow.showIntImpl(v.strokeWidth)),
    Data$dTuple.$Tuple("width", Data$dShow.showNumberImpl(App$dView$dUtil$dD3.bandwidth(v.scales.x))),
    App$dUtil.classes(["bar"])
  ])),
  setSelection: v => v1 => select => segment => {
    const $0 = v.y_index;
    const $$transient = (() => {
      if (v1.z._2.tag === "Inert") { return App$dUtil.None; }
      if (v1.z._2.tag === "Reactive") { return v1.z._2._1.transient; }
      $runtime.fail();
    })();
    const persistent = (() => {
      if (v1.z._2.tag === "Inert") { return App$dUtil.None; }
      if (v1.z._2.tag === "Reactive") { return v1.z._2._1.persistent; }
      $runtime.fail();
    })();
    const attrs = (() => {
      const col$p = indexCol($0);
      return [
        Data$dTuple.$Tuple(
          "fill",
          (() => {
            if (persistent === "None") { return col$p; }
            if (persistent === "Secondary") { return "url(#diagonalHatch-" + Data$dShow.showIntImpl($0) + ")"; }
            if (persistent === "Primary") { return App$dUtil.colorShade(col$p)(-40); }
            $runtime.fail();
          })()
        ),
        Data$dTuple.$Tuple("stroke-width", "1"),
        Data$dTuple.$Tuple(
          "stroke-dasharray",
          (() => {
            if ($$transient === "None") { return "none"; }
            if ($$transient === "Secondary") { return "1 2"; }
            if ($$transient === "Primary") { return "1 2"; }
            $runtime.fail();
          })()
        ),
        Data$dTuple.$Tuple("stroke-linecap", "round"),
        Data$dTuple.$Tuple(
          "stroke",
          (() => {
            if (persistent === "None") { return false; }
            if (persistent === "Secondary") { return true; }
            if (persistent === "Primary") { return true; }
            $runtime.fail();
          })() || (() => {
            if ($$transient === "None") { return false; }
            if ($$transient === "Secondary") { return true; }
            if ($$transient === "Primary") { return true; }
            $runtime.fail();
          })()
            ? App$dUtil.colorShade(col$p)(-70)
            : col$p
        )
      ];
    })();
    const $1 = Web$dEvent$dEventTarget.eventListener(x => select(App$dUtil$dSelector.nthSegment($0)(App$dUtil.selectionEventData$p(x)._2)));
    return () => {
      const listener = $1();
      const $2 = App$dView$dUtil$dD3.attrs(segment)(App$dView$dUtil$dD3.fromFoldable(attrs))();
      return App$dView$dUtil.registerMouseListeners(listener)($2)();
    };
  }
};
export { indexCol,   viewableSegmentSegmentCon};
