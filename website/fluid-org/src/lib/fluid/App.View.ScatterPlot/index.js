import * as $runtime from "../runtime.js";
import * as App$dUtil from "../App.Util/index.js";
import * as App$dUtil$dSelector from "../App.Util.Selector/index.js";
import * as App$dView$dUtil from "../App.View.Util/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dInt from "../Data.Int/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
import * as Util from "../Util/index.js";
import * as Web$dEvent$dEventTarget from "../Web.Event.EventTarget/index.js";
import {createElement, setSelection} from "./foreign.js";
const join = v => v1 => {
  if (v1.tag === "Inert") { return v; }
  if (v.tag === "Inert") { return v1; }
  if (v.tag === "Reactive" && v1.tag === "Reactive") {
    return App$dUtil.$SelState(
      "Reactive",
      {
        persistent: (() => {
          if (v._1.persistent === "None") {
            if (v1._1.persistent === "None") { return v._1.persistent; }
            return v1._1.persistent;
          }
          if (v._1.persistent === "Secondary") {
            if (v1._1.persistent === "Secondary") { return v._1.persistent; }
            if (v1._1.persistent === "Primary") { return v1._1.persistent; }
            if (v1._1.persistent === "None") { return v._1.persistent; }
            $runtime.fail();
          }
          if (v._1.persistent === "Primary") { return v._1.persistent; }
          $runtime.fail();
        })(),
        transient: (() => {
          if (v._1.transient === "None") {
            if (v1._1.transient === "None") { return v._1.transient; }
            return v1._1.transient;
          }
          if (v._1.transient === "Secondary") {
            if (v1._1.transient === "Secondary") { return v._1.transient; }
            if (v1._1.transient === "Primary") { return v1._1.transient; }
            if (v1._1.transient === "None") { return v._1.transient; }
            $runtime.fail();
          }
          if (v._1.transient === "Primary") { return v._1.transient; }
          $runtime.fail();
        })()
      }
    );
  }
  $runtime.fail();
};
const fromFoldable = /* #__PURE__ */ Foreign$dObject.fromFoldable(Data$dFoldable.foldableArray);
const ScatterPlot = x => x;
const scatterPlotHelpers = {
  point_attrs: v => v1 => {
    const v2 = Util.unsafeArrayArray.unsafeIndex(v.points)(v1.i);
    const sel = join(v2.x._2)(v2.y._2);
    return fromFoldable([
      Data$dTuple.$Tuple(
        "r",
        Data$dShow.showNumberImpl(Data$dInt.toNumber(2) * (() => {
          if (
            (() => {
              if (sel.tag === "Inert") { return false; }
              if (sel.tag === "Reactive") {
                return (() => {
                  if (sel._1.persistent === "None") { return false; }
                  if (sel._1.persistent === "Secondary") { return false; }
                  if (sel._1.persistent === "Primary") { return true; }
                  $runtime.fail();
                })() || (() => {
                  if (sel._1.transient === "None") { return false; }
                  if (sel._1.transient === "Secondary") { return false; }
                  if (sel._1.transient === "Primary") { return true; }
                  $runtime.fail();
                })();
              }
              $runtime.fail();
            })()
          ) {
            return 1.6;
          }
          if (
            (() => {
              if (sel.tag === "Inert") { return false; }
              if (sel.tag === "Reactive") {
                return (() => {
                  if (sel._1.persistent === "None") { return false; }
                  if (sel._1.persistent === "Secondary") { return true; }
                  if (sel._1.persistent === "Primary") { return false; }
                  $runtime.fail();
                })() || (() => {
                  if (sel._1.transient === "None") { return false; }
                  if (sel._1.transient === "Secondary") { return true; }
                  if (sel._1.transient === "Primary") { return false; }
                  $runtime.fail();
                })();
              }
              $runtime.fail();
            })()
          ) {
            return 1.25;
          }
          return 1.0;
        })())
      )
    ]);
  },
  eventListener: Web$dEvent$dEventTarget.eventListener,
  withScatterPlotPoint: sel => x => sel((() => {
    const $0 = App$dUtil.selectionEventData$p(x);
    return App$dUtil$dSelector.constrArg("ScatterPlot")(0)(App$dUtil$dSelector.scatterPoint($0._1.i)($0._2));
  })())
};
const viewableScatterPlotUnit = {
  isLeaf: v => false,
  createElement: v => createElement(App$dView$dUtil.uiHelpers),
  setSelection: v => setSelection(scatterPlotHelpers)(App$dView$dUtil.uiHelpers)
};
export {    viewableScatterPlotUnit};
export * from "./foreign.js";
