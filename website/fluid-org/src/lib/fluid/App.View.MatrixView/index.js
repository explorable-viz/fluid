import * as $runtime from "../runtime.js";
import * as App$dUtil from "../App.Util/index.js";
import * as App$dUtil$dSelector from "../App.Util.Selector/index.js";
import * as App$dView$dUtil from "../App.View.Util/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Primitive from "../Primitive/index.js";
import * as Util from "../Util/index.js";
import * as Web$dEvent$dEventTarget from "../Web.Event.EventTarget/index.js";
import {createElement, setSelection} from "./foreign.js";
const $ShadowDirection = tag => tag;
const North = /* #__PURE__ */ $ShadowDirection("North");
const South = /* #__PURE__ */ $ShadowDirection("South");
const East = /* #__PURE__ */ $ShadowDirection("East");
const West = /* #__PURE__ */ $ShadowDirection("West");
const None = /* #__PURE__ */ $ShadowDirection("None");
const MatrixView = x => x;
const matrixViewHelpers = {
  hBorderStyles: m => x => {
    if (x.i === 0) {
      if (
        (() => {
          const $0 = Util.unsafeArrayArray.unsafeIndex(Util.unsafeArrayArray.unsafeIndex(m.cells)(x.i))(x.j - 1 | 0);
          const $1 = (() => {
            if ($0._2.tag === "Inert") { return App$dUtil.None; }
            if ($0._2.tag === "Reactive") { return $0._2._1.transient; }
            $runtime.fail();
          })();
          if ($1 === "None") { return false; }
          if ($1 === "Secondary") { return true; }
          if ($1 === "Primary") { return true; }
          $runtime.fail();
        })()
      ) {
        return "filter: drop-shadow(0px 1px 1px blue);";
      }
      return "visibility: hidden;";
    }
    if (x.i === m.i) {
      if (
        (() => {
          const $0 = Util.unsafeArrayArray.unsafeIndex(Util.unsafeArrayArray.unsafeIndex(m.cells)(x.i - 1 | 0))(x.j - 1 | 0);
          const $1 = (() => {
            if ($0._2.tag === "Inert") { return App$dUtil.None; }
            if ($0._2.tag === "Reactive") { return $0._2._1.transient; }
            $runtime.fail();
          })();
          if ($1 === "None") { return false; }
          if ($1 === "Secondary") { return true; }
          if ($1 === "Primary") { return true; }
          $runtime.fail();
        })()
      ) {
        return "filter: drop-shadow(0px -1px 1px blue);";
      }
      return "visibility: hidden;";
    }
    if (
      (() => {
        const $0 = Util.unsafeArrayArray.unsafeIndex(Util.unsafeArrayArray.unsafeIndex(m.cells)(x.i))(x.j - 1 | 0);
        return (() => {
          const $1 = (() => {
            if ($0._2.tag === "Inert") { return App$dUtil.None; }
            if ($0._2.tag === "Reactive") { return $0._2._1.transient; }
            $runtime.fail();
          })();
          if ($1 === "None") { return false; }
          if ($1 === "Secondary") { return true; }
          if ($1 === "Primary") { return true; }
          $runtime.fail();
        })() && (() => {
          const $1 = Util.unsafeArrayArray.unsafeIndex(Util.unsafeArrayArray.unsafeIndex(m.cells)(x.i - 1 | 0))(x.j - 1 | 0);
          const $2 = (() => {
            if ($1._2.tag === "Inert") { return App$dUtil.None; }
            if ($1._2.tag === "Reactive") { return $1._2._1.transient; }
            $runtime.fail();
          })();
          if ($2 === "None") { return true; }
          if ($2 === "Secondary") { return false; }
          if ($2 === "Primary") { return false; }
          $runtime.fail();
        })();
      })()
    ) {
      return "filter: drop-shadow(0px 1px 1px blue);";
    }
    if (
      (() => {
        const $0 = Util.unsafeArrayArray.unsafeIndex(Util.unsafeArrayArray.unsafeIndex(m.cells)(x.i))(x.j - 1 | 0);
        const $1 = (() => {
          if ($0._2.tag === "Inert") { return App$dUtil.None; }
          if ($0._2.tag === "Reactive") { return $0._2._1.transient; }
          $runtime.fail();
        })();
        if ($1 === "None") { return true; }
        if ($1 === "Secondary") { return false; }
        if ($1 === "Primary") { return false; }
        $runtime.fail();
      })() && (() => {
        const $0 = Util.unsafeArrayArray.unsafeIndex(Util.unsafeArrayArray.unsafeIndex(m.cells)(x.i - 1 | 0))(x.j - 1 | 0);
        const $1 = (() => {
          if ($0._2.tag === "Inert") { return App$dUtil.None; }
          if ($0._2.tag === "Reactive") { return $0._2._1.transient; }
          $runtime.fail();
        })();
        if ($1 === "None") { return false; }
        if ($1 === "Secondary") { return true; }
        if ($1 === "Primary") { return true; }
        $runtime.fail();
      })()
    ) {
      return "filter: drop-shadow(0px -1px 1px blue);";
    }
    return "visibility: hidden;";
  },
  vBorderStyles: m => x => {
    if (x.j === 0) {
      if (
        (() => {
          const $0 = Util.unsafeArrayArray.unsafeIndex(Util.unsafeArrayArray.unsafeIndex(m.cells)(x.i - 1 | 0))(x.j);
          const $1 = (() => {
            if ($0._2.tag === "Inert") { return App$dUtil.None; }
            if ($0._2.tag === "Reactive") { return $0._2._1.transient; }
            $runtime.fail();
          })();
          if ($1 === "None") { return false; }
          if ($1 === "Secondary") { return true; }
          if ($1 === "Primary") { return true; }
          $runtime.fail();
        })()
      ) {
        return "filter: drop-shadow(1px 0px 1px blue);";
      }
      return "visibility: hidden;";
    }
    if (x.j === m.j) {
      if (
        (() => {
          const $0 = Util.unsafeArrayArray.unsafeIndex(Util.unsafeArrayArray.unsafeIndex(m.cells)(x.i - 1 | 0))(x.j - 1 | 0);
          const $1 = (() => {
            if ($0._2.tag === "Inert") { return App$dUtil.None; }
            if ($0._2.tag === "Reactive") { return $0._2._1.transient; }
            $runtime.fail();
          })();
          if ($1 === "None") { return false; }
          if ($1 === "Secondary") { return true; }
          if ($1 === "Primary") { return true; }
          $runtime.fail();
        })()
      ) {
        return "filter: drop-shadow(-1px 0px 1px blue);";
      }
      return "visibility: hidden;";
    }
    if (
      (() => {
        const $0 = Util.unsafeArrayArray.unsafeIndex(Util.unsafeArrayArray.unsafeIndex(m.cells)(x.i - 1 | 0))(x.j);
        return (() => {
          const $1 = (() => {
            if ($0._2.tag === "Inert") { return App$dUtil.None; }
            if ($0._2.tag === "Reactive") { return $0._2._1.transient; }
            $runtime.fail();
          })();
          if ($1 === "None") { return false; }
          if ($1 === "Secondary") { return true; }
          if ($1 === "Primary") { return true; }
          $runtime.fail();
        })() && (() => {
          const $1 = Util.unsafeArrayArray.unsafeIndex(Util.unsafeArrayArray.unsafeIndex(m.cells)(x.i - 1 | 0))(x.j - 1 | 0);
          const $2 = (() => {
            if ($1._2.tag === "Inert") { return App$dUtil.None; }
            if ($1._2.tag === "Reactive") { return $1._2._1.transient; }
            $runtime.fail();
          })();
          if ($2 === "None") { return true; }
          if ($2 === "Secondary") { return false; }
          if ($2 === "Primary") { return false; }
          $runtime.fail();
        })();
      })()
    ) {
      return "filter: drop-shadow(1px 0px 1px blue);";
    }
    if (
      (() => {
        const $0 = Util.unsafeArrayArray.unsafeIndex(Util.unsafeArrayArray.unsafeIndex(m.cells)(x.i - 1 | 0))(x.j);
        const $1 = (() => {
          if ($0._2.tag === "Inert") { return App$dUtil.None; }
          if ($0._2.tag === "Reactive") { return $0._2._1.transient; }
          $runtime.fail();
        })();
        if ($1 === "None") { return true; }
        if ($1 === "Secondary") { return false; }
        if ($1 === "Primary") { return false; }
        $runtime.fail();
      })() && (() => {
        const $0 = Util.unsafeArrayArray.unsafeIndex(Util.unsafeArrayArray.unsafeIndex(m.cells)(x.i - 1 | 0))(x.j - 1 | 0);
        const $1 = (() => {
          if ($0._2.tag === "Inert") { return App$dUtil.None; }
          if ($0._2.tag === "Reactive") { return $0._2._1.transient; }
          $runtime.fail();
        })();
        if ($1 === "None") { return false; }
        if ($1 === "Secondary") { return true; }
        if ($1 === "Primary") { return true; }
        $runtime.fail();
      })()
    ) {
      return "filter: drop-shadow(-1px 0px 1px blue);";
    }
    return "visibility: hidden;";
  },
  eventListener: Web$dEvent$dEventTarget.eventListener,
  withElement: sel => x => sel((() => {
    const $0 = App$dUtil.selectionEventData$p(x);
    return App$dUtil$dSelector.matrixElement($0._1.i)($0._1.j)($0._2);
  })())
};
const viewableMatrixViewUnit = {
  isLeaf: v => false,
  createElement: v => createElement(App$dView$dUtil.uiHelpers),
  setSelection: v => setSelection(matrixViewHelpers)(App$dView$dUtil.uiHelpers)
};
const matrixRep = v => (
  {
    cells: Data$dFunctor.arrayMap(v1 => Data$dFunctor.arrayMap(v$1 => Data$dTuple.$Tuple(v$1._3.tag === "Int" ? v$1._3._1 : Primitive.typeError(v$1._3)("Int"), v$1._1))(v1))(v._1),
    i: v._2._1._1,
    j: v._2._2._1
  }
);
export {       matrixRep,  viewableMatrixViewUnit};
export * from "./foreign.js";
