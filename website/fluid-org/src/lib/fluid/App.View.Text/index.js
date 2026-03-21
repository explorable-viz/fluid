import * as $runtime from "../runtime.js";
import * as App$dUtil from "../App.Util/index.js";
import * as App$dView$dUtil from "../App.View.Util/index.js";
import * as App$dView$dUtil$dD3 from "../App.View.Util.D3/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Unsafe$dCoerce from "../Unsafe.Coerce/index.js";
import * as Web$dEvent$dEventTarget from "../Web.Event.EventTarget/index.js";
const Text = x => x;
const newtypeText_ = {Coercible0: () => {}};
const textualText = {getText: Unsafe$dCoerce.unsafeCoerce};
const getText = dict => dict.getText;
const textAttrs = dictTextual => text => {
  const $0 = dictTextual.getText(text);
  return [
    Data$dTuple.$Tuple(
      "border-bottom",
      (() => {
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
        ? "1px solid blue"
        : "none"
    ),
    Data$dTuple.$Tuple(
      "background",
      (() => {
        if (
          (() => {
            const $1 = (() => {
              if ($0._2.tag === "Inert") { return App$dUtil.None; }
              if ($0._2.tag === "Reactive") { return $0._2._1.persistent; }
              $runtime.fail();
            })();
            return (() => {
              if ($0._2.tag === "Inert") { return false; }
              if ($0._2.tag === "Reactive") {
                return (() => {
                  if ($0._2._1.persistent === "None") { return false; }
                  if ($0._2._1.persistent === "Secondary") { return false; }
                  if ($0._2._1.persistent === "Primary") { return true; }
                  $runtime.fail();
                })() || (() => {
                  if ($0._2._1.transient === "None") { return false; }
                  if ($0._2._1.transient === "Secondary") { return false; }
                  if ($0._2._1.transient === "Primary") { return true; }
                  $runtime.fail();
                })();
              }
              $runtime.fail();
            })() && (() => {
              if ($1 === "None") { return false; }
              if ($1 === "Secondary") { return true; }
              if ($1 === "Primary") { return true; }
              $runtime.fail();
            })();
          })()
        ) {
          return "#93E9BE";
        }
        if (
          (() => {
            const $1 = (() => {
              if ($0._2.tag === "Inert") { return App$dUtil.None; }
              if ($0._2.tag === "Reactive") { return $0._2._1.persistent; }
              $runtime.fail();
            })();
            return (() => {
              if ($0._2.tag === "Inert") { return false; }
              if ($0._2.tag === "Reactive") {
                return (() => {
                  if ($0._2._1.persistent === "None") { return false; }
                  if ($0._2._1.persistent === "Secondary") { return true; }
                  if ($0._2._1.persistent === "Primary") { return false; }
                  $runtime.fail();
                })() || (() => {
                  if ($0._2._1.transient === "None") { return false; }
                  if ($0._2._1.transient === "Secondary") { return true; }
                  if ($0._2._1.transient === "Primary") { return false; }
                  $runtime.fail();
                })();
              }
              $runtime.fail();
            })() && (() => {
              if ($1 === "None") { return false; }
              if ($1 === "Secondary") { return true; }
              if ($1 === "Primary") { return true; }
              $runtime.fail();
            })();
          })()
        ) {
          return "rgb(226, 226, 226)";
        }
        return "white";
      })()
    ),
    Data$dTuple.$Tuple(
      "color",
      (() => {
        const $1 = (() => {
          if ($0._2.tag === "Inert") { return App$dUtil.None; }
          if ($0._2.tag === "Reactive") { return $0._2._1.transient; }
          $runtime.fail();
        })();
        return (() => {
          if ($0._2.tag === "Inert") { return false; }
          if ($0._2.tag === "Reactive") {
            return (() => {
              if ($0._2._1.persistent === "None") { return false; }
              if ($0._2._1.persistent === "Secondary") { return false; }
              if ($0._2._1.persistent === "Primary") { return true; }
              $runtime.fail();
            })() || (() => {
              if ($0._2._1.transient === "None") { return false; }
              if ($0._2._1.transient === "Secondary") { return false; }
              if ($0._2._1.transient === "Primary") { return true; }
              $runtime.fail();
            })();
          }
          $runtime.fail();
        })() && (() => {
          if ($1 === "None") { return false; }
          if ($1 === "Secondary") { return true; }
          if ($1 === "Primary") { return true; }
          $runtime.fail();
        })();
      })()
        ? "blue"
        : "black"
    )
  ];
};
const viewableTextUnit = {
  isLeaf: v => true,
  createElement: v => text => parent => {
    const $0 = App$dView$dUtil$dD3.createChild(parent)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.Span))(App$dView$dUtil$dD3.fromFoldable([]));
    return () => {
      const rootElement = $0();
      return App$dView$dUtil$dD3.setText(text._1)(rootElement)();
    };
  },
  setSelection: v => text => redraw => rootElement => {
    const $0 = Web$dEvent$dEventTarget.eventListener(x => redraw(App$dUtil.selectionEventData$p(x)._2));
    return () => {
      const listener = $0();
      const $1 = App$dView$dUtil$dD3.styles(rootElement)(App$dView$dUtil$dD3.fromFoldable(textAttrs(textualText)(text)))();
      return App$dView$dUtil.registerMouseListeners(listener)($1)();
    };
  }
};
export {   textAttrs,  viewableTextUnit};
