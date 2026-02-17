import * as $runtime from "../runtime.js";
import * as App$dUtil from "../App.Util/index.js";
import * as App$dView$dText from "../App.View.Text/index.js";
import * as App$dView$dUtil from "../App.View.Util/index.js";
import * as App$dView$dUtil$dD3 from "../App.View.Util.D3/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Val from "../Val/index.js";
import * as Web$dEvent$dEventTarget from "../Web.Event.EventTarget/index.js";
const $Link = (_1, _2) => ({tag: "Link", _1, _2});
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
const Link = value0 => value1 => $Link(value0, value1);
const textualLink = {getText: v => Data$dTuple.$Tuple(v._2._1, Val.foldableVal.foldr(join)(App$dUtil.Inert)(v._1))};
const linkContents = v => v._2._1;
const viewableLinkUnit = {
  isLeaf: v => true,
  createElement: v => link => parent => {
    const $0 = App$dView$dUtil$dD3.createChild(parent)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.Text))(App$dView$dUtil$dD3.fromFoldable([
      App$dUtil.classes(["link"])
    ]));
    return () => {
      const rootElement = $0();
      const $1 = App$dView$dUtil$dD3.setText(link._2._1)(rootElement)();
      return App$dView$dUtil$dD3.setDatum(link)($1)();
    };
  },
  setSelection: v => link => redraw => rootElement => {
    const $0 = Web$dEvent$dEventTarget.eventListener(x => redraw(v2 => {
      if (v2._3.tag === "Constr" && v2._3._2.tag === "Cons" && v2._3._2._2.tag === "Cons" && v2._3._2._2._2.tag === "Nil" && v2._3._1 === "Link") {
        const $0 = App$dUtil.selectionEventData$p(x)._2(v2._3._2._1);
        return Data$dTuple.$Tuple(
          Val.$Val(v2._1, v2._2, Val.$BaseVal("Constr", v2._3._1, Data$dList$dTypes.$List("Cons", $0._1, Data$dList$dTypes.$List("Cons", v2._3._2._2._1, Data$dList$dTypes.Nil)))),
          $0._2
        );
      }
      $runtime.fail();
    }));
    return () => {
      const listener = $0();
      const $1 = App$dView$dUtil$dD3.styles(rootElement)(App$dView$dUtil$dD3.fromFoldable(App$dView$dText.textAttrs(textualLink)(link)))();
      return App$dView$dUtil.registerMouseListeners(listener)($1)();
    };
  }
};
export {$Link,     viewableLinkUnit};
