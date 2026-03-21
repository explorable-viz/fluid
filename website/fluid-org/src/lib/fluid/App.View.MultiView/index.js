import * as App$dUtil$dSelector from "../App.Util.Selector/index.js";
import * as App$dView$dUtil$dD3 from "../App.View.Util.D3/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dFunctorWithIndex from "../Data.FunctorWithIndex/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Effect from "../Effect/index.js";
const $MultiView = _1 => ({tag: "MultiView", _1});
const sequence_ = /* #__PURE__ */ Data$dFoldable.traverse_(Effect.applicativeEffect)(Data$dFoldable.foldableArray)(Data$dFoldable.identity);
const MultiView = value0 => $MultiView(value0);
const viewableMultiViewUnit = {
  isLeaf: v => false,
  createElement: v => v1 => parent => {
    const $0 = v1._1;
    const $1 = App$dView$dUtil$dD3.createChild(parent)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.Div))(App$dView$dUtil$dD3.fromFoldable([]));
    return () => {
      const rootElement = $1();
      sequence_(Data$dFunctor.arrayMap(view => view(dictViewable => v1$1 => dictViewable.createElement()(v1$1)(rootElement)))($0))();
      return rootElement;
    };
  },
  setSelection: v => v1 => select => rootElement => sequence_(Data$dFunctorWithIndex.mapWithIndexArray(i => view => {
    const $0 = App$dView$dUtil$dD3.select(":scope > :nth-child(" + Data$dShow.showIntImpl(i + 1 | 0) + ")")(rootElement);
    return () => {
      const child = $0();
      return view(dictViewable => v1$1 => dictViewable.setSelection()(v1$1)(x => select(App$dUtil$dSelector.constrArg("MultiView")(0)(App$dUtil$dSelector.listElement(i)(x))))(child))();
    };
  })(v1._1))
};
export {$MultiView,   viewableMultiViewUnit};
