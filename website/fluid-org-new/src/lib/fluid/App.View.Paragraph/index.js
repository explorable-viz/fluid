import * as App$dUtil$dSelector from "../App.Util.Selector/index.js";
import * as App$dView$dUtil from "../App.View.Util/index.js";
import * as App$dView$dUtil$dD3 from "../App.View.Util.D3/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dFunctorWithIndex from "../Data.FunctorWithIndex/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Effect from "../Effect/index.js";
const $Paragraph = _1 => ({tag: "Paragraph", _1});
const all = /* #__PURE__ */ (() => Data$dFoldable.foldableArray.foldMap((() => {
  const semigroupConj1 = {append: v => v1 => v && v1};
  return {mempty: true, Semigroup0: () => semigroupConj1};
})()))();
const sequence_ = /* #__PURE__ */ Data$dFoldable.traverse_(Effect.applicativeEffect)(Data$dFoldable.foldableArray)(Data$dFoldable.identity);
const Paragraph = value0 => $Paragraph(value0);
const viewableParagraphUnit = {
  isLeaf: v => all(App$dView$dUtil.viewableViewUnit.isLeaf)(v._1),
  createElement: v => v1 => parent => {
    const $0 = v1._1;
    const $1 = App$dView$dUtil$dD3.createChild(parent)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.Div))(App$dView$dUtil$dD3.fromFoldable([
      Data$dTuple.$Tuple("class", "para-text")
    ]));
    return () => {
      const rootElement = $1();
      sequence_(Data$dFunctor.arrayMap(view => {
        const $2 = App$dView$dUtil$dD3.createText(rootElement)(" ");
        return () => {
          $2();
          return view(dictViewable => v1$1 => dictViewable.createElement()(v1$1)(rootElement))();
        };
      })($0))();
      return rootElement;
    };
  },
  setSelection: v => v1 => select => rootElement => sequence_(Data$dFunctorWithIndex.mapWithIndexArray(i => view => {
    const $0 = App$dView$dUtil$dD3.select(":scope > :nth-child(" + Data$dShow.showIntImpl(i + 1 | 0) + ")")(rootElement);
    return () => {
      const child = $0();
      return view(dictViewable => v1$1 => dictViewable.setSelection()(v1$1)(x => select(App$dUtil$dSelector.constrArg("Paragraph")(0)(App$dUtil$dSelector.listElement(i)(x))))(child))();
    };
  })(v1._1))
};
export {$Paragraph,    viewableParagraphUnit};
