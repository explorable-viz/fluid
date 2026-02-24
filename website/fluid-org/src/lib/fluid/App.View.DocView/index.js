import * as $runtime from "../runtime.js";
import * as App$dView$dParagraph from "../App.View.Paragraph/index.js";
import * as App$dView$dUtil$dD3 from "../App.View.Util.D3/index.js";
const DocView = x => x;
const viewableDocViewUnit = {
  isLeaf: v => {
    if (v.doc.tag === "Nothing") { return v.view(dictViewable => dictViewable.isLeaf); }
    if (v.doc.tag === "Just") { return false; }
    $runtime.fail();
  },
  createElement: v => v1 => v2 => {
    if (v1.doc.tag === "Just") {
      const $0 = v1.doc._1;
      const $1 = App$dView$dUtil$dD3.createChild(v2)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.G))(App$dView$dUtil$dD3.fromFoldable([]));
      return () => {
        const rootElement = $1();
        v1.view(dictViewable => v3 => dictViewable.createElement()(v3)(rootElement))();
        App$dView$dParagraph.viewableParagraphUnit.createElement()($0)(rootElement)();
        return rootElement;
      };
    }
    if (v1.doc.tag === "Nothing") { return v1.view(dictViewable => v3 => dictViewable.createElement()(v3)(v2)); }
    $runtime.fail();
  },
  setSelection: v => v1 => v2 => v3 => {
    if (v1.doc.tag === "Just") {
      const $0 = v1.doc._1;
      const $1 = App$dView$dUtil$dD3.select(":scope > :nth-child(1)")(v3);
      return () => {
        const viewElem = $1();
        v1.view(dictViewable => v4 => dictViewable.setSelection()(v4)(v2)(viewElem))();
        const docElem = App$dView$dUtil$dD3.select(":scope > :nth-child(2)")(v3)();
        App$dView$dParagraph.viewableParagraphUnit.setSelection()($0)(v2)(docElem)();
      };
    }
    if (v1.doc.tag === "Nothing") { return v1.view(dictViewable => v4 => dictViewable.setSelection()(v4)(v2)(v3)); }
    $runtime.fail();
  }
};
export { viewableDocViewUnit};
