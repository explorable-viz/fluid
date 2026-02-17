import * as $runtime from "../runtime.js";
import * as App$dUtil from "../App.Util/index.js";
import * as App$dUtil$dSelector from "../App.Util.Selector/index.js";
import * as App$dView$dUtil$dD3 from "../App.View.Util.D3/index.js";
import * as Control$dMonad$dError$dClass from "../Control.Monad.Error.Class/index.js";
import * as Data$dArgonaut$dCore from "../Data.Argonaut.Core/index.js";
import * as Data$dArgonaut$dDecode$dError from "../Data.Argonaut.Decode.Error/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dFunctorWithIndex from "../Data.FunctorWithIndex/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Data$dUnfoldable from "../Data.Unfoldable/index.js";
import * as Effect from "../Effect/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
import * as Util from "../Util/index.js";
import * as Util$dMap from "../Util.Map/index.js";
const $Direction = tag => tag;
const $Filter = tag => tag;
const all = /* #__PURE__ */ (() => Data$dList$dTypes.foldableList.foldMap((() => {
  const semigroupConj1 = {append: v => v1 => v && v1};
  return {mempty: true, Semigroup0: () => semigroupConj1};
})()))();
const sequence_ = /* #__PURE__ */ Data$dFoldable.traverse_(Effect.applicativeEffect)(Data$dFoldable.foldableArray)(Data$dFoldable.identity);
const toUnfoldable = /* #__PURE__ */ Foreign$dObject.toAscUnfoldable(Data$dUnfoldable.unfoldableArray);
const View = x => x;
const Everything = /* #__PURE__ */ $Filter("Everything");
const Interactive = /* #__PURE__ */ $Filter("Interactive");
const Relevant = /* #__PURE__ */ $Filter("Relevant");
const LinkedInputs = /* #__PURE__ */ $Direction("LinkedInputs");
const LinkedOutputs = /* #__PURE__ */ $Direction("LinkedOutputs");
const Intermediates = /* #__PURE__ */ $Direction("Intermediates");
const eqFilter = {
  eq: x => y => {
    if (x === "Everything") { return y === "Everything"; }
    if (x === "Interactive") { return y === "Interactive"; }
    return x === "Relevant" && y === "Relevant";
  }
};
const eqDirection = {
  eq: x => y => {
    if (x === "LinkedInputs") { return y === "LinkedInputs"; }
    if (x === "LinkedOutputs") { return y === "LinkedOutputs"; }
    return x === "Intermediates" && y === "Intermediates";
  }
};
const unpack = v => k => v(dictViewable => k(dictViewable));
const uiHelpers = {
  val: Data$dTuple.fst,
  selState: Data$dTuple.snd,
  join: v => v1 => {
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
  },
  selClasses: App$dUtil.selClasses,
  selClassesFor: App$dUtil.selClassesFor
};
const setSelection = dict => dict.setSelection;
const registerMouseListeners = redraw => element => () => {
  for (const ev of ["mousedown", "mouseenter", "mouseleave"]) {
    App$dView$dUtil$dD3.on(ev)(redraw)(element)();
  }
};
const pack = dictViewable => x => v => v(dictViewable)(x);
const isLeaf = dict => dict.isLeaf;
const decodeJsonFilter = {
  decodeJson: json => {
    const $0 = Data$dArgonaut$dCore._caseJson(
      v => Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "String")),
      v => Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "String")),
      v => Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "String")),
      Data$dEither.Right,
      v => Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "String")),
      v => Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "String")),
      json
    );
    return (() => {
      if ($0.tag === "Left") {
        const $1 = $0._1;
        return v => Data$dEither.$Either("Left", $1);
      }
      if ($0.tag === "Right") {
        const $1 = $0._1;
        return f => f($1);
      }
      $runtime.fail();
    })()(s => {
      if (s === "Everything") { return Data$dEither.$Either("Right", Everything); }
      if (s === "Interactive") { return Data$dEither.$Either("Right", Interactive); }
      if (s === "Relevant") { return Data$dEither.$Either("Right", Relevant); }
      return Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "Unknown Filter: " + s));
    });
  }
};
const createElement = dict => dict.createElement;
const draw = dictViewable => v => v1 => select$p => {
  const $0 = v1.divId;
  const $1 = v1.view;
  const childId = $0 + "-" + v1.suffix;
  const $2 = App$dView$dUtil$dD3.rootSelect("#" + $0);
  return () => {
    const div = $2();
    const a$p = App$dView$dUtil$dD3.isEmpty(div)();
    Util.check(Control$dMonad$dError$dClass.monadThrowEffect)(!a$p)("Unable to insert figure: no div found with id " + $0)();
    const maybeRootElement = App$dView$dUtil$dD3.select("#" + childId)(div)();
    const v2 = App$dView$dUtil$dD3.isEmpty(maybeRootElement)();
    const $3 = (() => {
      if (v2) {
        const a$p$1 = dictViewable.createElement()($1)(div)();
        return App$dView$dUtil$dD3.attrs(a$p$1)(App$dView$dUtil$dD3.fromFoldable([Data$dTuple.$Tuple("id", childId)]))();
      }
      return maybeRootElement;
    })();
    return dictViewable.setSelection()($1)(select$p)($3)();
  };
};
const drawView = v => redraw => v.view(dictViewable => view => draw(dictViewable)(uiHelpers)({divId: v.divId, suffix: v.suffix, view})(redraw));
const viewableViewUnit = {
  isLeaf: view => view(dictViewable => dictViewable.isLeaf),
  createElement: v => view => parent => view(dictViewable => v1 => dictViewable.createElement()(v1)(parent)),
  setSelection: v => view => select => rootElement => view(dictViewable => v1 => dictViewable.setSelection()(v1)(select)(rootElement))
};
const viewableDictView$x215ViewUnit = {
  isLeaf: views => Foreign$dObject.size(views) === 0,
  createElement: v => views => parent => {
    const $0 = App$dView$dUtil$dD3.createChild(parent)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.Div))(App$dView$dUtil$dD3.fromFoldable([
      App$dUtil.classes(["tree-children", ...all(viewableViewUnit.isLeaf)(Data$dList$dTypes.listMap(Data$dTuple.snd)(Util$dMap.mapObjectString.values(views))) ? ["columnar"] : []])
    ]));
    return () => {
      const rootElement = $0();
      sequence_(Data$dFunctor.arrayMap(v1 => {
        const $1 = App$dView$dUtil$dD3.createChild(rootElement)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.Div))(App$dView$dUtil$dD3.fromFoldable([
          App$dUtil.classes(["tree-node"])
        ]));
        return () => {
          const child = $1();
          const key = v1._2._1(dictViewable => v1$1 => dictViewable.createElement()(v1$1)(child))();
          App$dView$dUtil$dD3.attrs(key)(App$dView$dUtil$dD3.fromFoldable([App$dUtil.classes(["tree-label"])]))();
          return v1._2._2(dictViewable => v1$1 => dictViewable.createElement()(v1$1)(child))();
        };
      })(toUnfoldable(views)))();
      return rootElement;
    };
  },
  setSelection: v => views => select => rootElement => sequence_(Data$dFunctorWithIndex.mapWithIndexArray(i => v1 => {
    const $0 = v1._1;
    const $1 = App$dView$dUtil$dD3.select(":scope > :nth-child(" + Data$dShow.showIntImpl(i + 1 | 0) + ")")(rootElement);
    return () => {
      const child = $1();
      const child1 = App$dView$dUtil$dD3.select(":scope > :nth-child(1)")(child)();
      const child2 = App$dView$dUtil$dD3.select(":scope > :nth-child(2)")(child)();
      v1._2._1(dictViewable => v1$1 => dictViewable.setSelection()(v1$1)(v2 => () => {})(child1))();
      v1._2._2(dictViewable => v1$1 => dictViewable.setSelection()(v1$1)(x => select(App$dUtil$dSelector.dictVal($0)(x)))(child2))();
    };
  })(toUnfoldable(views)))
};
export {
  
  
  Everything,
  Interactive,
  Intermediates,
  LinkedInputs,
  LinkedOutputs,
  
  
  
  
  decodeJsonFilter,
  
  drawView,
  
  
  
  
  registerMouseListeners,
  
  
  
  uiHelpers,
  
  viewableDictView$x215ViewUnit,
  viewableViewUnit
};
