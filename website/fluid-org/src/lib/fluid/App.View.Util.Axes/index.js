import * as $runtime from "../runtime.js";
import * as App$dUtil from "../App.Util/index.js";
import * as App$dView$dUtil$dD3 from "../App.View.Util.D3/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Effect from "../Effect/index.js";
import * as Primitive from "../Primitive/index.js";
import * as Val from "../Val/index.js";
const $Orientation = tag => tag;
const for_ = /* #__PURE__ */ Data$dFoldable.for_(Effect.applicativeEffect)(Data$dFoldable.foldableArray);
const Default = /* #__PURE__ */ $Orientation("Default");
const Rotated = /* #__PURE__ */ $Orientation("Rotated");
const eqOrientation = {
  eq: x => y => {
    if (x === "Default") { return y === "Default"; }
    return x === "Rotated" && y === "Rotated";
  }
};
const orientation = {
  pack: v => {
    if (v === "Default") { return Val.$BaseVal("Constr", "Default", Data$dList$dTypes.Nil); }
    if (v === "Rotated") { return Val.$BaseVal("Constr", "Rotated", Data$dList$dTypes.Nil); }
    $runtime.fail();
  },
  unpack: v => {
    if (v.tag === "Constr" && v._2.tag === "Nil") {
      if (v._1 === "Default") { return Default; }
      if (v._1 === "Rotated") { return Rotated; }
    }
    return Primitive.typeError(v)("Orientation");
  }
};
const create_yAxis = parent$p => to => nTicks => decPlaces => orient => {
  const $0 = App$dView$dUtil$dD3.yAxis(to)(nTicks)(decPlaces);
  const $1 = App$dView$dUtil$dD3.createChild(parent$p)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.G))(App$dView$dUtil$dD3.fromFoldable([
    App$dUtil.classes(["y-axis"])
  ]));
  return () => {
    const $2 = $1();
    const y = $0($2)();
    const $3 = App$dView$dUtil$dD3.selectAll("text")(y);
    if (orient === "Rotated") {
      const labels = $3();
      for_(labels)(a => {
        const $4 = App$dView$dUtil$dD3.attrs(a)(App$dView$dUtil$dD3.fromFoldable([Data$dTuple.$Tuple("transform", "rotate(45)")]));
        return () => {
          const $5 = $4();
          return App$dView$dUtil$dD3.styles($5)(App$dView$dUtil$dD3.fromFoldable([Data$dTuple.$Tuple("text-anchor", "end")]))();
        };
      })();
    }
    return y;
  };
};
const create_xAxis = parent$p => to => ticks => y => orient => {
  const $0 = App$dView$dUtil$dD3.xAxis(to)(ticks);
  const $1 = App$dView$dUtil$dD3.createChild(parent$p)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.G))(App$dView$dUtil$dD3.fromFoldable([
    App$dUtil.classes(["x-axis"]),
    App$dView$dUtil$dD3.translate({x: 0, y})
  ]));
  return () => {
    const $2 = $1();
    const x = $0($2)();
    const $3 = App$dView$dUtil$dD3.selectAll("text")(x);
    if (orient === "Rotated") {
      const labels = $3();
      for_(labels)(a => {
        const $4 = App$dView$dUtil$dD3.attrs(a)(App$dView$dUtil$dD3.fromFoldable([Data$dTuple.$Tuple("transform", "rotate(45)")]));
        return () => {
          const $5 = $4();
          return App$dView$dUtil$dD3.styles($5)(App$dView$dUtil$dD3.fromFoldable([Data$dTuple.$Tuple("text-anchor", "start")]))();
        };
      })();
    }
    return x;
  };
};
export {   create_xAxis, create_yAxis,   orientation};
