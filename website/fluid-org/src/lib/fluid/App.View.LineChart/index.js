import * as $runtime from "../runtime.js";
import * as App$dUtil from "../App.Util/index.js";
import * as App$dUtil$dSelector from "../App.Util.Selector/index.js";
import * as App$dView$dUtil from "../App.View.Util/index.js";
import * as App$dView$dUtil$dAxes from "../App.View.Util.Axes/index.js";
import * as App$dView$dUtil$dD3 from "../App.View.Util.D3/index.js";
import * as Control$dBind from "../Control.Bind/index.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dArray$dNonEmpty$dInternal from "../Data.Array.NonEmpty.Internal/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dFunctorWithIndex from "../Data.FunctorWithIndex/index.js";
import * as Data$dInt from "../Data.Int/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dSemigroup$dFoldable from "../Data.Semigroup.Foldable/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Util from "../Util/index.js";
import * as Web$dEvent$dEventTarget from "../Web.Event.EventTarget/index.js";
const identity = x => x;
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
const meet = v => v1 => {
  if (v1.tag === "Inert") { return App$dUtil.Inert; }
  if (v.tag === "Inert") { return App$dUtil.Inert; }
  if (v.tag === "Reactive" && v1.tag === "Reactive") {
    return App$dUtil.$SelState(
      "Reactive",
      {
        persistent: (() => {
          if (v._1.persistent === "None") { return v._1.persistent; }
          if (v._1.persistent === "Secondary") {
            if (v1._1.persistent === "Secondary") { return v._1.persistent; }
            if (v1._1.persistent === "Primary") { return v._1.persistent; }
            if (v1._1.persistent === "None") { return v1._1.persistent; }
            $runtime.fail();
          }
          if (v._1.persistent === "Primary") {
            if (v1._1.persistent === "Primary") { return v._1.persistent; }
            return v1._1.persistent;
          }
          $runtime.fail();
        })(),
        transient: (() => {
          if (v._1.transient === "None") { return v._1.transient; }
          if (v._1.transient === "Secondary") {
            if (v1._1.transient === "Secondary") { return v._1.transient; }
            if (v1._1.transient === "Primary") { return v._1.transient; }
            if (v1._1.transient === "None") { return v1._1.transient; }
            $runtime.fail();
          }
          if (v._1.transient === "Primary") {
            if (v1._1.transient === "Primary") { return v._1.transient; }
            return v1._1.transient;
          }
          $runtime.fail();
        })()
      }
    );
  }
  $runtime.fail();
};
const minimum = /* #__PURE__ */ Data$dSemigroup$dFoldable.minimum(Data$dOrd.ordNumber)(Data$dArray$dNonEmpty$dInternal.foldable1NonEmptyArray);
const maximum = /* #__PURE__ */ Data$dSemigroup$dFoldable.maximum(Data$dOrd.ordNumber)(Data$dArray$dNonEmpty$dInternal.foldable1NonEmptyArray);
const maximum1 = /* #__PURE__ */ Data$dSemigroup$dFoldable.maximum(Data$dOrd.ordInt)(Data$dArray$dNonEmpty$dInternal.foldable1NonEmptyArray);
const length = /* #__PURE__ */ Data$dFoldable.foldlArray(c => v => 1 + c | 0)(0);
const max = x => y => {
  const v = Data$dOrd.ordInt.compare(x)(y);
  if (v === "LT") { return y; }
  if (v === "EQ") { return x; }
  if (v === "GT") { return x; }
  $runtime.fail();
};
const LinePlot = x => x;
const LineChart = x => x;
const newtypeLinePlot_ = {Coercible0: () => {}};
const point_smallRadius = 2;
const names = plots => Data$dFunctor.arrayMap(x => x.name._1)(plots);
const nameCol = /* #__PURE__ */ App$dView$dUtil$dD3.colorScale("schemePastel1");
const fill = sel => {
  if (
    (() => {
      const $0 = (() => {
        if (sel.tag === "Inert") { return App$dUtil.None; }
        if (sel.tag === "Reactive") { return sel._1.persistent; }
        $runtime.fail();
      })();
      if ($0 === "None") { return false; }
      if ($0 === "Secondary") { return true; }
      if ($0 === "Primary") { return true; }
      $runtime.fail();
    })()
  ) {
    return a => App$dUtil.colorShade(a)(-30);
  }
  return identity;
};
const viewableLineChartUnit = {
  isLeaf: v => false,
  setSelection: v => v1 => redraw => rootElement => {
    const $0 = v1.plots;
    const $1 = App$dView$dUtil$dD3.selectAll(".linechart-point")(rootElement);
    return () => {
      const points = $1();
      const listener = Web$dEvent$dEventTarget.eventListener(x => redraw((() => {
        const $2 = App$dUtil.selectionEventData$p(x);
        return App$dUtil$dSelector.constrArg("LineChart")(0)(App$dUtil$dSelector.dictVal("plots")(App$dUtil$dSelector.listElement($2._1.i)(App$dUtil$dSelector.linePoint($2._1.j)($2._2))));
      })()))();
      for (const point of points) {
        const point$p = App$dView$dUtil$dD3.datum(point)();
        const v3 = Util.unsafeArrayArray.unsafeIndex($0)(point$p.i);
        const $2 = App$dView$dUtil$dD3.attrs(point)(App$dView$dUtil$dD3.fromFoldable((() => {
          const $2 = Util.unsafeArrayArray.unsafeIndex(v3.points)(point$p.j);
          const sel = join($2.x._2)($2.y._2);
          const fill$p = fill(sel)(nameCol(Util.definitely("absurd")((() => {
            const $3 = v3.name._1;
            return Data$dArray.findIndexImpl(Data$dMaybe.Just, Data$dMaybe.Nothing, v$1 => v$1 === $3, Data$dFunctor.arrayMap(x => x.name._1)($0));
          })())));
          return [
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
                  return 2.0;
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
                  return 1.4;
                }
                return 1.0;
              })())
            ),
            Data$dTuple.$Tuple(
              "stroke",
              (() => {
                const $3 = (() => {
                  if (sel.tag === "Inert") { return App$dUtil.None; }
                  if (sel.tag === "Reactive") { return sel._1.transient; }
                  $runtime.fail();
                })();
                if ($3 === "None") { return false; }
                if ($3 === "Secondary") { return true; }
                if ($3 === "Primary") { return true; }
                $runtime.fail();
              })()
                ? App$dUtil.colorShade(fill$p)(-30)
                : fill$p
            ),
            Data$dTuple.$Tuple("fill", fill$p)
          ];
        })()))();
        App$dView$dUtil.registerMouseListeners(listener)($2)();
      }
      const segments = App$dView$dUtil$dD3.selectAll(".linechart-segment")(rootElement)();
      for (const segment of segments) {
        const segment$p = App$dView$dUtil$dD3.datum(segment)();
        const v3 = Util.unsafeArrayArray.unsafeIndex($0)(segment$p.i);
        App$dView$dUtil$dD3.attrs(segment)(App$dView$dUtil$dD3.fromFoldable((() => {
          const sel = meet((() => {
            const $2 = Util.unsafeArrayArray.unsafeIndex(v3.points)(segment$p.j1);
            return join($2.x._2)($2.y._2);
          })())((() => {
            const $2 = Util.unsafeArrayArray.unsafeIndex(v3.points)(segment$p.j2);
            return join($2.x._2)($2.y._2);
          })());
          return [
            Data$dTuple.$Tuple(
              "stroke",
              ((() => {
                const $2 = (() => {
                  if (sel.tag === "Inert") { return App$dUtil.None; }
                  if (sel.tag === "Reactive") { return sel._1.transient; }
                  $runtime.fail();
                })();
                if ($2 === "None") { return false; }
                if ($2 === "Secondary") { return true; }
                if ($2 === "Primary") { return true; }
                $runtime.fail();
              })()
                ? (a => App$dUtil.colorShade(a)(-30))
                : identity)(fill(sel)(nameCol(Util.definitely("absurd")((() => {
                const $2 = v3.name._1;
                return Data$dArray.findIndexImpl(Data$dMaybe.Just, Data$dMaybe.Nothing, v$1 => v$1 === $2, Data$dFunctor.arrayMap(x => x.name._1)($0));
              })()))))
            ),
            Data$dTuple.$Tuple(
              "stroke-width",
              Data$dShow.showIntImpl((() => {
                if (
                  (() => {
                    const $2 = (() => {
                      if (sel.tag === "Inert") { return App$dUtil.None; }
                      if (sel.tag === "Reactive") { return sel._1.transient; }
                      $runtime.fail();
                    })();
                    if ($2 === "None") { return false; }
                    if ($2 === "Secondary") { return true; }
                    if ($2 === "Primary") { return true; }
                    $runtime.fail();
                  })()
                ) {
                  return 2;
                }
                if (
                  (() => {
                    const $2 = (() => {
                      if (sel.tag === "Inert") { return App$dUtil.None; }
                      if (sel.tag === "Reactive") { return sel._1.persistent; }
                      $runtime.fail();
                    })();
                    if ($2 === "None") { return false; }
                    if ($2 === "Secondary") { return true; }
                    if ($2 === "Primary") { return true; }
                    $runtime.fail();
                  })()
                ) {
                  return 2;
                }
                return 1;
              })())
            )
          ];
        })()))();
      }
    };
  },
  createElement: v => v1 => parent => {
    const $0 = v1.caption;
    const $1 = v1.plots;
    const $2 = v1.size;
    const $3 = v1.tickLabels;
    const caption_height = App$dView$dUtil$dD3.textDimensions("title-text")($0._1).height * 2 | 0;
    const $4 = $2.width._1;
    const $5 = $2.height._1;
    const $6 = Control$dBind.arrayBind(Data$dFunctor.arrayMap(x => x.points)($1))(Control$dBind.identity);
    const ps = Util.definitely("non-empty")($6.length > 0 ? Data$dMaybe.$Maybe("Just", $6) : Data$dMaybe.Nothing);
    const $7 = Data$dFunctor.arrayMap(x => x.x._1)(ps);
    const $8 = minimum($7);
    const $9 = maximum($7);
    const $10 = maximum(Data$dFunctor.arrayMap(x => x.y._1)(ps));
    const to = v3 => (
      {
        x: App$dView$dUtil$dD3.scaleLinear({min: $8, max: $9})({min: 0.0, max: Data$dInt.toNumber(v3.width)}),
        y: App$dView$dUtil$dD3.scaleLinear({min: 0.0, max: $10})({min: Data$dInt.toNumber(v3.height), max: 0.0})
      }
    );
    const $11 = (
      15 + maximum1((() => {
        const $11 = App$dView$dUtil$dD3.textDimensions("legend-text");
        const $12 = Data$dFunctor.arrayMap(x => $11(x.name._1).width)($1);
        return Util.definitely("non-empty")($12.length > 0 ? Data$dMaybe.$Maybe("Just", $12) : Data$dMaybe.Nothing);
      })()) | 0
    ) + 4 | 0;
    const $12 = 15 * length($1) | 0;
    const createAxes = (range, parent$p) => {
      const $13 = $3.y;
      const $14 = App$dView$dUtil$dAxes.create_xAxis(parent$p)(to(range))(Data$dArray.nubBy(Data$dOrd.ordNumber.compare)($7))(range.height)($3.x._1);
      return () => {
        const x = $14();
        const y = App$dView$dUtil$dAxes.create_yAxis(parent$p)(to(range))(3.0)(1)($13._1)();
        return {x, y};
      };
    };
    const $13 = App$dView$dUtil$dD3.createChild(parent)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.SVG))(App$dView$dUtil$dD3.fromFoldable([
      Data$dTuple.$Tuple("width", Data$dShow.showIntImpl($4)),
      Data$dTuple.$Tuple("height", Data$dShow.showIntImpl($5))
    ]));
    return () => {
      const svg = $13();
      const v3 = createAxes({width: $2.width._1, height: $2.height._1}, svg)();
      const a$p = App$dView$dUtil$dD3.dimensions(v3.x)();
      const a$p$1 = App$dView$dUtil$dD3.dimensions(v3.y)();
      App$dView$dUtil$dD3.remove(v3.x)();
      App$dView$dUtil$dD3.remove(v3.y)();
      const interior = {width: ((($4 - a$p$1.width | 0) - 3 | 0) - $11 | 0) - 15 | 0, height: (($5 - 6 | 0) - a$p.height | 0) - caption_height | 0};
      const g = App$dView$dUtil$dD3.createChild(svg)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.G))(App$dView$dUtil$dD3.fromFoldable([
        App$dView$dUtil$dD3.translate({x: a$p$1.width, y: 6})
      ]))();
      createAxes(interior, g)();
      for (
        const v3$1 of Data$dArray.concat(Data$dFunctorWithIndex.mapWithIndexArray(i => v3$2 => {
          if (v3$2.points.length > 0) {
            return Data$dArray.zipWithImpl(
              v5 => {
                const $14 = v5._2;
                const $15 = v5._1;
                return v6 => Data$dTuple.$Tuple({name: v3$2.name._1, start: $15, end: v6._1}, {i, j1: $14, j2: v6._2});
              },
              Data$dFunctorWithIndex.mapWithIndexArray(j => point => Data$dTuple.$Tuple({x: point.x._1, y: point.y._1}, j))((() => {
                if (v3$2.points.length === 0) { $runtime.fail(); }
                return Data$dArray.sliceImpl(0, v3$2.points.length - 1 | 0, v3$2.points);
              })()),
              Data$dFunctorWithIndex.mapWithIndexArray(j => point => Data$dTuple.$Tuple({x: point.x._1, y: point.y._1}, j + 1 | 0))((() => {
                const $14 = Data$dArray.unconsImpl(v$1 => Data$dMaybe.Nothing, v$1 => xs => Data$dMaybe.$Maybe("Just", xs), v3$2.points);
                if ($14.tag === "Just") { return $14._1; }
                $runtime.fail();
              })())
            );
          }
          return [];
        })($1))
      ) {
        const $14 = App$dView$dUtil$dD3.createChild(g)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.Path))(App$dView$dUtil$dD3.fromFoldable([
          App$dUtil.classes(["linechart-segment"]),
          Data$dTuple.$Tuple("d", App$dView$dUtil$dD3.line(to(interior))([v3$1._1.start, v3$1._1.end]))
        ]))();
        App$dView$dUtil$dD3.setDatum(v3$1._2)($14)();
      }
      for (
        const v3$1 of Data$dArray.concat(Data$dFunctorWithIndex.mapWithIndexArray(i => v3$2 => Data$dFunctorWithIndex.mapWithIndexArray(j => p => Data$dTuple.$Tuple(p, {i, j}))(v3$2.points))($1))
      ) {
        const $14 = App$dView$dUtil$dD3.createChild(g)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.Circle))(App$dView$dUtil$dD3.fromFoldable([
          App$dUtil.classes(["linechart-point"]),
          Data$dTuple.$Tuple("stroke-width", "1"),
          Data$dTuple.$Tuple("cx", Data$dShow.showNumberImpl(to(interior).x(v3$1._1.x._1))),
          Data$dTuple.$Tuple("cy", Data$dShow.showNumberImpl(to(interior).y(v3$1._1.y._1)))
        ]))();
        App$dView$dUtil$dD3.setDatum({i: v3$1._2.i, j: v3$1._2.j})($14)();
      }
      const $14 = App$dView$dUtil$dD3.createChild(svg)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.Text))(App$dView$dUtil$dD3.fromFoldable([
        Data$dTuple.$Tuple("x", Data$dShow.showIntImpl($runtime.intDiv($4, 2))),
        Data$dTuple.$Tuple("y", Data$dShow.showIntImpl($5 - $runtime.intDiv(caption_height, 2) | 0)),
        App$dUtil.classes(["title-text"]),
        Data$dTuple.$Tuple("dominant-baseline", "middle"),
        Data$dTuple.$Tuple("text-anchor", "middle")
      ]))();
      App$dView$dUtil$dD3.setText($0._1)($14)();
      const legend$p = App$dView$dUtil$dD3.createChild(g)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.G))(App$dView$dUtil$dD3.fromFoldable([
        App$dView$dUtil$dD3.translate({x: interior.width + 15 | 0, y: max(0)($runtime.intDiv(interior.height - $12 | 0, 2))})
      ]))();
      App$dView$dUtil$dD3.createChild(legend$p)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.Rect))(App$dView$dUtil$dD3.fromFoldable([
        App$dUtil.classes(["legend-box"]),
        Data$dTuple.$Tuple("x", "0"),
        Data$dTuple.$Tuple("y", "0"),
        Data$dTuple.$Tuple("height", Data$dShow.showIntImpl($12)),
        Data$dTuple.$Tuple("width", Data$dShow.showIntImpl($11))
      ]))();
      for (const v4 of Data$dFunctorWithIndex.mapWithIndexArray(i => v4$1 => ({i, name: v4$1.name._1}))($1)) {
        const $15 = v4.name;
        const g$1 = App$dView$dUtil$dD3.createChild(legend$p)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.G))(App$dView$dUtil$dD3.fromFoldable([
          App$dUtil.classes(["legend-entry"]),
          App$dView$dUtil$dD3.translate({x: 0, y: (v4.i * 15 | 0) + 2 | 0})
        ]))();
        const $16 = App$dView$dUtil$dD3.createChild(g$1)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.Text))(App$dView$dUtil$dD3.fromFoldable([
          App$dUtil.classes(["legend-text"]),
          App$dView$dUtil$dD3.translate({x: 15, y: 9})
        ]))();
        App$dView$dUtil$dD3.setText($15)($16)();
        App$dView$dUtil$dD3.createChild(g$1)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.Circle))(App$dView$dUtil$dD3.fromFoldable([
          Data$dTuple.$Tuple(
            "fill",
            nameCol(Util.definitely("absurd")(Data$dArray.findIndexImpl(Data$dMaybe.Just, Data$dMaybe.Nothing, v$1 => v$1 === $15, Data$dFunctor.arrayMap(x => x.name._1)($1))))
          ),
          Data$dTuple.$Tuple("r", "2"),
          Data$dTuple.$Tuple("cx", "6"),
          Data$dTuple.$Tuple("cy", "6")
        ]))();
      }
      return g;
    };
  }
};
export {               viewableLineChartUnit};
