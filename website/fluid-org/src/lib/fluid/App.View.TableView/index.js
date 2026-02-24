import * as $runtime from "../runtime.js";
import * as App$dUtil from "../App.Util/index.js";
import * as App$dUtil$dSelector from "../App.Util.Selector/index.js";
import * as App$dView$dUtil from "../App.View.Util/index.js";
import * as App$dView$dUtil$dD3 from "../App.View.Util.D3/index.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dFoldableWithIndex from "../Data.FoldableWithIndex/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNumber$dFormat from "../Data.Number.Format/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dTraversable from "../Data.Traversable/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Data$dUnfoldable from "../Data.Unfoldable/index.js";
import * as Dict from "../Dict/index.js";
import * as Effect from "../Effect/index.js";
import * as Effect$dException from "../Effect.Exception/index.js";
import * as Util from "../Util/index.js";
import * as Util$dMap from "../Util.Map/index.js";
import * as Web$dEvent$dEventTarget from "../Web.Event.EventTarget/index.js";
const $BorderVis = tag => tag;
const toUnfoldable = /* #__PURE__ */ (() => {
  const $0 = Data$dUnfoldable.unfoldableArray.unfoldr(xs => {
    if (xs.tag === "Nil") { return Data$dMaybe.Nothing; }
    if (xs.tag === "Cons") { return Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple(xs._1, xs._2)); }
    $runtime.fail();
  });
  return x => $0((() => {
    const go = (m$p, z$p) => {
      if (m$p.tag === "Leaf") { return z$p; }
      if (m$p.tag === "Node") { return go(m$p._5, Data$dList$dTypes.$List("Cons", m$p._3, go(m$p._6, z$p))); }
      $runtime.fail();
    };
    return go(x, Data$dList$dTypes.Nil);
  })());
})();
const $$for = /* #__PURE__ */ (() => {
  const traverse2 = Data$dTraversable.traversableArray.traverse(Effect.applicativeEffect);
  return x => f => traverse2(f)(x);
})();
const forWithIndex_ = /* #__PURE__ */ Data$dFoldableWithIndex.forWithIndex_(Effect.applicativeEffect)(Data$dFoldableWithIndex.foldableWithIndexArray);
const TableView = x => x;
const Transparent = /* #__PURE__ */ $BorderVis("Transparent");
const Faint = /* #__PURE__ */ $BorderVis("Faint");
const visible = filter => v => {
  if (filter === "Everything") { return true; }
  if (filter === "Interactive") {
    if (v._1.tag === "Inert") { return false; }
    if (v._1.tag === "Reactive") { return true; }
    $runtime.fail();
  }
  if (filter === "Relevant") {
    return !(
      (() => {
        const $0 = (() => {
          if (v._1.tag === "Inert") { return App$dUtil.None; }
          if (v._1.tag === "Reactive") { return v._1._1.persistent; }
          $runtime.fail();
        })();
        const $1 = (() => {
          if (v._1.tag === "Inert") { return App$dUtil.None; }
          if (v._1.tag === "Reactive") { return v._1._1.transient; }
          $runtime.fail();
        })();
        return (() => {
          if ($0 === "None") { return true; }
          if ($0 === "Secondary") { return false; }
          if ($0 === "Primary") { return false; }
          $runtime.fail();
        })() && (() => {
          if ($1 === "None") { return true; }
          if ($1 === "Secondary") { return false; }
          if ($1 === "Primary") { return false; }
          $runtime.fail();
        })();
      })() || (() => {
        if (v._1.tag === "Inert") { return true; }
        if (v._1.tag === "Reactive") { return false; }
        $runtime.fail();
      })()
    );
  }
  $runtime.fail();
};
const unhighlightedBorder = v => {
  if (v === "Transparent") { return "1px solid transparent"; }
  if (v === "Faint") { return "1px solid #eee"; }
  $runtime.fail();
};
const rowKey = "__n";
const prim = v => {
  if (v._3.tag === "Int") { return Data$dShow.showIntImpl(v._3._1); }
  if (v._3.tag === "Float") { return Data$dNumber$dFormat.toStringWith(Data$dNumber$dFormat.$Format("Fixed", Data$dNumber$dFormat.clamp(0)(20)(2)))(v._3._1); }
  if (v._3.tag === "Str") { return v._3._1; }
  return Effect$dException.throwException(Effect$dException.error("TableView only supports primitive values."))();
};
const highlightedBorder = v => "1px solid blue";
const headers = records => Data$dArray.sortBy(Data$dOrd.ordString.compare)(toUnfoldable((() => {
  const $0 = Util.definitely("non-empty")(records.length > 0 ? Data$dMaybe.$Maybe("Just", records) : Data$dMaybe.Nothing);
  return Util$dMap.mapObjectString.keys((() => {
    if (0 < $0.length) { return $0[0]; }
    $runtime.fail();
  })());
})()));
const cell_selClassesFor = colName => s => {
  if (colName === "__n") { return ""; }
  return App$dUtil.selClassesFor(s);
};
const viewableTableViewUnit = {
  isLeaf: v => false,
  setSelection: v => v1 => redraw => rootElement => {
    const $0 = v1.colNames;
    const $1 = v1.rows;
    const $2 = v1.title;
    const row_isVisible = Data$dFunctor.arrayMap(Data$dArray.any(visible(v1.rowFilter)))($1);
    const row_visiblePred = row_visiblePred$a0$copy => {
      let row_visiblePred$a0 = row_visiblePred$a0$copy, row_visiblePred$c = true, row_visiblePred$r;
      while (row_visiblePred$c) {
        const i = row_visiblePred$a0;
        if (i < 0) {
          row_visiblePred$c = false;
          row_visiblePred$r = Effect$dException.throwException(Effect$dException.error("absurd"))();
          continue;
        }
        if (i === 0) {
          row_visiblePred$c = false;
          row_visiblePred$r = -1;
          continue;
        }
        if (Util.unsafeArrayArray.unsafeIndex(row_isVisible)(i - 1 | 0)) {
          row_visiblePred$c = false;
          row_visiblePred$r = i - 1 | 0;
          continue;
        }
        row_visiblePred$a0 = i - 1 | 0;
      }
      return row_visiblePred$r;
    };
    const row_visibleSucc = row_visibleSucc$a0$copy => {
      let row_visibleSucc$a0 = row_visibleSucc$a0$copy, row_visibleSucc$c = true, row_visibleSucc$r;
      while (row_visibleSucc$c) {
        const i = row_visibleSucc$a0;
        if (i === ($1.length - 1 | 0)) {
          row_visibleSucc$c = false;
          row_visibleSucc$r = Data$dMaybe.Nothing;
          continue;
        }
        if (Util.unsafeArrayArray.unsafeIndex(row_isVisible)(i + 1 | 0)) {
          row_visibleSucc$c = false;
          row_visibleSucc$r = Data$dMaybe.$Maybe("Just", i + 1 | 0);
          continue;
        }
        row_visibleSucc$a0 = i + 1 | 0;
      }
      return row_visibleSucc$r;
    };
    const isCellTransient = (i, j) => {
      if (i === -1 || j === -1) { return false; }
      const $3 = Util.unsafeArrayArray.unsafeIndex(Util.unsafeArrayArray.unsafeIndex($1)(i))(j);
      const $4 = (() => {
        if ($3._1.tag === "Inert") { return App$dUtil.None; }
        if ($3._1.tag === "Reactive") { return $3._1._1.transient; }
        $runtime.fail();
      })();
      if ($4 === "None") { return false; }
      if ($4 === "Secondary") { return true; }
      if ($4 === "Primary") { return true; }
      $runtime.fail();
    };
    const $3 = App$dView$dUtil$dD3.selectAll(".table-row")(rootElement);
    const column_isVisible = Data$dFunctor.arrayMap(Data$dArray.any(visible($0.length >= 10 ? App$dView$dUtil.Interactive : App$dView$dUtil.Everything)))(Data$dArray.transpose($1));
    const column_visiblePred = column_visiblePred$a0$copy => {
      let column_visiblePred$a0 = column_visiblePred$a0$copy, column_visiblePred$c = true, column_visiblePred$r;
      while (column_visiblePred$c) {
        const j = column_visiblePred$a0;
        if (j < 0) {
          column_visiblePred$c = false;
          column_visiblePred$r = Effect$dException.throwException(Effect$dException.error("absurd"))();
          continue;
        }
        if (j === 0) {
          column_visiblePred$c = false;
          column_visiblePred$r = -1;
          continue;
        }
        if (Util.unsafeArrayArray.unsafeIndex(column_isVisible)(j - 1 | 0)) {
          column_visiblePred$c = false;
          column_visiblePred$r = j - 1 | 0;
          continue;
        }
        column_visiblePred$a0 = j - 1 | 0;
      }
      return column_visiblePred$r;
    };
    const column_visibleSucc = column_visibleSucc$a0$copy => {
      let column_visibleSucc$a0 = column_visibleSucc$a0$copy, column_visibleSucc$c = true, column_visibleSucc$r;
      while (column_visibleSucc$c) {
        const i = column_visibleSucc$a0;
        if (i === ($0.length - 1 | 0)) {
          column_visibleSucc$c = false;
          column_visibleSucc$r = Data$dMaybe.Nothing;
          continue;
        }
        if (Util.unsafeArrayArray.unsafeIndex(column_isVisible)(i + 1 | 0)) {
          column_visibleSucc$c = false;
          column_visibleSucc$r = Data$dMaybe.$Maybe("Just", i + 1 | 0);
          continue;
        }
        column_visibleSucc$a0 = i + 1 | 0;
      }
      return column_visibleSucc$r;
    };
    const hiddenColumns = Data$dArray.filterImpl(x => !Util.unsafeArrayArray.unsafeIndex(column_isVisible)(x), Data$dArray.rangeImpl(0, $0.length - 1 | 0));
    const $4 = App$dView$dUtil$dD3.selectAll(".table-cell")(rootElement);
    const $5 = App$dView$dUtil$dD3.selectAll(".table-cell")(rootElement);
    return () => {
      const cells = $5();
      const listener = Web$dEvent$dEventTarget.eventListener(x => redraw((() => {
        const $6 = App$dUtil.selectionEventData$p(x);
        return App$dUtil$dSelector.listElement($6._1.i)(App$dUtil$dSelector.dictVal($6._1.colName)($6._2));
      })()))();
      for (const cell of cells) {
        const v2 = App$dView$dUtil$dD3.datum(cell)();
        if (v2.i === -1 || v2.j === -1) {

        } else {
          const $6 = App$dView$dUtil$dD3.classed(App$dUtil.selClasses)(false)(cell)();
          const $7 = App$dView$dUtil$dD3.classed(v2.colName === "__n"
            ? ""
            : App$dUtil.selClassesFor(Util.unsafeArrayArray.unsafeIndex(Util.unsafeArrayArray.unsafeIndex($1)(v2.i))(v2.j)._1))(true)($6)();
          App$dView$dUtil.registerMouseListeners(listener)($7)();
        }
        App$dView$dUtil$dD3.styles(cell)(App$dView$dUtil$dD3.fromFoldable([
          Data$dTuple.$Tuple(
            "border-right",
            (() => {
              const v2$1 = column_visibleSucc(v2.j);
              const $6 = column_visibleSucc(v2.j);
              const $7 = (() => {
                if ($6.tag === "Nothing") { return true; }
                if ($6.tag === "Just") { return false; }
                $runtime.fail();
              })();
              if (
                (() => {
                  if (v2$1.tag === "Nothing") { return isCellTransient(v2.i, v2.j); }
                  if (v2$1.tag === "Just") { return isCellTransient(v2.i, v2$1._1) !== isCellTransient(v2.i, column_visiblePred(v2$1._1)); }
                  $runtime.fail();
                })()
              ) {
                return "1px solid blue";
              }
              if ($7) { return "1px solid #eee"; }
              return "";
            })()
          ),
          Data$dTuple.$Tuple(
            "border-bottom",
            (() => {
              const v2$1 = row_visibleSucc(v2.i);
              const $6 = v2.i === ($1.length - 1 | 0);
              if (
                (() => {
                  if (v2$1.tag === "Nothing") { return isCellTransient(v2.i, v2.j); }
                  if (v2$1.tag === "Just") { return isCellTransient(v2$1._1, v2.j) !== isCellTransient(row_visiblePred(v2$1._1), v2.j) && v2.i === (v2$1._1 - 1 | 0); }
                  $runtime.fail();
                })()
              ) {
                return "1px solid blue";
              }
              if ($6) { return "1px solid transparent"; }
              return "";
            })()
          )
        ]))();
      }
      const rows$p = $3();
      const a$p = $$for(rows$p)(row => {
        const $6 = App$dView$dUtil$dD3.datum(row);
        return () => {
          const v2 = $6();
          return Data$dTuple.$Tuple(row, Util.unsafeArrayArray.unsafeIndex(row_isVisible)(v2.i));
        };
      })();
      const v2 = Data$dArray.partitionImpl(Data$dTuple.snd, a$p);
      for (const v3 of v2.no) {
        App$dView$dUtil$dD3.classed("hidden")(true)(v3._1)();
      }
      for (const v3 of v2.yes) {
        App$dView$dUtil$dD3.classed("hidden")(false)(v3._1)();
      }
      const cells$1 = $4();
      for (const cell of cells$1) {
        const v2$1 = App$dView$dUtil$dD3.datum(cell)();
        if (Data$dArray.elem(Data$dEq.eqInt)(v2$1.j)(hiddenColumns)) {
          App$dView$dUtil$dD3.classed("hidden")(true)(cell)();
        } else {
          App$dView$dUtil$dD3.classed("hidden")(false)(cell)();
        }
      }
      const $6 = App$dView$dUtil$dD3.select(".table-caption")(rootElement)();
      App$dView$dUtil$dD3.setText($2 + " (" + Data$dShow.showIntImpl($1.length - v2.no.length | 0) + " of " + Data$dShow.showIntImpl($1.length) + " × " + Data$dShow.showIntImpl($0.length - hiddenColumns.length | 0) + " of " + Data$dShow.showIntImpl($0.length) + ")")($6)();
    };
  },
  createElement: v => v1 => parent => {
    const $0 = v1.colNames;
    const $1 = v1.rowFilter;
    const $2 = v1.rows;
    const $3 = App$dView$dUtil$dD3.createChild(parent)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.Div))(App$dView$dUtil$dD3.fromFoldable([
      App$dUtil.classes(["table-wrapper"])
    ]));
    return () => {
      const rootElement = $3();
      App$dView$dUtil$dD3.createChild(rootElement)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.Div))(App$dView$dUtil$dD3.fromFoldable([
        App$dUtil.classes(["title-text", "table-caption"]),
        Data$dTuple.$Tuple("dominant-baseline", "middle"),
        Data$dTuple.$Tuple("text-anchor", "left")
      ]))();
      const table = App$dView$dUtil$dD3.createChild(rootElement)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.Table))(App$dView$dUtil$dD3.fromFoldable([
        App$dUtil.classes(["table-view"])
      ]))();
      const colNames$p = ["__n", ...$0];
      const $4 = App$dView$dUtil$dD3.createChild(table)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.THead))(App$dView$dUtil$dD3.fromFoldable([]))();
      const row = App$dView$dUtil$dD3.createChild($4)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.TR))(App$dView$dUtil$dD3.fromFoldable([]))();
      forWithIndex_(colNames$p)(j => colName => {
        const value = (() => {
          if (colName === "__n") {
            if ($1 === "Relevant") { return "▸"; }
            return "▾";
          }
          return colName;
        })();
        const $5 = App$dView$dUtil$dD3.createChild(row)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.TH))(App$dView$dUtil$dD3.fromFoldable([
          App$dUtil.classes(["table-cell", ...colName === "__n" ? ["filter-toggle", "toggle-button"] : []])
        ]));
        const $6 = App$dView$dUtil$dD3.setText(value);
        const $7 = App$dView$dUtil$dD3.setDatum({i: -1, j: j - 1 | 0, value, colName: Util.unsafeArrayArray.unsafeIndex(colNames$p)(j)});
        return () => {
          const $8 = $5();
          const $9 = App$dView$dUtil$dD3.styles($8)(App$dView$dUtil$dD3.fromFoldable([Data$dTuple.$Tuple("border-left", "1px solid #eee")]))();
          const $10 = $6($9)();
          return $7($10)();
        };
      })();
      const body = App$dView$dUtil$dD3.createChild(table)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.TBody))(App$dView$dUtil$dD3.fromFoldable([]))();
      forWithIndex_($2)(i => row$1 => {
        const $5 = App$dView$dUtil$dD3.createChild(body)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.TR))(App$dView$dUtil$dD3.fromFoldable([
          App$dUtil.classes(["table-row"])
        ]));
        const $6 = App$dView$dUtil$dD3.setDatum({i});
        return () => {
          const $7 = $5();
          const row$p = $6($7)();
          return forWithIndex_([Data$dShow.showIntImpl(i + 1 | 0), ...Data$dFunctor.arrayMap(prim)(row$1)])(j => value => {
            const $8 = App$dView$dUtil$dD3.createChild(row$p)(App$dView$dUtil$dD3.showElementType.show(App$dView$dUtil$dD3.TD))(App$dView$dUtil$dD3.fromFoldable([
              App$dUtil.classes(j >= 0 ? ["table-cell"] : [])
            ]));
            const $9 = App$dView$dUtil$dD3.setText(value);
            const $10 = App$dView$dUtil$dD3.setDatum({i, j: j - 1 | 0, value, colName: Util.unsafeArrayArray.unsafeIndex(colNames$p)(j)});
            return () => {
              const $11 = $8();
              const $12 = App$dView$dUtil$dD3.styles($11)(App$dView$dUtil$dD3.fromFoldable([
                Data$dTuple.$Tuple("border-top", "1px solid transparent"),
                Data$dTuple.$Tuple("border-left", "1px solid #eee")
              ]))();
              const $13 = $9($12)();
              return $10($13)();
            };
          })();
        };
      })();
      return rootElement;
    };
  }
};
const arrayDictToArray2 = x => Data$dFunctor.arrayMap(a => Data$dFunctor.arrayMap(a$1 => Util$dMap.get(Data$dShow.showString)(Dict.mapDictString)(a$1)(a))(x));
export {
  
  
  
  
  arrayDictToArray2,
  
  
  
  headers,
  
  
  
  
  
  viewableTableViewUnit,
  
};
