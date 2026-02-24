// | A data structure and functions for graphs
import * as $runtime from "../runtime.js";
import * as Data$dCatList from "../Data.CatList/index.js";
import * as Data$dCatQueue from "../Data.CatQueue/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMap$dInternal from "../Data.Map.Internal/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const $SortStep = (tag, _1) => ({tag, _1});
const fromFoldable = /* #__PURE__ */ (() => {
  const foldMap1 = Data$dList$dTypes.foldableList.foldMap(Data$dCatList.monoidCatList);
  return f => foldMap1(Data$dCatList.singleton)(f);
})();
const fromFoldable1 = /* #__PURE__ */ (() => {
  const foldMap1 = Data$dFoldable.foldableArray.foldMap(Data$dCatList.monoidCatList);
  return f => foldMap1(Data$dCatList.singleton)(f);
})();
const identity = x => x;
const Emit = value0 => $SortStep("Emit", value0);
const Visit = value0 => $SortStep("Visit", value0);
const Graph = x => x;
const vertices = v => Data$dList$dTypes.listMap(Data$dTuple.fst)((() => {
  const go = (m$p, z$p) => {
    if (m$p.tag === "Leaf") { return z$p; }
    if (m$p.tag === "Node") { return go(m$p._5, Data$dList$dTypes.$List("Cons", m$p._4, go(m$p._6, z$p))); }
    $runtime.fail();
  };
  return go(v, Data$dList$dTypes.Nil);
})());
const unfoldGraph = dictOrd => dictFunctor => dictFoldable => {
  const fromFoldable3 = Data$dMap$dInternal.fromFoldable(dictOrd)(dictFoldable);
  return dictFoldable1 => {
    const fromFoldable4 = dictFoldable1.foldr(Data$dList$dTypes.Cons)(Data$dList$dTypes.Nil);
    return ks => label => theEdges => fromFoldable3(dictFunctor.map(k => Data$dTuple.$Tuple(k, Data$dTuple.$Tuple(label(k), fromFoldable4(theEdges(k)))))(ks));
  };
};
const topologicalSort = dictOrd => v => {
  const visit = visit$a0$copy => visit$a1$copy => {
    let visit$a0 = visit$a0$copy, visit$a1 = visit$a1$copy, visit$c = true, visit$r;
    while (visit$c) {
      const state = visit$a0, stack = visit$a1;
      const v1 = Data$dCatList.uncons(stack);
      if (v1.tag === "Nothing") {
        visit$c = false;
        visit$r = state;
        continue;
      }
      if (v1.tag === "Just") {
        if (v1._1._1.tag === "Emit") {
          visit$a0 = {result: Data$dList$dTypes.$List("Cons", v1._1._1._1, state.result), unvisited: state.unvisited};
          visit$a1 = v1._1._2;
          continue;
        }
        if (v1._1._1.tag === "Visit") {
          if (
            (() => {
              const $0 = v1._1._1._1;
              const go = go$a0$copy => {
                let go$a0 = go$a0$copy, go$c = true, go$r;
                while (go$c) {
                  const v$1 = go$a0;
                  if (v$1.tag === "Leaf") {
                    go$c = false;
                    go$r = false;
                    continue;
                  }
                  if (v$1.tag === "Node") {
                    const v1$1 = dictOrd.compare($0)(v$1._3);
                    if (v1$1 === "LT") {
                      go$a0 = v$1._5;
                      continue;
                    }
                    if (v1$1 === "GT") {
                      go$a0 = v$1._6;
                      continue;
                    }
                    if (v1$1 === "EQ") {
                      go$c = false;
                      go$r = true;
                      continue;
                    }
                  }
                  $runtime.fail();
                }
                return go$r;
              };
              return go(state.unvisited);
            })()
          ) {
            const $0 = v1._1._1._1;
            visit$a0 = {result: state.result, unvisited: Data$dMap$dInternal.delete(dictOrd)($0)(state.unvisited)};
            visit$a1 = (() => {
              const $1 = fromFoldable(Data$dList$dTypes.listMap(Visit)((() => {
                const go = go$a0$copy => {
                  let go$a0 = go$a0$copy, go$c = true, go$r;
                  while (go$c) {
                    const v$1 = go$a0;
                    if (v$1.tag === "Leaf") {
                      go$c = false;
                      go$r = Data$dMaybe.Nothing;
                      continue;
                    }
                    if (v$1.tag === "Node") {
                      const v1$1 = dictOrd.compare($0)(v$1._3);
                      if (v1$1 === "LT") {
                        go$a0 = v$1._5;
                        continue;
                      }
                      if (v1$1 === "GT") {
                        go$a0 = v$1._6;
                        continue;
                      }
                      if (v1$1 === "EQ") {
                        go$c = false;
                        go$r = Data$dMaybe.$Maybe("Just", v$1._4);
                        continue;
                      }
                    }
                    $runtime.fail();
                  }
                  return go$r;
                };
                const $1 = go(v);
                if ($1.tag === "Nothing") { return Data$dList$dTypes.Nil; }
                if ($1.tag === "Just") { return $1._1._2; }
                $runtime.fail();
              })()));
              const $2 = v1._1._2.tag === "CatNil"
                ? Data$dCatList.$CatList("CatCons", $SortStep("Emit", $0), Data$dCatQueue.$CatQueue(Data$dList$dTypes.Nil, Data$dList$dTypes.Nil))
                : Data$dCatList.$CatList(
                    "CatCons",
                    $SortStep("Emit", $0),
                    Data$dCatQueue.$CatQueue(Data$dList$dTypes.Nil, Data$dList$dTypes.$List("Cons", v1._1._2, Data$dList$dTypes.Nil))
                  );
              if ($1.tag === "CatNil") { return $2; }
              if ($2.tag === "CatNil") { return $1; }
              if ($1.tag === "CatCons") { return Data$dCatList.$CatList("CatCons", $1._1, Data$dCatQueue.$CatQueue($1._2._1, Data$dList$dTypes.$List("Cons", $2, $1._2._2))); }
              $runtime.fail();
            })();
            continue;
          }
          visit$a0 = state;
          visit$a1 = v1._1._2;
          continue;
        }
      }
      $runtime.fail();
    }
    return visit$r;
  };
  const go = go$a0$copy => {
    let go$a0 = go$a0$copy, go$c = true, go$r;
    while (go$c) {
      const v1 = go$a0;
      const v2 = Data$dMap$dInternal.findMin(v1.unvisited);
      if (v2.tag === "Just") {
        go$a0 = visit(v1)(fromFoldable1([$SortStep("Visit", v2._1.key)]));
        continue;
      }
      if (v2.tag === "Nothing") {
        go$c = false;
        go$r = v1.result;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go({unvisited: v, result: Data$dList$dTypes.Nil});
};
const toMap = v => v;
const outEdges = dictOrd => k => v => {
  const go = go$a0$copy => {
    let go$a0 = go$a0$copy, go$c = true, go$r;
    while (go$c) {
      const v$1 = go$a0;
      if (v$1.tag === "Leaf") {
        go$c = false;
        go$r = Data$dMaybe.Nothing;
        continue;
      }
      if (v$1.tag === "Node") {
        const v1 = dictOrd.compare(k)(v$1._3);
        if (v1 === "LT") {
          go$a0 = v$1._5;
          continue;
        }
        if (v1 === "GT") {
          go$a0 = v$1._6;
          continue;
        }
        if (v1 === "EQ") {
          go$c = false;
          go$r = Data$dMaybe.$Maybe("Just", v$1._4);
          continue;
        }
      }
      $runtime.fail();
    }
    return go$r;
  };
  const $0 = go(v);
  if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", $0._1._2); }
  return Data$dMaybe.Nothing;
};
const lookup = dictOrd => k => v => {
  const go = go$a0$copy => {
    let go$a0 = go$a0$copy, go$c = true, go$r;
    while (go$c) {
      const v$1 = go$a0;
      if (v$1.tag === "Leaf") {
        go$c = false;
        go$r = Data$dMaybe.Nothing;
        continue;
      }
      if (v$1.tag === "Node") {
        const v1 = dictOrd.compare(k)(v$1._3);
        if (v1 === "LT") {
          go$a0 = v$1._5;
          continue;
        }
        if (v1 === "GT") {
          go$a0 = v$1._6;
          continue;
        }
        if (v1 === "EQ") {
          go$c = false;
          go$r = Data$dMaybe.$Maybe("Just", v$1._4);
          continue;
        }
      }
      $runtime.fail();
    }
    return go$r;
  };
  const $0 = go(v);
  if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", $0._1._1); }
  return Data$dMaybe.Nothing;
};
const functorGraph = {
  map: f => v => {
    const go = v$1 => {
      if (v$1.tag === "Leaf") { return Data$dMap$dInternal.Leaf; }
      if (v$1.tag === "Node") { return Data$dMap$dInternal.$$$Map("Node", v$1._1, v$1._2, v$1._3, Data$dTuple.$Tuple(f(v$1._4._1), v$1._4._2), go(v$1._5), go(v$1._6)); }
      $runtime.fail();
    };
    return go(v);
  }
};
const fromMap = Graph;
const foldableGraph = {
  foldl: f => z => v => {
    const go = go$a0$copy => go$a1$copy => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const b = go$a0, v$1 = go$a1;
        if (v$1.tag === "Nil") {
          go$c = false;
          go$r = b;
          continue;
        }
        if (v$1.tag === "Cons") {
          go$a0 = f(b)(v$1._1._1);
          go$a1 = v$1._2;
          continue;
        }
        $runtime.fail();
      }
      return go$r;
    };
    return go(z)((() => {
      const go$1 = (m$p, z$p) => {
        if (m$p.tag === "Leaf") { return z$p; }
        if (m$p.tag === "Node") { return go$1(m$p._5, Data$dList$dTypes.$List("Cons", m$p._4, go$1(m$p._6, z$p))); }
        $runtime.fail();
      };
      return go$1(v, Data$dList$dTypes.Nil);
    })());
  },
  foldr: f => z => v => Data$dList$dTypes.foldableList.foldr(v1 => {
    const $0 = v1._1;
    return acc => f($0)(acc);
  })(z)((() => {
    const go = (m$p, z$p) => {
      if (m$p.tag === "Leaf") { return z$p; }
      if (m$p.tag === "Node") { return go(m$p._5, Data$dList$dTypes.$List("Cons", m$p._4, go(m$p._6, z$p))); }
      $runtime.fail();
    };
    return go(v, Data$dList$dTypes.Nil);
  })()),
  foldMap: dictMonoid => {
    const foldMap1 = Data$dList$dTypes.foldableList.foldMap(dictMonoid);
    return f => v => foldMap1(x => f(x._1))((() => {
      const go = (m$p, z$p) => {
        if (m$p.tag === "Leaf") { return z$p; }
        if (m$p.tag === "Node") { return go(m$p._5, Data$dList$dTypes.$List("Cons", m$p._4, go(m$p._6, z$p))); }
        $runtime.fail();
      };
      return go(v, Data$dList$dTypes.Nil);
    })());
  }
};
const traversableGraph = {
  traverse: dictApplicative => {
    const $0 = dictApplicative.Apply0().Functor0();
    const traverse1 = Data$dMap$dInternal.traversableMap.traverse(dictApplicative);
    return f => v => $0.map(Graph)(traverse1(v1 => {
      const $1 = v1._2;
      return $0.map(v3 => Data$dTuple.$Tuple(v3, $1))(f(v1._1));
    })(v));
  },
  sequence: dictApplicative => traversableGraph.traverse(dictApplicative)(identity),
  Functor0: () => functorGraph,
  Foldable1: () => foldableGraph
};
const edges = v => {
  const go = (z$p, m$p) => {
    if (m$p.tag === "Leaf") { return z$p; }
    if (m$p.tag === "Node") {
      return go(
        (() => {
          const $0 = m$p._3;
          const go$1 = go$1$a0$copy => go$1$a1$copy => {
            let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
            while (go$1$c) {
              const b = go$1$a0, v$1 = go$1$a1;
              if (v$1.tag === "Nil") {
                go$1$c = false;
                go$1$r = b;
                continue;
              }
              if (v$1.tag === "Cons") {
                go$1$a0 = Data$dList$dTypes.$List("Cons", {start: $0, end: v$1._1}, b);
                go$1$a1 = v$1._2;
                continue;
              }
              $runtime.fail();
            }
            return go$1$r;
          };
          return go$1(go(z$p, m$p._5))(m$p._4._2);
        })(),
        m$p._6
      );
    }
    $runtime.fail();
  };
  return go(Data$dList$dTypes.Nil, v);
};
export {
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  topologicalSort,
  
  
  
};
