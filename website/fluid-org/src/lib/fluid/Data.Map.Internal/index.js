// | This module defines a type of maps as height-balanced (AVL) binary trees.
// | Efficient set operations are implemented in terms of
// | <https://www.cs.cmu.edu/~guyb/papers/BFS16.pdf>
import * as $runtime from "../runtime.js";
import * as Data$dFunction from "../Data.Function/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Data$dUnfoldable from "../Data.Unfoldable/index.js";
const $$$Map = (tag, _1, _2, _3, _4, _5, _6) => ({tag, _1, _2, _3, _4, _5, _6});
const $MapIter = (tag, _1, _2, _3) => ({tag, _1, _2, _3});
const $MapIterStep = (tag, _1, _2, _3) => ({tag, _1, _2, _3});
const $Split = (_1, _2, _3) => ({tag: "Split", _1, _2, _3});
const $SplitLast = (_1, _2, _3) => ({tag: "SplitLast", _1, _2, _3});
const identity = x => x;
const Leaf = /* #__PURE__ */ $$$Map("Leaf");
const Node = value0 => value1 => value2 => value3 => value4 => value5 => $$$Map("Node", value0, value1, value2, value3, value4, value5);
const IterLeaf = /* #__PURE__ */ $MapIter("IterLeaf");
const IterEmit = value0 => value1 => value2 => $MapIter("IterEmit", value0, value1, value2);
const IterNode = value0 => value1 => $MapIter("IterNode", value0, value1);
const IterDone = /* #__PURE__ */ $MapIterStep("IterDone");
const IterNext = value0 => value1 => value2 => $MapIterStep("IterNext", value0, value1, value2);
const Split = value0 => value1 => value2 => $Split(value0, value1, value2);
const SplitLast = value0 => value1 => value2 => $SplitLast(value0, value1, value2);
const unsafeNode = (k, v, l, r) => {
  if (l.tag === "Leaf") {
    if (r.tag === "Leaf") { return $$$Map("Node", 1, 1, k, v, l, r); }
    if (r.tag === "Node") { return $$$Map("Node", 1 + r._1 | 0, 1 + r._2 | 0, k, v, l, r); }
    $runtime.fail();
  }
  if (l.tag === "Node") {
    if (r.tag === "Leaf") { return $$$Map("Node", 1 + l._1 | 0, 1 + l._2 | 0, k, v, l, r); }
    if (r.tag === "Node") { return $$$Map("Node", l._1 > r._1 ? 1 + l._1 | 0 : 1 + r._1 | 0, (1 + l._2 | 0) + r._2 | 0, k, v, l, r); }
  }
  $runtime.fail();
};
const toMapIter = a => $MapIter("IterNode", a, IterLeaf);
const size = v => {
  if (v.tag === "Leaf") { return 0; }
  if (v.tag === "Node") { return v._2; }
  $runtime.fail();
};
const singleton = k => v => $$$Map("Node", 1, 1, k, v, Leaf, Leaf);
const unsafeBalancedNode = (k, v, l, r) => {
  if (l.tag === "Leaf") {
    if (r.tag === "Leaf") { return $$$Map("Node", 1, 1, k, v, Leaf, Leaf); }
    if (r.tag === "Node" && r._1 > 1) {
      if (
        r._5.tag === "Node" && (() => {
          if (r._6.tag === "Leaf") { return r._5._1 > 0; }
          if (r._6.tag === "Node") { return r._5._1 > r._6._1; }
          $runtime.fail();
        })()
      ) {
        return unsafeNode(r._5._3, r._5._4, unsafeNode(k, v, l, r._5._5), unsafeNode(r._3, r._4, r._5._6, r._6));
      }
      return unsafeNode(r._3, r._4, unsafeNode(k, v, l, r._5), r._6);
    }
    return unsafeNode(k, v, l, r);
  }
  if (l.tag === "Node") {
    if (r.tag === "Node") {
      if (r._1 > (l._1 + 1 | 0)) {
        if (
          r._5.tag === "Node" && (() => {
            if (r._6.tag === "Leaf") { return r._5._1 > 0; }
            if (r._6.tag === "Node") { return r._5._1 > r._6._1; }
            $runtime.fail();
          })()
        ) {
          return unsafeNode(r._5._3, r._5._4, unsafeNode(k, v, l, r._5._5), unsafeNode(r._3, r._4, r._5._6, r._6));
        }
        return unsafeNode(r._3, r._4, unsafeNode(k, v, l, r._5), r._6);
      }
      if (l._1 > (r._1 + 1 | 0)) {
        if (
          l._6.tag === "Node" && (() => {
            if (l._5.tag === "Leaf") { return 0 <= l._6._1; }
            if (l._5.tag === "Node") { return l._5._1 <= l._6._1; }
            $runtime.fail();
          })()
        ) {
          return unsafeNode(l._6._3, l._6._4, unsafeNode(l._3, l._4, l._5, l._6._5), unsafeNode(k, v, l._6._6, r));
        }
        return unsafeNode(l._3, l._4, l._5, unsafeNode(k, v, l._6, r));
      }
      return unsafeNode(k, v, l, r);
    }
    if (r.tag === "Leaf" && l._1 > 1) {
      if (
        l._6.tag === "Node" && (() => {
          if (l._5.tag === "Leaf") { return 0 <= l._6._1; }
          if (l._5.tag === "Node") { return l._5._1 <= l._6._1; }
          $runtime.fail();
        })()
      ) {
        return unsafeNode(l._6._3, l._6._4, unsafeNode(l._3, l._4, l._5, l._6._5), unsafeNode(k, v, l._6._6, r));
      }
      return unsafeNode(l._3, l._4, l._5, unsafeNode(k, v, l._6, r));
    }
    return unsafeNode(k, v, l, r);
  }
  $runtime.fail();
};
const unsafeSplit = (comp, k, m) => {
  if (m.tag === "Leaf") { return $Split(Data$dMaybe.Nothing, Leaf, Leaf); }
  if (m.tag === "Node") {
    const v = comp(k)(m._3);
    if (v === "LT") {
      const v1 = unsafeSplit(comp, k, m._5);
      return $Split(v1._1, v1._2, unsafeBalancedNode(m._3, m._4, v1._3, m._6));
    }
    if (v === "GT") {
      const v1 = unsafeSplit(comp, k, m._6);
      return $Split(v1._1, unsafeBalancedNode(m._3, m._4, m._5, v1._2), v1._3);
    }
    if (v === "EQ") { return $Split(Data$dMaybe.$Maybe("Just", m._4), m._5, m._6); }
  }
  $runtime.fail();
};
const unsafeSplitLast = (k, v, l, r) => {
  if (r.tag === "Leaf") { return $SplitLast(k, v, l); }
  if (r.tag === "Node") {
    const v1 = unsafeSplitLast(r._3, r._4, r._5, r._6);
    return $SplitLast(v1._1, v1._2, unsafeBalancedNode(k, v, l, v1._3));
  }
  $runtime.fail();
};
const unsafeJoinNodes = (v, v1) => {
  if (v.tag === "Leaf") { return v1; }
  if (v.tag === "Node") {
    const v2 = unsafeSplitLast(v._3, v._4, v._5, v._6);
    return unsafeBalancedNode(v2._1, v2._2, v2._3, v1);
  }
  $runtime.fail();
};
const unsafeDifference = (comp, l, r) => {
  if (l.tag === "Leaf") { return Leaf; }
  if (r.tag === "Leaf") { return l; }
  if (r.tag === "Node") {
    const v = unsafeSplit(comp, r._3, l);
    return unsafeJoinNodes(unsafeDifference(comp, v._2, r._5), unsafeDifference(comp, v._3, r._6));
  }
  $runtime.fail();
};
const unsafeIntersectionWith = (comp, app, l, r) => {
  if (l.tag === "Leaf") { return Leaf; }
  if (r.tag === "Leaf") { return Leaf; }
  if (r.tag === "Node") {
    const v = unsafeSplit(comp, r._3, l);
    const l$p = unsafeIntersectionWith(comp, app, v._2, r._5);
    const r$p = unsafeIntersectionWith(comp, app, v._3, r._6);
    if (v._1.tag === "Just") { return unsafeBalancedNode(r._3, app(v._1._1)(r._4), l$p, r$p); }
    if (v._1.tag === "Nothing") { return unsafeJoinNodes(l$p, r$p); }
  }
  $runtime.fail();
};
const unsafeUnionWith = (comp, app, l, r) => {
  if (l.tag === "Leaf") { return r; }
  if (r.tag === "Leaf") { return l; }
  if (r.tag === "Node") {
    const v = unsafeSplit(comp, r._3, l);
    const l$p = unsafeUnionWith(comp, app, v._2, r._5);
    const r$p = unsafeUnionWith(comp, app, v._3, r._6);
    if (v._1.tag === "Just") { return unsafeBalancedNode(r._3, app(v._1._1)(r._4), l$p, r$p); }
    if (v._1.tag === "Nothing") { return unsafeBalancedNode(r._3, r._4, l$p, r$p); }
  }
  $runtime.fail();
};
const unionWith = dictOrd => {
  const compare = dictOrd.compare;
  return app => m1 => m2 => unsafeUnionWith(compare, app, m1, m2);
};
const union = dictOrd => {
  const compare = dictOrd.compare;
  return m1 => m2 => unsafeUnionWith(compare, Data$dFunction.const, m1, m2);
};
const update = dictOrd => f => k => {
  const go = v => {
    if (v.tag === "Leaf") { return Leaf; }
    if (v.tag === "Node") {
      const v1 = dictOrd.compare(k)(v._3);
      if (v1 === "LT") { return unsafeBalancedNode(v._3, v._4, go(v._5), v._6); }
      if (v1 === "GT") { return unsafeBalancedNode(v._3, v._4, v._5, go(v._6)); }
      if (v1 === "EQ") {
        const v2 = f(v._4);
        if (v2.tag === "Nothing") { return unsafeJoinNodes(v._5, v._6); }
        if (v2.tag === "Just") { return $$$Map("Node", v._1, v._2, v._3, v2._1, v._5, v._6); }
      }
    }
    $runtime.fail();
  };
  return go;
};
const showTree = dictShow => dictShow1 => {
  const go = ind => v => {
    if (v.tag === "Leaf") { return ind + "Leaf"; }
    if (v.tag === "Node") {
      return ind + "[" + Data$dShow.showIntImpl(v._1) + "] " + dictShow.show(v._3) + " => " + dictShow1.show(v._4) + "\n" + go(ind + "    ")(v._5) + "\n" + go(ind + "    ")(v._6);
    }
    $runtime.fail();
  };
  return go("");
};
const semigroupMap = () => dictOrd => {
  const compare = dictOrd.compare;
  return dictSemigroup => (
    {
      append: (() => {
        const $0 = dictSemigroup.append;
        return m1 => m2 => unsafeUnionWith(compare, $0, m1, m2);
      })()
    }
  );
};
const pop = dictOrd => {
  const compare = dictOrd.compare;
  return k => m => {
    const v = unsafeSplit(compare, k, m);
    if (v._1.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple(v._1._1, unsafeJoinNodes(v._2, v._3))); }
    return Data$dMaybe.Nothing;
  };
};
const member = dictOrd => k => {
  const go = go$a0$copy => {
    let go$a0 = go$a0$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0;
      if (v.tag === "Leaf") {
        go$c = false;
        go$r = false;
        continue;
      }
      if (v.tag === "Node") {
        const v1 = dictOrd.compare(k)(v._3);
        if (v1 === "LT") {
          go$a0 = v._5;
          continue;
        }
        if (v1 === "GT") {
          go$a0 = v._6;
          continue;
        }
        if (v1 === "EQ") {
          go$c = false;
          go$r = true;
          continue;
        }
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go;
};
const mapMaybeWithKey = dictOrd => f => {
  const go = v => {
    if (v.tag === "Leaf") { return Leaf; }
    if (v.tag === "Node") {
      const v2 = f(v._3)(v._4);
      if (v2.tag === "Just") { return unsafeBalancedNode(v._3, v2._1, go(v._5), go(v._6)); }
      if (v2.tag === "Nothing") { return unsafeJoinNodes(go(v._5), go(v._6)); }
    }
    $runtime.fail();
  };
  return go;
};
const mapMaybe = dictOrd => x => mapMaybeWithKey(dictOrd)(v => x);
const lookupLE = dictOrd => k => {
  const go = v => {
    if (v.tag === "Leaf") { return Data$dMaybe.Nothing; }
    if (v.tag === "Node") {
      const v1 = dictOrd.compare(k)(v._3);
      if (v1 === "LT") { return go(v._5); }
      if (v1 === "GT") {
        const v2 = go(v._6);
        if (v2.tag === "Nothing") { return Data$dMaybe.$Maybe("Just", {key: v._3, value: v._4}); }
        return v2;
      }
      if (v1 === "EQ") { return Data$dMaybe.$Maybe("Just", {key: v._3, value: v._4}); }
    }
    $runtime.fail();
  };
  return go;
};
const lookupGE = dictOrd => k => {
  const go = v => {
    if (v.tag === "Leaf") { return Data$dMaybe.Nothing; }
    if (v.tag === "Node") {
      const v1 = dictOrd.compare(k)(v._3);
      if (v1 === "LT") {
        const v2 = go(v._5);
        if (v2.tag === "Nothing") { return Data$dMaybe.$Maybe("Just", {key: v._3, value: v._4}); }
        return v2;
      }
      if (v1 === "GT") { return go(v._6); }
      if (v1 === "EQ") { return Data$dMaybe.$Maybe("Just", {key: v._3, value: v._4}); }
    }
    $runtime.fail();
  };
  return go;
};
const lookup = dictOrd => k => {
  const go = go$a0$copy => {
    let go$a0 = go$a0$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0;
      if (v.tag === "Leaf") {
        go$c = false;
        go$r = Data$dMaybe.Nothing;
        continue;
      }
      if (v.tag === "Node") {
        const v1 = dictOrd.compare(k)(v._3);
        if (v1 === "LT") {
          go$a0 = v._5;
          continue;
        }
        if (v1 === "GT") {
          go$a0 = v._6;
          continue;
        }
        if (v1 === "EQ") {
          go$c = false;
          go$r = Data$dMaybe.$Maybe("Just", v._4);
          continue;
        }
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go;
};
const stepUnorderedCps = next => done => {
  const go = go$a0$copy => {
    let go$a0 = go$a0$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0;
      if (v.tag === "IterLeaf") {
        go$c = false;
        go$r = done();
        continue;
      }
      if (v.tag === "IterEmit") {
        go$c = false;
        go$r = next(v._1, v._2, v._3);
        continue;
      }
      if (v.tag === "IterNode") {
        go$a0 = (() => {
          if (v._1.tag === "Leaf") { return v._2; }
          if (v._1.tag === "Node") {
            if (v._1._5.tag === "Leaf") {
              if (v._1._6.tag === "Leaf") { return $MapIter("IterEmit", v._1._3, v._1._4, v._2); }
              return $MapIter("IterEmit", v._1._3, v._1._4, $MapIter("IterNode", v._1._6, v._2));
            }
            if (v._1._6.tag === "Leaf") { return $MapIter("IterEmit", v._1._3, v._1._4, $MapIter("IterNode", v._1._5, v._2)); }
            return $MapIter("IterEmit", v._1._3, v._1._4, $MapIter("IterNode", v._1._5, $MapIter("IterNode", v._1._6, v._2)));
          }
          $runtime.fail();
        })();
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go;
};
const stepUnfoldrUnordered = /* #__PURE__ */ stepUnorderedCps((k, v, next) => Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple(Data$dTuple.$Tuple(k, v), next)))(v => Data$dMaybe.Nothing);
const toUnfoldableUnordered = dictUnfoldable => {
  const $0 = dictUnfoldable.unfoldr(stepUnfoldrUnordered);
  return x => $0($MapIter("IterNode", x, IterLeaf));
};
const stepUnordered = /* #__PURE__ */ stepUnorderedCps((k, v, next) => $MapIterStep("IterNext", k, v, next))(v => IterDone);
const stepDescCps = next => done => {
  const go = go$a0$copy => {
    let go$a0 = go$a0$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0;
      if (v.tag === "IterLeaf") {
        go$c = false;
        go$r = done();
        continue;
      }
      if (v.tag === "IterEmit") {
        go$c = false;
        go$r = next(v._1, v._2, v._3);
        continue;
      }
      if (v.tag === "IterNode") {
        go$a0 = (() => {
          const go$1 = go$1$a0$copy => go$1$a1$copy => {
            let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
            while (go$1$c) {
              const iter = go$1$a0, v$1 = go$1$a1;
              if (v$1.tag === "Leaf") {
                go$1$c = false;
                go$1$r = iter;
                continue;
              }
              if (v$1.tag === "Node") {
                if (v$1._6.tag === "Leaf") {
                  go$1$a0 = $MapIter("IterEmit", v$1._3, v$1._4, iter);
                  go$1$a1 = v$1._5;
                  continue;
                }
                go$1$a0 = $MapIter("IterEmit", v$1._3, v$1._4, $MapIter("IterNode", v$1._5, iter));
                go$1$a1 = v$1._6;
                continue;
              }
              $runtime.fail();
            }
            return go$1$r;
          };
          return go$1(v._2)(v._1);
        })();
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go;
};
const stepDesc = /* #__PURE__ */ stepDescCps((k, v, next) => $MapIterStep("IterNext", k, v, next))(v => IterDone);
const stepAscCps = next => done => {
  const go = go$a0$copy => {
    let go$a0 = go$a0$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0;
      if (v.tag === "IterLeaf") {
        go$c = false;
        go$r = done();
        continue;
      }
      if (v.tag === "IterEmit") {
        go$c = false;
        go$r = next(v._1, v._2, v._3);
        continue;
      }
      if (v.tag === "IterNode") {
        go$a0 = (() => {
          const go$1 = go$1$a0$copy => go$1$a1$copy => {
            let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
            while (go$1$c) {
              const iter = go$1$a0, v$1 = go$1$a1;
              if (v$1.tag === "Leaf") {
                go$1$c = false;
                go$1$r = iter;
                continue;
              }
              if (v$1.tag === "Node") {
                if (v$1._6.tag === "Leaf") {
                  go$1$a0 = $MapIter("IterEmit", v$1._3, v$1._4, iter);
                  go$1$a1 = v$1._5;
                  continue;
                }
                go$1$a0 = $MapIter("IterEmit", v$1._3, v$1._4, $MapIter("IterNode", v$1._6, iter));
                go$1$a1 = v$1._5;
                continue;
              }
              $runtime.fail();
            }
            return go$1$r;
          };
          return go$1(v._2)(v._1);
        })();
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go;
};
const stepAsc = /* #__PURE__ */ stepAscCps((k, v, next) => $MapIterStep("IterNext", k, v, next))(v => IterDone);
const eqMapIter = dictEq => dictEq1 => (
  {
    eq: (() => {
      const go = a => b => {
        const v = stepAsc(a);
        if (v.tag === "IterNext") {
          const v2 = stepAsc(b);
          return v2.tag === "IterNext" && dictEq.eq(v._1)(v2._1) && dictEq1.eq(v._2)(v2._2) && go(v._3)(v2._3);
        }
        if (v.tag === "IterDone") { return true; }
        $runtime.fail();
      };
      return go;
    })()
  }
);
const ordMapIter = dictOrd => {
  const eqMapIter1 = eqMapIter(dictOrd.Eq0());
  return dictOrd1 => {
    const eqMapIter2 = eqMapIter1(dictOrd1.Eq0());
    return {
      compare: (() => {
        const go = go$a0$copy => go$a1$copy => {
          let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
          while (go$c) {
            const a = go$a0, b = go$a1;
            const v = stepAsc(b);
            const v1 = stepAsc(a);
            if (v1.tag === "IterNext") {
              if (v.tag === "IterNext") {
                const v3 = dictOrd.compare(v1._1)(v._1);
                if (v3 === "EQ") {
                  const v4 = dictOrd1.compare(v1._2)(v._2);
                  if (v4 === "EQ") {
                    go$a0 = v1._3;
                    go$a1 = v._3;
                    continue;
                  }
                  go$c = false;
                  go$r = v4;
                  continue;
                }
                go$c = false;
                go$r = v3;
                continue;
              }
              if (v.tag === "IterDone") {
                go$c = false;
                go$r = Data$dOrdering.GT;
                continue;
              }
              $runtime.fail();
            }
            if (v1.tag === "IterDone") {
              if (v.tag === "IterDone") {
                go$c = false;
                go$r = Data$dOrdering.EQ;
                continue;
              }
              go$c = false;
              go$r = Data$dOrdering.LT;
              continue;
            }
            if (v.tag === "IterDone") {
              go$c = false;
              go$r = Data$dOrdering.GT;
              continue;
            }
            $runtime.fail();
          }
          return go$r;
        };
        return go;
      })(),
      Eq0: () => eqMapIter2
    };
  };
};
const stepUnfoldr = /* #__PURE__ */ stepAscCps((k, v, next) => Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple(Data$dTuple.$Tuple(k, v), next)))(v => Data$dMaybe.Nothing);
const toUnfoldable = dictUnfoldable => {
  const $0 = dictUnfoldable.unfoldr(stepUnfoldr);
  return x => $0($MapIter("IterNode", x, IterLeaf));
};
const toUnfoldable1 = /* #__PURE__ */ (() => {
  const $0 = Data$dUnfoldable.unfoldableArray.unfoldr(stepUnfoldr);
  return x => $0($MapIter("IterNode", x, IterLeaf));
})();
const showMap = dictShow => dictShow1 => {
  const show1 = Data$dShow.showArrayImpl(v => "(Tuple " + dictShow.show(v._1) + " " + dictShow1.show(v._2) + ")");
  return {show: as => "(fromFoldable " + show1(toUnfoldable1(as)) + ")"};
};
const isSubmap = dictOrd => dictEq => {
  const go = m1 => m2 => {
    if (m1.tag === "Leaf") { return true; }
    if (m1.tag === "Node") {
      const $0 = m1._3;
      const go$1 = go$1$a0$copy => {
        let go$1$a0 = go$1$a0$copy, go$1$c = true, go$1$r;
        while (go$1$c) {
          const v = go$1$a0;
          if (v.tag === "Leaf") {
            go$1$c = false;
            go$1$r = Data$dMaybe.Nothing;
            continue;
          }
          if (v.tag === "Node") {
            const v1 = dictOrd.compare($0)(v._3);
            if (v1 === "LT") {
              go$1$a0 = v._5;
              continue;
            }
            if (v1 === "GT") {
              go$1$a0 = v._6;
              continue;
            }
            if (v1 === "EQ") {
              go$1$c = false;
              go$1$r = Data$dMaybe.$Maybe("Just", v._4);
              continue;
            }
          }
          $runtime.fail();
        }
        return go$1$r;
      };
      const v1 = go$1(m2);
      if (v1.tag === "Nothing") { return false; }
      if (v1.tag === "Just") { return dictEq.eq(m1._4)(v1._1) && go(m1._5)(m2) && go(m1._6)(m2); }
    }
    $runtime.fail();
  };
  return go;
};
const isEmpty = v => v.tag === "Leaf";
const intersectionWith = dictOrd => {
  const compare = dictOrd.compare;
  return app => m1 => m2 => unsafeIntersectionWith(compare, app, m1, m2);
};
const intersection = dictOrd => {
  const compare = dictOrd.compare;
  return m1 => m2 => unsafeIntersectionWith(compare, Data$dFunction.const, m1, m2);
};
const insertWith = dictOrd => app => k => v => {
  const go = v1 => {
    if (v1.tag === "Leaf") { return $$$Map("Node", 1, 1, k, v, Leaf, Leaf); }
    if (v1.tag === "Node") {
      const v2 = dictOrd.compare(k)(v1._3);
      if (v2 === "LT") { return unsafeBalancedNode(v1._3, v1._4, go(v1._5), v1._6); }
      if (v2 === "GT") { return unsafeBalancedNode(v1._3, v1._4, v1._5, go(v1._6)); }
      if (v2 === "EQ") { return $$$Map("Node", v1._1, v1._2, k, app(v1._4)(v), v1._5, v1._6); }
    }
    $runtime.fail();
  };
  return go;
};
const insert = dictOrd => k => v => {
  const go = v1 => {
    if (v1.tag === "Leaf") { return $$$Map("Node", 1, 1, k, v, Leaf, Leaf); }
    if (v1.tag === "Node") {
      const v2 = dictOrd.compare(k)(v1._3);
      if (v2 === "LT") { return unsafeBalancedNode(v1._3, v1._4, go(v1._5), v1._6); }
      if (v2 === "GT") { return unsafeBalancedNode(v1._3, v1._4, v1._5, go(v1._6)); }
      if (v2 === "EQ") { return $$$Map("Node", v1._1, v1._2, k, v, v1._5, v1._6); }
    }
    $runtime.fail();
  };
  return go;
};
const functorMap = {
  map: f => {
    const go = v => {
      if (v.tag === "Leaf") { return Leaf; }
      if (v.tag === "Node") { return $$$Map("Node", v._1, v._2, v._3, f(v._4), go(v._5), go(v._6)); }
      $runtime.fail();
    };
    return go;
  }
};
const functorWithIndexMap = {
  mapWithIndex: f => {
    const go = v => {
      if (v.tag === "Leaf") { return Leaf; }
      if (v.tag === "Node") { return $$$Map("Node", v._1, v._2, v._3, f(v._3)(v._4), go(v._5), go(v._6)); }
      $runtime.fail();
    };
    return go;
  },
  Functor0: () => functorMap
};
const foldableMap = {
  foldr: f => z => {
    const go = (m$p, z$p) => {
      if (m$p.tag === "Leaf") { return z$p; }
      if (m$p.tag === "Node") { return go(m$p._5, f(m$p._4)(go(m$p._6, z$p))); }
      $runtime.fail();
    };
    return m => go(m, z);
  },
  foldl: f => z => {
    const go = (z$p, m$p) => {
      if (m$p.tag === "Leaf") { return z$p; }
      if (m$p.tag === "Node") { return go(f(go(z$p, m$p._5))(m$p._4), m$p._6); }
      $runtime.fail();
    };
    return m => go(z, m);
  },
  foldMap: dictMonoid => {
    const mempty = dictMonoid.mempty;
    const $0 = dictMonoid.Semigroup0();
    return f => {
      const go = v => {
        if (v.tag === "Leaf") { return mempty; }
        if (v.tag === "Node") { return $0.append(go(v._5))($0.append(f(v._4))(go(v._6))); }
        $runtime.fail();
      };
      return go;
    };
  }
};
const foldableWithIndexMap = {
  foldrWithIndex: f => z => {
    const go = (m$p, z$p) => {
      if (m$p.tag === "Leaf") { return z$p; }
      if (m$p.tag === "Node") { return go(m$p._5, f(m$p._3)(m$p._4)(go(m$p._6, z$p))); }
      $runtime.fail();
    };
    return m => go(m, z);
  },
  foldlWithIndex: f => z => {
    const go = (z$p, m$p) => {
      if (m$p.tag === "Leaf") { return z$p; }
      if (m$p.tag === "Node") { return go(f(m$p._3)(go(z$p, m$p._5))(m$p._4), m$p._6); }
      $runtime.fail();
    };
    return m => go(z, m);
  },
  foldMapWithIndex: dictMonoid => {
    const mempty = dictMonoid.mempty;
    const $0 = dictMonoid.Semigroup0();
    return f => {
      const go = v => {
        if (v.tag === "Leaf") { return mempty; }
        if (v.tag === "Node") { return $0.append(go(v._5))($0.append(f(v._3)(v._4))(go(v._6))); }
        $runtime.fail();
      };
      return go;
    };
  },
  Foldable0: () => foldableMap
};
const keys = /* #__PURE__ */ (() => {
  const go = (m$p, z$p) => {
    if (m$p.tag === "Leaf") { return z$p; }
    if (m$p.tag === "Node") { return go(m$p._5, Data$dList$dTypes.$List("Cons", m$p._3, go(m$p._6, z$p))); }
    $runtime.fail();
  };
  return m => go(m, Data$dList$dTypes.Nil);
})();
const traversableMap = {
  traverse: dictApplicative => {
    const Apply0 = dictApplicative.Apply0();
    return f => {
      const go = v => {
        if (v.tag === "Leaf") { return dictApplicative.pure(Leaf); }
        if (v.tag === "Node") {
          const $0 = v._1;
          const $1 = v._3;
          const $2 = v._2;
          return Apply0.apply(Apply0.apply(Apply0.Functor0().map(l$p => v$p => r$p => $$$Map("Node", $0, $2, $1, v$p, l$p, r$p))(go(v._5)))(f(v._4)))(go(v._6));
        }
        $runtime.fail();
      };
      return go;
    };
  },
  sequence: dictApplicative => traversableMap.traverse(dictApplicative)(identity),
  Functor0: () => functorMap,
  Foldable1: () => foldableMap
};
const traversableWithIndexMap = {
  traverseWithIndex: dictApplicative => {
    const Apply0 = dictApplicative.Apply0();
    return f => {
      const go = v => {
        if (v.tag === "Leaf") { return dictApplicative.pure(Leaf); }
        if (v.tag === "Node") {
          const $0 = v._1;
          const $1 = v._3;
          const $2 = v._2;
          return Apply0.apply(Apply0.apply(Apply0.Functor0().map(l$p => v$p => r$p => $$$Map("Node", $0, $2, $1, v$p, l$p, r$p))(go(v._5)))(f($1)(v._4)))(go(v._6));
        }
        $runtime.fail();
      };
      return go;
    };
  },
  FunctorWithIndex0: () => functorWithIndexMap,
  FoldableWithIndex1: () => foldableWithIndexMap,
  Traversable2: () => traversableMap
};
const values = /* #__PURE__ */ (() => {
  const go = (m$p, z$p) => {
    if (m$p.tag === "Leaf") { return z$p; }
    if (m$p.tag === "Node") { return go(m$p._5, Data$dList$dTypes.$List("Cons", m$p._4, go(m$p._6, z$p))); }
    $runtime.fail();
  };
  return m => go(m, Data$dList$dTypes.Nil);
})();
const foldSubmapBy = dictOrd => appendFn => memptyValue => kmin => kmax => f => {
  const tooSmall = (() => {
    if (kmin.tag === "Just") {
      const $0 = kmin._1;
      return k => dictOrd.compare(k)($0) === "LT";
    }
    if (kmin.tag === "Nothing") { return v => false; }
    $runtime.fail();
  })();
  const tooLarge = (() => {
    if (kmax.tag === "Just") {
      const $0 = kmax._1;
      return k => dictOrd.compare(k)($0) === "GT";
    }
    if (kmax.tag === "Nothing") { return v => false; }
    $runtime.fail();
  })();
  const inBounds = (() => {
    if (kmin.tag === "Just") {
      if (kmax.tag === "Just") {
        const $0 = kmax._1;
        const $1 = kmin._1;
        return k => dictOrd.compare($1)(k) !== "GT" && dictOrd.compare(k)($0) !== "GT";
      }
      if (kmax.tag === "Nothing") {
        const $0 = kmin._1;
        return k => dictOrd.compare($0)(k) !== "GT";
      }
      $runtime.fail();
    }
    if (kmin.tag === "Nothing") {
      if (kmax.tag === "Just") {
        const $0 = kmax._1;
        return k => dictOrd.compare(k)($0) !== "GT";
      }
      if (kmax.tag === "Nothing") { return v => true; }
    }
    $runtime.fail();
  })();
  const go = v => {
    if (v.tag === "Leaf") { return memptyValue; }
    if (v.tag === "Node") {
      return appendFn(appendFn(tooSmall(v._3) ? memptyValue : go(v._5))(inBounds(v._3) ? f(v._3)(v._4) : memptyValue))(tooLarge(v._3) ? memptyValue : go(v._6));
    }
    $runtime.fail();
  };
  return go;
};
const foldSubmap = dictOrd => dictMonoid => foldSubmapBy(dictOrd)(dictMonoid.Semigroup0().append)(dictMonoid.mempty);
const findMin = findMin$a0$copy => {
  let findMin$a0 = findMin$a0$copy, findMin$c = true, findMin$r;
  while (findMin$c) {
    const v = findMin$a0;
    if (v.tag === "Leaf") {
      findMin$c = false;
      findMin$r = Data$dMaybe.Nothing;
      continue;
    }
    if (v.tag === "Node") {
      if (v._5.tag === "Leaf") {
        findMin$c = false;
        findMin$r = Data$dMaybe.$Maybe("Just", {key: v._3, value: v._4});
        continue;
      }
      findMin$a0 = v._5;
      continue;
    }
    $runtime.fail();
  }
  return findMin$r;
};
const lookupGT = dictOrd => k => {
  const go = v => {
    if (v.tag === "Leaf") { return Data$dMaybe.Nothing; }
    if (v.tag === "Node") {
      const v1 = dictOrd.compare(k)(v._3);
      if (v1 === "LT") {
        const v2 = go(v._5);
        if (v2.tag === "Nothing") { return Data$dMaybe.$Maybe("Just", {key: v._3, value: v._4}); }
        return v2;
      }
      if (v1 === "GT") { return go(v._6); }
      if (v1 === "EQ") { return findMin(v._6); }
    }
    $runtime.fail();
  };
  return go;
};
const findMax = findMax$a0$copy => {
  let findMax$a0 = findMax$a0$copy, findMax$c = true, findMax$r;
  while (findMax$c) {
    const v = findMax$a0;
    if (v.tag === "Leaf") {
      findMax$c = false;
      findMax$r = Data$dMaybe.Nothing;
      continue;
    }
    if (v.tag === "Node") {
      if (v._6.tag === "Leaf") {
        findMax$c = false;
        findMax$r = Data$dMaybe.$Maybe("Just", {key: v._3, value: v._4});
        continue;
      }
      findMax$a0 = v._6;
      continue;
    }
    $runtime.fail();
  }
  return findMax$r;
};
const lookupLT = dictOrd => k => {
  const go = v => {
    if (v.tag === "Leaf") { return Data$dMaybe.Nothing; }
    if (v.tag === "Node") {
      const v1 = dictOrd.compare(k)(v._3);
      if (v1 === "LT") { return go(v._5); }
      if (v1 === "GT") {
        const v2 = go(v._6);
        if (v2.tag === "Nothing") { return Data$dMaybe.$Maybe("Just", {key: v._3, value: v._4}); }
        return v2;
      }
      if (v1 === "EQ") { return findMax(v._5); }
    }
    $runtime.fail();
  };
  return go;
};
const filterWithKey = dictOrd => f => {
  const go = v => {
    if (v.tag === "Leaf") { return Leaf; }
    if (v.tag === "Node") {
      if (f(v._3)(v._4)) { return unsafeBalancedNode(v._3, v._4, go(v._5), go(v._6)); }
      return unsafeJoinNodes(go(v._5), go(v._6));
    }
    $runtime.fail();
  };
  return go;
};
const filterKeys = dictOrd => f => {
  const go = v => {
    if (v.tag === "Leaf") { return Leaf; }
    if (v.tag === "Node") {
      if (f(v._3)) { return unsafeBalancedNode(v._3, v._4, go(v._5), go(v._6)); }
      return unsafeJoinNodes(go(v._5), go(v._6));
    }
    $runtime.fail();
  };
  return go;
};
const filter = dictOrd => x => filterWithKey(dictOrd)(v => x);
const eqMap = dictEq => dictEq1 => (
  {
    eq: xs => ys => {
      if (xs.tag === "Leaf") { return ys.tag === "Leaf"; }
      if (xs.tag === "Node") {
        return ys.tag === "Node" && xs._2 === ys._2 && eqMapIter(dictEq)(dictEq1).eq($MapIter("IterNode", xs, IterLeaf))($MapIter("IterNode", ys, IterLeaf));
      }
      $runtime.fail();
    }
  }
);
const ordMap = dictOrd => {
  const ordMapIter1 = ordMapIter(dictOrd);
  const eqMap1 = eqMap(dictOrd.Eq0());
  return dictOrd1 => {
    const eqMap2 = eqMap1(dictOrd1.Eq0());
    return {
      compare: xs => ys => {
        if (xs.tag === "Leaf") {
          if (ys.tag === "Leaf") { return Data$dOrdering.EQ; }
          return Data$dOrdering.LT;
        }
        if (ys.tag === "Leaf") { return Data$dOrdering.GT; }
        return ordMapIter1(dictOrd1).compare($MapIter("IterNode", xs, IterLeaf))($MapIter("IterNode", ys, IterLeaf));
      },
      Eq0: () => eqMap2
    };
  };
};
const eq1Map = dictEq => ({eq1: dictEq1 => eqMap(dictEq)(dictEq1).eq});
const ord1Map = dictOrd => {
  const ordMap1 = ordMap(dictOrd);
  const $0 = dictOrd.Eq0();
  const eq1Map1 = {eq1: dictEq1 => eqMap($0)(dictEq1).eq};
  return {compare1: dictOrd1 => ordMap1(dictOrd1).compare, Eq10: () => eq1Map1};
};
const empty = Leaf;
const fromFoldable = dictOrd => dictFoldable => dictFoldable.foldl(m => v => insert(dictOrd)(v._1)(v._2)(m))(Leaf);
const fromFoldableWith = dictOrd => dictFoldable => f => {
  const f$p = insertWith(dictOrd)(b => a => f(a)(b));
  return dictFoldable.foldl(m => v => f$p(v._1)(v._2)(m))(Leaf);
};
const fromFoldableWithIndex = dictOrd => dictFoldableWithIndex => dictFoldableWithIndex.foldlWithIndex(k => m => v => insert(dictOrd)(k)(v)(m))(Leaf);
const monoidSemigroupMap = () => dictOrd => {
  const semigroupMap2 = semigroupMap()(dictOrd);
  return dictSemigroup => {
    const semigroupMap3 = semigroupMap2(dictSemigroup);
    return {mempty: Leaf, Semigroup0: () => semigroupMap3};
  };
};
const submap = dictOrd => {
  const compare = dictOrd.compare;
  return kmin => kmax => foldSubmapBy(dictOrd)(m1 => m2 => unsafeUnionWith(compare, Data$dFunction.const, m1, m2))(Leaf)(kmin)(kmax)(singleton);
};
const unions = dictOrd => {
  const compare = dictOrd.compare;
  return dictFoldable => dictFoldable.foldl(m1 => m2 => unsafeUnionWith(compare, Data$dFunction.const, m1, m2))(Leaf);
};
const difference = dictOrd => {
  const compare = dictOrd.compare;
  return m1 => m2 => unsafeDifference(compare, m1, m2);
};
const $$delete = dictOrd => k => {
  const go = v => {
    if (v.tag === "Leaf") { return Leaf; }
    if (v.tag === "Node") {
      const v1 = dictOrd.compare(k)(v._3);
      if (v1 === "LT") { return unsafeBalancedNode(v._3, v._4, go(v._5), v._6); }
      if (v1 === "GT") { return unsafeBalancedNode(v._3, v._4, v._5, go(v._6)); }
      if (v1 === "EQ") { return unsafeJoinNodes(v._5, v._6); }
    }
    $runtime.fail();
  };
  return go;
};
const checkValid = dictOrd => {
  const go = v => {
    if (v.tag === "Leaf") { return true; }
    if (v.tag === "Node") {
      if (v._5.tag === "Leaf") {
        if (v._6.tag === "Leaf") { return true; }
        if (v._6.tag === "Node") { return v._1 === 2 && v._6._1 === 1 && v._2 > v._6._2 && dictOrd.compare(v._6._3)(v._3) === "GT" && go(v._6); }
        $runtime.fail();
      }
      if (v._5.tag === "Node") {
        if (v._6.tag === "Leaf") { return v._1 === 2 && v._5._1 === 1 && v._2 > v._5._2 && dictOrd.compare(v._5._3)(v._3) === "LT" && go(v._5); }
        if (v._6.tag === "Node") {
          return v._1 > v._6._1 && dictOrd.compare(v._6._3)(v._3) === "GT" && v._1 > v._5._1 && (() => {
            const $0 = v._6._1 - v._5._1 | 0;
            return dictOrd.compare(v._5._3)(v._3) === "LT" && ($0 >= 0 ? $0 < 2 : -$0 < 2) && ((v._6._2 + v._5._2 | 0) + 1 | 0) === v._2 && go(v._5) && go(v._6);
          })();
        }
      }
    }
    $runtime.fail();
  };
  return go;
};
const catMaybes = dictOrd => mapMaybeWithKey(dictOrd)(v => identity);
const applyMap = dictOrd => (
  {
    apply: (() => {
      const compare = dictOrd.compare;
      return m1 => m2 => unsafeIntersectionWith(compare, identity, m1, m2);
    })(),
    Functor0: () => functorMap
  }
);
const bindMap = dictOrd => {
  const applyMap1 = {
    apply: (() => {
      const compare = dictOrd.compare;
      return m1 => m2 => unsafeIntersectionWith(compare, identity, m1, m2);
    })(),
    Functor0: () => functorMap
  };
  return {
    bind: m => f => mapMaybeWithKey(dictOrd)(k => {
      const go = go$a0$copy => {
        let go$a0 = go$a0$copy, go$c = true, go$r;
        while (go$c) {
          const v = go$a0;
          if (v.tag === "Leaf") {
            go$c = false;
            go$r = Data$dMaybe.Nothing;
            continue;
          }
          if (v.tag === "Node") {
            const v1 = dictOrd.compare(k)(v._3);
            if (v1 === "LT") {
              go$a0 = v._5;
              continue;
            }
            if (v1 === "GT") {
              go$a0 = v._6;
              continue;
            }
            if (v1 === "EQ") {
              go$c = false;
              go$r = Data$dMaybe.$Maybe("Just", v._4);
              continue;
            }
          }
          $runtime.fail();
        }
        return go$r;
      };
      return x => go(f(x));
    })(m),
    Apply0: () => applyMap1
  };
};
const alter = dictOrd => {
  const compare = dictOrd.compare;
  return f => k => m => {
    const v = unsafeSplit(compare, k, m);
    const v2 = f(v._1);
    if (v2.tag === "Nothing") { return unsafeJoinNodes(v._2, v._3); }
    if (v2.tag === "Just") { return unsafeBalancedNode(k, v2._1, v._2, v._3); }
    $runtime.fail();
  };
};
const altMap = dictOrd => (
  {
    alt: (() => {
      const compare = dictOrd.compare;
      return m1 => m2 => unsafeUnionWith(compare, Data$dFunction.const, m1, m2);
    })(),
    Functor0: () => functorMap
  }
);
const plusMap = dictOrd => {
  const altMap1 = {
    alt: (() => {
      const compare = dictOrd.compare;
      return m1 => m2 => unsafeUnionWith(compare, Data$dFunction.const, m1, m2);
    })(),
    Functor0: () => functorMap
  };
  return {empty: Leaf, Alt0: () => altMap1};
};
export {
  $$$Map,
  
  
  
  
  
  
  
  
  
  Leaf,
  
  
  
  
  alter,
  
  
  
  checkValid,
  $$delete as delete,
  
  
  
  eqMap,
  
  
  filterKeys,
  
  findMax,
  findMin,
  
  
  
  
  fromFoldable,
  
  
  
  
  
  insert,
  
  
  
  isEmpty,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  size,
  
  
  
  
  
  
  
  
  
  
  
  
  
  traversableMap,
  
  
  
  
  
  unsafeDifference,
  unsafeIntersectionWith,
  
  
  
  
  unsafeUnionWith,
  
  
};
