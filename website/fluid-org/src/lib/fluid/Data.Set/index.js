// | This module defines a type of sets as height-balanced (AVL) binary trees.
// | Efficient set operations are implemented in terms of
// | <https://www.cs.cmu.edu/~guyb/papers/BFS16.pdf>
import * as $runtime from "../runtime.js";
import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dFunction from "../Data.Function/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMap$dInternal from "../Data.Map.Internal/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Data$dUnfoldable from "../Data.Unfoldable/index.js";
const identity = x => x;
const $$Set = x => x;
const union = dictOrd => {
  const compare = dictOrd.compare;
  return m1 => m2 => Data$dMap$dInternal.unsafeUnionWith(compare, Data$dFunction.const, m1, m2);
};
const toggle = dictOrd => {
  const alter = Data$dMap$dInternal.alter(dictOrd);
  return a => v => alter(v2 => {
    if (v2.tag === "Nothing") { return Data$dMaybe.$Maybe("Just", undefined); }
    if (v2.tag === "Just") { return Data$dMaybe.Nothing; }
    $runtime.fail();
  })(a)(v);
};
const toMap = v => v;
const toUnfoldable = dictUnfoldable => {
  const $0 = dictUnfoldable.unfoldr(xs => {
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
};
const toUnfoldable1 = /* #__PURE__ */ (() => {
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
const size = Data$dMap$dInternal.size;
const singleton = a => Data$dMap$dInternal.$$$Map("Node", 1, 1, a, undefined, Data$dMap$dInternal.Leaf, Data$dMap$dInternal.Leaf);
const showSet = dictShow => ({show: s => "(fromFoldable " + Data$dShow.showArrayImpl(dictShow.show)(toUnfoldable1(s)) + ")"});
const semigroupSet = dictOrd => (
  {
    append: (() => {
      const compare = dictOrd.compare;
      return m1 => m2 => Data$dMap$dInternal.unsafeUnionWith(compare, Data$dFunction.const, m1, m2);
    })()
  }
);
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
const isEmpty = Data$dMap$dInternal.isEmpty;
const intersection = dictOrd => {
  const compare = dictOrd.compare;
  return m1 => m2 => Data$dMap$dInternal.unsafeIntersectionWith(compare, Data$dFunction.const, m1, m2);
};
const insert = dictOrd => a => v => Data$dMap$dInternal.insert(dictOrd)(a)()(v);
const fromMap = $$Set;
const foldableSet = {
  foldMap: dictMonoid => {
    const foldMap1 = Data$dList$dTypes.foldableList.foldMap(dictMonoid);
    return f => {
      const $0 = foldMap1(f);
      return x => $0((() => {
        const go = (m$p, z$p) => {
          if (m$p.tag === "Leaf") { return z$p; }
          if (m$p.tag === "Node") { return go(m$p._5, Data$dList$dTypes.$List("Cons", m$p._3, go(m$p._6, z$p))); }
          $runtime.fail();
        };
        return go(x, Data$dList$dTypes.Nil);
      })());
    };
  },
  foldl: f => x => {
    const go = go$a0$copy => go$a1$copy => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const b = go$a0, v = go$a1;
        if (v.tag === "Nil") {
          go$c = false;
          go$r = b;
          continue;
        }
        if (v.tag === "Cons") {
          go$a0 = f(b)(v._1);
          go$a1 = v._2;
          continue;
        }
        $runtime.fail();
      }
      return go$r;
    };
    const $0 = go(x);
    return x$1 => $0((() => {
      const go$1 = (m$p, z$p) => {
        if (m$p.tag === "Leaf") { return z$p; }
        if (m$p.tag === "Node") { return go$1(m$p._5, Data$dList$dTypes.$List("Cons", m$p._3, go$1(m$p._6, z$p))); }
        $runtime.fail();
      };
      return go$1(x$1, Data$dList$dTypes.Nil);
    })());
  },
  foldr: f => x => {
    const $0 = Data$dList$dTypes.foldableList.foldr(f)(x);
    return x$1 => $0((() => {
      const go = (m$p, z$p) => {
        if (m$p.tag === "Leaf") { return z$p; }
        if (m$p.tag === "Node") { return go(m$p._5, Data$dList$dTypes.$List("Cons", m$p._3, go(m$p._6, z$p))); }
        $runtime.fail();
      };
      return go(x$1, Data$dList$dTypes.Nil);
    })());
  }
};
const findMin = v => {
  const $0 = Data$dMap$dInternal.findMin(v);
  if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", $0._1.key); }
  return Data$dMaybe.Nothing;
};
const findMax = v => {
  const $0 = Data$dMap$dInternal.findMax(v);
  if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", $0._1.key); }
  return Data$dMaybe.Nothing;
};
const filter = dictOrd => Data$dMap$dInternal.filterKeys(dictOrd);
const eqSet = dictEq => ({eq: v => v1 => Data$dMap$dInternal.eqMap(dictEq)(Data$dEq.eqUnit).eq(v)(v1)});
const ordSet = dictOrd => {
  const $0 = dictOrd.Eq0();
  const eqSet1 = {eq: v => v1 => Data$dMap$dInternal.eqMap($0)(Data$dEq.eqUnit).eq(v)(v1)};
  return {
    compare: s1 => s2 => Data$dList$dTypes.ordList(dictOrd).compare((() => {
      const go = (m$p, z$p) => {
        if (m$p.tag === "Leaf") { return z$p; }
        if (m$p.tag === "Node") { return go(m$p._5, Data$dList$dTypes.$List("Cons", m$p._3, go(m$p._6, z$p))); }
        $runtime.fail();
      };
      return go(s1, Data$dList$dTypes.Nil);
    })())((() => {
      const go = (m$p, z$p) => {
        if (m$p.tag === "Leaf") { return z$p; }
        if (m$p.tag === "Node") { return go(m$p._5, Data$dList$dTypes.$List("Cons", m$p._3, go(m$p._6, z$p))); }
        $runtime.fail();
      };
      return go(s2, Data$dList$dTypes.Nil);
    })()),
    Eq0: () => eqSet1
  };
};
const eq1Set = {eq1: dictEq => v => v1 => Data$dMap$dInternal.eqMap(dictEq)(Data$dEq.eqUnit).eq(v)(v1)};
const ord1Set = {compare1: dictOrd => ordSet(dictOrd).compare, Eq10: () => eq1Set};
const empty = Data$dMap$dInternal.Leaf;
const fromFoldable = dictFoldable => dictOrd => dictFoldable.foldl(m => a => Data$dMap$dInternal.insert(dictOrd)(a)()(m))(Data$dMap$dInternal.Leaf);
const map = dictOrd => f => {
  const go = go$a0$copy => go$a1$copy => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
    while (go$c) {
      const b = go$a0, v = go$a1;
      if (v.tag === "Nil") {
        go$c = false;
        go$r = b;
        continue;
      }
      if (v.tag === "Cons") {
        go$a0 = Data$dMap$dInternal.insert(dictOrd)(f(v._1))()(b);
        go$a1 = v._2;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  const $0 = go(Data$dMap$dInternal.Leaf);
  return x => $0((() => {
    const go$1 = (m$p, z$p) => {
      if (m$p.tag === "Leaf") { return z$p; }
      if (m$p.tag === "Node") { return go$1(m$p._5, Data$dList$dTypes.$List("Cons", m$p._3, go$1(m$p._6, z$p))); }
      $runtime.fail();
    };
    return go$1(x, Data$dList$dTypes.Nil);
  })());
};
const mapMaybe = dictOrd => f => foldableSet.foldr(a => acc => {
  const $0 = f(a);
  if ($0.tag === "Nothing") { return acc; }
  if ($0.tag === "Just") { return Data$dMap$dInternal.insert(dictOrd)($0._1)()(acc); }
  $runtime.fail();
})(Data$dMap$dInternal.Leaf);
const monoidSet = dictOrd => {
  const semigroupSet1 = {
    append: (() => {
      const compare = dictOrd.compare;
      return m1 => m2 => Data$dMap$dInternal.unsafeUnionWith(compare, Data$dFunction.const, m1, m2);
    })()
  };
  return {mempty: Data$dMap$dInternal.Leaf, Semigroup0: () => semigroupSet1};
};
const unions = dictFoldable => dictOrd => dictFoldable.foldl((() => {
  const compare = dictOrd.compare;
  return m1 => m2 => Data$dMap$dInternal.unsafeUnionWith(compare, Data$dFunction.const, m1, m2);
})())(Data$dMap$dInternal.Leaf);
const difference = dictOrd => {
  const compare = dictOrd.compare;
  return m1 => m2 => Data$dMap$dInternal.unsafeDifference(compare, m1, m2);
};
const subset = dictOrd => {
  const compare = dictOrd.compare;
  return s1 => s2 => Data$dMap$dInternal.unsafeDifference(compare, s1, s2).tag === "Leaf";
};
const properSubset = dictOrd => {
  const compare = dictOrd.compare;
  return s1 => s2 => (() => {
    if (s1.tag === "Leaf") { return 0; }
    if (s1.tag === "Node") { return s1._2; }
    $runtime.fail();
  })() !== (() => {
    if (s2.tag === "Leaf") { return 0; }
    if (s2.tag === "Node") { return s2._2; }
    $runtime.fail();
  })() && Data$dMap$dInternal.unsafeDifference(compare, s1, s2).tag === "Leaf";
};
const $$delete = dictOrd => Data$dMap$dInternal.delete(dictOrd);
const checkValid = dictOrd => Data$dMap$dInternal.checkValid(dictOrd);
const catMaybes = dictOrd => mapMaybe(dictOrd)(identity);
export {
  
  
  
  
  
  
  
  
  filter,
  
  
  foldableSet,
  
  
  
  
  
  
  map,
  mapMaybe,
  
  monoidSet,
  
  ordSet,
  
  
  showSet,
  singleton,
  
  
  
  
  
  
  
  unions
};
