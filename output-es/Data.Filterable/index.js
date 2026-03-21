import * as $runtime from "../runtime.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dCompactable from "../Data.Compactable/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dList from "../Data.List/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMap$dInternal from "../Data.Map.Internal/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
const toUnfoldable = x => {
  const go = go$a0$copy => go$a1$copy => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
    while (go$c) {
      const source = go$a0, memo = go$a1;
      const v = Data$dMap$dInternal.stepUnfoldr(source);
      if (v.tag === "Nothing") {
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
              go$1$a0 = Data$dList$dTypes.$List("Cons", v$1._1, b);
              go$1$a1 = v$1._2;
              continue;
            }
            $runtime.fail();
          }
          return go$1$r;
        };
        go$c = false;
        go$r = go$1(Data$dList$dTypes.Nil)(memo);
        continue;
      }
      if (v.tag === "Just") {
        go$a0 = v._1._2;
        go$a1 = Data$dList$dTypes.$List("Cons", v._1._1, memo);
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go(Data$dMap$dInternal.$MapIter("IterNode", x, Data$dMap$dInternal.IterLeaf))(Data$dList$dTypes.Nil);
};
const partitionMapDefault = dictFilterable => p => {
  const $0 = dictFilterable.Functor1().map(p);
  return x => dictFilterable.Compactable0().separate($0(x));
};
const partitionMap = dict => dict.partitionMap;
const partition = dict => dict.partition;
const maybeBool = p => x => {
  if (p(x)) { return Data$dMaybe.$Maybe("Just", x); }
  return Data$dMaybe.Nothing;
};
const filterableList = {
  partitionMap: p => xs => Data$dList$dTypes.foldableList.foldr(x => v => {
    const v1 = p(x);
    if (v1.tag === "Left") { return {left: Data$dList$dTypes.$List("Cons", v1._1, v.left), right: v.right}; }
    if (v1.tag === "Right") { return {left: v.left, right: Data$dList$dTypes.$List("Cons", v1._1, v.right)}; }
    $runtime.fail();
  })({left: Data$dList$dTypes.Nil, right: Data$dList$dTypes.Nil})(xs),
  partition: p => xs => Data$dList$dTypes.foldableList.foldr(x => v => {
    if (p(x)) { return {no: v.no, yes: Data$dList$dTypes.$List("Cons", x, v.yes)}; }
    return {no: Data$dList$dTypes.$List("Cons", x, v.no), yes: v.yes};
  })({no: Data$dList$dTypes.Nil, yes: Data$dList$dTypes.Nil})(xs),
  filterMap: p => {
    const go = go$a0$copy => go$a1$copy => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const v = go$a0, v1 = go$a1;
        if (v1.tag === "Nil") {
          const go$1 = go$1$a0$copy => go$1$a1$copy => {
            let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
            while (go$1$c) {
              const v$1 = go$1$a0, v1$1 = go$1$a1;
              if (v1$1.tag === "Nil") {
                go$1$c = false;
                go$1$r = v$1;
                continue;
              }
              if (v1$1.tag === "Cons") {
                go$1$a0 = Data$dList$dTypes.$List("Cons", v1$1._1, v$1);
                go$1$a1 = v1$1._2;
                continue;
              }
              $runtime.fail();
            }
            return go$1$r;
          };
          go$c = false;
          go$r = go$1(Data$dList$dTypes.Nil)(v);
          continue;
        }
        if (v1.tag === "Cons") {
          const v2 = p(v1._1);
          if (v2.tag === "Nothing") {
            go$a0 = v;
            go$a1 = v1._2;
            continue;
          }
          if (v2.tag === "Just") {
            go$a0 = Data$dList$dTypes.$List("Cons", v2._1, v);
            go$a1 = v1._2;
            continue;
          }
        }
        $runtime.fail();
      }
      return go$r;
    };
    return go(Data$dList$dTypes.Nil);
  },
  filter: Data$dList.filter,
  Compactable0: () => Data$dCompactable.compactableList,
  Functor1: () => Data$dList$dTypes.functorList
};
const filterableArray = {
  partitionMap: p => Data$dFoldable.foldlArray(acc => x => {
    const v = p(x);
    if (v.tag === "Left") { return {left: [...acc.left, v._1], right: acc.right}; }
    if (v.tag === "Right") { return {right: [...acc.right, v._1], left: acc.left}; }
    $runtime.fail();
  })({left: [], right: []}),
  partition: Data$dArray.partition,
  filterMap: Data$dArray.mapMaybe,
  filter: Data$dArray.filter,
  Compactable0: () => Data$dCompactable.compactableArray,
  Functor1: () => Data$dFunctor.functorArray
};
const filterMapDefault = dictFilterable => p => {
  const $0 = dictFilterable.Functor1().map(p);
  return x => dictFilterable.Compactable0().compact($0(x));
};
const filterMap = dict => dict.filterMap;
const partitionDefaultFilterMap = dictFilterable => p => xs => (
  {
    yes: dictFilterable.filterMap(x => {
      if (p(x)) { return Data$dMaybe.$Maybe("Just", x); }
      return Data$dMaybe.Nothing;
    })(xs),
    no: dictFilterable.filterMap(x => {
      if (!p(x)) { return Data$dMaybe.$Maybe("Just", x); }
      return Data$dMaybe.Nothing;
    })(xs)
  }
);
const filterDefaultPartition = dictFilterable => p => xs => dictFilterable.partition(p)(xs).yes;
const filterDefault = dictFilterable => x => dictFilterable.filterMap(x$1 => {
  if (x(x$1)) { return Data$dMaybe.$Maybe("Just", x$1); }
  return Data$dMaybe.Nothing;
});
const filter = dict => dict.filter;
const partitionDefaultFilter = dictFilterable => p => xs => ({yes: dictFilterable.filter(p)(xs), no: dictFilterable.filter(a => !p(a))(xs)});
const eitherBool = p => x => {
  if (p(x)) { return Data$dEither.$Either("Right", x); }
  return Data$dEither.$Either("Left", x);
};
const filterDefaultPartitionMap = dictFilterable => p => xs => dictFilterable.partitionMap(x => {
  if (p(x)) { return Data$dEither.$Either("Right", x); }
  return Data$dEither.$Either("Left", x);
})(xs).right;
const partitionDefault = dictFilterable => p => xs => {
  const o = dictFilterable.partitionMap(x => {
    if (p(x)) { return Data$dEither.$Either("Right", x); }
    return Data$dEither.$Either("Left", x);
  })(xs);
  return {no: o.left, yes: o.right};
};
const filterableEither = dictMonoid => {
  const mempty = dictMonoid.mempty;
  const mempty$1 = dictMonoid.mempty;
  const compactableEither = {
    compact: v => {
      if (v.tag === "Left") { return Data$dEither.$Either("Left", v._1); }
      if (v.tag === "Right") {
        if (v._1.tag === "Just") { return Data$dEither.$Either("Right", v._1._1); }
        if (v._1.tag === "Nothing") { return Data$dEither.$Either("Left", mempty$1); }
      }
      $runtime.fail();
    },
    separate: v => {
      if (v.tag === "Left") { return {left: Data$dEither.$Either("Left", v._1), right: Data$dEither.$Either("Left", v._1)}; }
      if (v.tag === "Right") {
        if (v._1.tag === "Left") { return {left: Data$dEither.$Either("Right", v._1._1), right: Data$dEither.$Either("Left", mempty$1)}; }
        if (v._1.tag === "Right") { return {left: Data$dEither.$Either("Left", mempty$1), right: Data$dEither.$Either("Right", v._1._1)}; }
      }
      $runtime.fail();
    }
  };
  return {
    partitionMap: v => v1 => {
      if (v1.tag === "Left") { return {left: Data$dEither.$Either("Left", v1._1), right: Data$dEither.$Either("Left", v1._1)}; }
      if (v1.tag === "Right") {
        const v2 = v(v1._1);
        if (v2.tag === "Left") { return {left: Data$dEither.$Either("Right", v2._1), right: Data$dEither.$Either("Left", mempty)}; }
        if (v2.tag === "Right") { return {left: Data$dEither.$Either("Left", mempty), right: Data$dEither.$Either("Right", v2._1)}; }
      }
      $runtime.fail();
    },
    partition: p => {
      const $0 = filterableEither(dictMonoid);
      return xs => {
        const o = $0.partitionMap(x => {
          if (p(x)) { return Data$dEither.$Either("Right", x); }
          return Data$dEither.$Either("Left", x);
        })(xs);
        return {no: o.left, yes: o.right};
      };
    },
    filterMap: v => v1 => {
      if (v1.tag === "Left") { return Data$dEither.$Either("Left", v1._1); }
      if (v1.tag === "Right") {
        const v2 = v(v1._1);
        if (v2.tag === "Nothing") { return Data$dEither.$Either("Left", mempty); }
        if (v2.tag === "Just") { return Data$dEither.$Either("Right", v2._1); }
      }
      $runtime.fail();
    },
    filter: p => filterableEither(dictMonoid).filterMap(x => {
      if (p(x)) { return Data$dMaybe.$Maybe("Just", x); }
      return Data$dMaybe.Nothing;
    }),
    Compactable0: () => compactableEither,
    Functor1: () => Data$dEither.functorEither
  };
};
const filterableMap = dictOrd => {
  const alter = Data$dMap$dInternal.alter(dictOrd);
  const compactableMap = Data$dCompactable.compactableMap(dictOrd);
  return {
    partitionMap: p => xs => Data$dList$dTypes.foldableList.foldr(v => v1 => {
      const v2 = p(v._2);
      if (v2.tag === "Left") { return {left: Data$dMap$dInternal.insert(dictOrd)(v._1)(v2._1)(v1.left), right: v1.right}; }
      if (v2.tag === "Right") { return {left: v1.left, right: Data$dMap$dInternal.insert(dictOrd)(v._1)(v2._1)(v1.right)}; }
      $runtime.fail();
    })({left: Data$dMap$dInternal.Leaf, right: Data$dMap$dInternal.Leaf})(toUnfoldable(xs)),
    partition: p => {
      const $0 = filterableMap(dictOrd);
      return xs => {
        const o = $0.partitionMap(x => {
          if (p(x)) { return Data$dEither.$Either("Right", x); }
          return Data$dEither.$Either("Left", x);
        })(xs);
        return {no: o.left, yes: o.right};
      };
    },
    filterMap: p => xs => Data$dList$dTypes.foldableList.foldr(v => m => alter((() => {
      const $0 = p(v._2);
      return v$1 => $0;
    })())(v._1)(m))(Data$dMap$dInternal.Leaf)(toUnfoldable(xs)),
    filter: p => filterableMap(dictOrd).filterMap(x => {
      if (p(x)) { return Data$dMaybe.$Maybe("Just", x); }
      return Data$dMaybe.Nothing;
    }),
    Compactable0: () => compactableMap,
    Functor1: () => Data$dMap$dInternal.functorMap
  };
};
const filterableMaybe = {
  partitionMap: v => v1 => {
    if (v1.tag === "Nothing") { return {left: Data$dMaybe.Nothing, right: Data$dMaybe.Nothing}; }
    if (v1.tag === "Just") {
      const v2 = v(v1._1);
      if (v2.tag === "Left") { return {left: Data$dMaybe.$Maybe("Just", v2._1), right: Data$dMaybe.Nothing}; }
      if (v2.tag === "Right") { return {left: Data$dMaybe.Nothing, right: Data$dMaybe.$Maybe("Just", v2._1)}; }
    }
    $runtime.fail();
  },
  partition: p => xs => {
    const o = filterableMaybe.partitionMap(x => {
      if (p(x)) { return Data$dEither.$Either("Right", x); }
      return Data$dEither.$Either("Left", x);
    })(xs);
    return {no: o.left, yes: o.right};
  },
  filterMap: b => a => {
    if (a.tag === "Just") { return b(a._1); }
    if (a.tag === "Nothing") { return Data$dMaybe.Nothing; }
    $runtime.fail();
  },
  filter: p => filterableMaybe.filterMap(x => {
    if (p(x)) { return Data$dMaybe.$Maybe("Just", x); }
    return Data$dMaybe.Nothing;
  }),
  Compactable0: () => Data$dCompactable.compactableMaybe,
  Functor1: () => Data$dMaybe.functorMaybe
};
const cleared = dictFilterable => dictFilterable.filterMap(v => Data$dMaybe.Nothing);
export {
  cleared,
  eitherBool,
  filter,
  filterDefault,
  filterDefaultPartition,
  filterDefaultPartitionMap,
  filterMap,
  filterMapDefault,
  filterableArray,
  filterableEither,
  filterableList,
  filterableMap,
  filterableMaybe,
  maybeBool,
  partition,
  partitionDefault,
  partitionDefaultFilter,
  partitionDefaultFilterMap,
  partitionMap,
  partitionMapDefault,
  toUnfoldable
};
