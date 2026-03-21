import * as $runtime from "../runtime.js";
import * as Data$dArray$dST$dIterator from "../Data.Array.ST.Iterator/index.js";
import * as Data$dEither from "../Data.Either/index.js";
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
const separate = dict => dict.separate;
const compactableMaybe = {
  compact: m => {
    if (m.tag === "Just") { return m._1; }
    if (m.tag === "Nothing") { return Data$dMaybe.Nothing; }
    $runtime.fail();
  },
  separate: v => {
    if (v.tag === "Nothing") { return {left: Data$dMaybe.Nothing, right: Data$dMaybe.Nothing}; }
    if (v.tag === "Just") {
      if (v._1.tag === "Left") { return {left: Data$dMaybe.$Maybe("Just", v._1._1), right: Data$dMaybe.Nothing}; }
      if (v._1.tag === "Right") { return {left: Data$dMaybe.Nothing, right: Data$dMaybe.$Maybe("Just", v._1._1)}; }
    }
    $runtime.fail();
  }
};
const compactableMap = dictOrd => {
  const alter = Data$dMap$dInternal.alter(dictOrd);
  return {
    compact: (() => {
      const $0 = Data$dList$dTypes.foldableList.foldr(v => m => {
        const $0 = v._2;
        return alter(v$1 => $0)(v._1)(m);
      })(Data$dMap$dInternal.Leaf);
      return x => $0(toUnfoldable(x));
    })(),
    separate: (() => {
      const $0 = Data$dList$dTypes.foldableList.foldr(v => v1 => {
        if (v._2.tag === "Left") { return {left: Data$dMap$dInternal.insert(dictOrd)(v._1)(v._2._1)(v1.left), right: v1.right}; }
        if (v._2.tag === "Right") { return {left: v1.left, right: Data$dMap$dInternal.insert(dictOrd)(v._1)(v._2._1)(v1.right)}; }
        $runtime.fail();
      })({left: Data$dMap$dInternal.Leaf, right: Data$dMap$dInternal.Leaf});
      return x => $0(toUnfoldable(x));
    })()
  };
};
const compactableList = {
  compact: Data$dList.catMaybes,
  separate: /* #__PURE__ */ (() => {
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
          go$a0 = (() => {
            if (v._1.tag === "Left") {
              return {left: Data$dList$dTypes.foldableList.foldr(Data$dList$dTypes.Cons)(Data$dList$dTypes.$List("Cons", v._1._1, Data$dList$dTypes.Nil))(b.left), right: b.right};
            }
            if (v._1.tag === "Right") {
              return {right: Data$dList$dTypes.foldableList.foldr(Data$dList$dTypes.Cons)(Data$dList$dTypes.$List("Cons", v._1._1, Data$dList$dTypes.Nil))(b.right), left: b.left};
            }
            $runtime.fail();
          })();
          go$a1 = v._2;
          continue;
        }
        $runtime.fail();
      }
      return go$r;
    };
    return go({left: Data$dList$dTypes.Nil, right: Data$dList$dTypes.Nil});
  })()
};
const compactableEither = dictMonoid => {
  const mempty = dictMonoid.mempty;
  return {
    compact: v => {
      if (v.tag === "Left") { return Data$dEither.$Either("Left", v._1); }
      if (v.tag === "Right") {
        if (v._1.tag === "Just") { return Data$dEither.$Either("Right", v._1._1); }
        if (v._1.tag === "Nothing") { return Data$dEither.$Either("Left", mempty); }
      }
      $runtime.fail();
    },
    separate: v => {
      if (v.tag === "Left") { return {left: Data$dEither.$Either("Left", v._1), right: Data$dEither.$Either("Left", v._1)}; }
      if (v.tag === "Right") {
        if (v._1.tag === "Left") { return {left: Data$dEither.$Either("Right", v._1._1), right: Data$dEither.$Either("Left", mempty)}; }
        if (v._1.tag === "Right") { return {left: Data$dEither.$Either("Left", mempty), right: Data$dEither.$Either("Right", v._1._1)}; }
      }
      $runtime.fail();
    }
  };
};
const compactableArray = {
  compact: xs => {
    const result = [];
    const $0 = {value: 0};
    Data$dArray$dST$dIterator.iterate(Data$dArray$dST$dIterator.$Iterator(
      v => {
        if (v >= 0 && v < xs.length) { return Data$dMaybe.$Maybe("Just", xs[v]); }
        return Data$dMaybe.Nothing;
      },
      $0
    ))(x => {
      const $1 = (() => {
        if (x.tag === "Nothing") { return () => 0; }
        if (x.tag === "Just") { return () => result.push(x._1); }
        $runtime.fail();
      })();
      return () => {$1();};
    })();
    return result;
  },
  separate: xs => {
    const ls = [];
    const rs = [];
    const $0 = {value: 0};
    Data$dArray$dST$dIterator.iterate(Data$dArray$dST$dIterator.$Iterator(
      v => {
        if (v >= 0 && v < xs.length) { return Data$dMaybe.$Maybe("Just", xs[v]); }
        return Data$dMaybe.Nothing;
      },
      $0
    ))(x => {
      const $1 = (() => {
        if (x.tag === "Left") { return () => ls.push(x._1); }
        if (x.tag === "Right") { return () => rs.push(x._1); }
        $runtime.fail();
      })();
      return () => {$1();};
    })();
    return {left: ls, right: rs};
  }
};
const compactDefault = dictFunctor => dictCompactable => {
  const $0 = dictFunctor.map(v2 => {
    if (v2.tag === "Nothing") { return Data$dEither.$Either("Left", undefined); }
    if (v2.tag === "Just") { return Data$dEither.$Either("Right", v2._1); }
    $runtime.fail();
  });
  return x => dictCompactable.separate($0(x)).right;
};
const compact = dict => dict.compact;
const separateDefault = dictFunctor => dictCompactable => xs => (
  {
    left: dictCompactable.compact(dictFunctor.map(x => {
      if (x.tag === "Left") { return Data$dMaybe.$Maybe("Just", x._1); }
      if (x.tag === "Right") { return Data$dMaybe.Nothing; }
      $runtime.fail();
    })(xs)),
    right: dictCompactable.compact(dictFunctor.map(Data$dEither.hush)(xs))
  }
);
const bindMaybe = dictBind => dictCompactable => x => {
  const $0 = dictBind.bind(x);
  return x$1 => dictCompactable.compact($0(x$1));
};
const bindEither = dictBind => dictCompactable => x => {
  const $0 = dictBind.bind(x);
  return x$1 => dictCompactable.separate($0(x$1));
};
const applyMaybe = dictApply => dictCompactable => p => {
  const $0 = dictApply.apply(p);
  return x => dictCompactable.compact($0(x));
};
const applyEither = dictApply => dictCompactable => p => {
  const $0 = dictApply.apply(p);
  return x => dictCompactable.separate($0(x));
};
export {
  applyEither,
  applyMaybe,
  bindEither,
  bindMaybe,
  compact,
  compactDefault,
  compactableArray,
  compactableEither,
  compactableList,
  compactableMap,
  compactableMaybe,
  separate,
  separateDefault,
  toUnfoldable
};
