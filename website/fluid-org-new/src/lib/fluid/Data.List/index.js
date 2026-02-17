// | This module defines a type of _strict_ linked lists, and associated helper
// | functions and type class instances.
// |
// | _Note_: Depending on your use-case, you may prefer to use
// | `Data.Sequence` instead, which might give better performance for certain
// | use cases. This module is an improvement over `Data.Array` when working with
// | immutable lists of data in a purely-functional setting, but does not have
// | good random-access performance.
import * as $runtime from "../runtime.js";
import * as Control$dMonad$dRec$dClass from "../Control.Monad.Rec.Class/index.js";
import * as Data$dList$dInternal from "../Data.List.Internal/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNonEmpty from "../Data.NonEmpty/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const any = /* #__PURE__ */ (() => Data$dList$dTypes.foldableList.foldMap((() => {
  const semigroupDisj1 = {append: v => v1 => v || v1};
  return {mempty: false, Semigroup0: () => semigroupDisj1};
})()))();
const identity = x => x;
const Pattern = x => x;
const updateAt = v => v1 => v2 => {
  if (v2.tag === "Cons") {
    if (v === 0) { return Data$dMaybe.$Maybe("Just", Data$dList$dTypes.$List("Cons", v1, v2._2)); }
    const $0 = updateAt(v - 1 | 0)(v1)(v2._2);
    if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dList$dTypes.$List("Cons", v2._1, $0._1)); }
  }
  return Data$dMaybe.Nothing;
};
const unzip = /* #__PURE__ */ (() => Data$dList$dTypes.foldableList.foldr(v => {
  const $0 = v._1;
  const $1 = v._2;
  return v1 => Data$dTuple.$Tuple(Data$dList$dTypes.$List("Cons", $0, v1._1), Data$dList$dTypes.$List("Cons", $1, v1._2));
})(Data$dTuple.$Tuple(Data$dList$dTypes.Nil, Data$dList$dTypes.Nil)))();
const uncons = v => {
  if (v.tag === "Nil") { return Data$dMaybe.Nothing; }
  if (v.tag === "Cons") { return Data$dMaybe.$Maybe("Just", {head: v._1, tail: v._2}); }
  $runtime.fail();
};
const toUnfoldable = dictUnfoldable => dictUnfoldable.unfoldr(xs => {
  if (xs.tag === "Nil") { return Data$dMaybe.Nothing; }
  if (xs.tag === "Cons") { return Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple(xs._1, xs._2)); }
  $runtime.fail();
});
const tail = v => {
  if (v.tag === "Nil") { return Data$dMaybe.Nothing; }
  if (v.tag === "Cons") { return Data$dMaybe.$Maybe("Just", v._2); }
  $runtime.fail();
};
const stripPrefix = dictEq => v => s => {
  const $0 = (prefix, input) => {
    if (input.tag === "Cons" && prefix.tag === "Cons") {
      if (dictEq.eq(prefix._1)(input._1)) { return Data$dMaybe.$Maybe("Just", Control$dMonad$dRec$dClass.$Step("Loop", {a: prefix._2, b: input._2})); }
      return Data$dMaybe.Nothing;
    }
    if (prefix.tag === "Nil") { return Data$dMaybe.$Maybe("Just", Control$dMonad$dRec$dClass.$Step("Done", input)); }
    return Data$dMaybe.Nothing;
  };
  const $1 = v$1 => {
    if (v$1.tag === "Nothing") { return Control$dMonad$dRec$dClass.$Step("Done", Data$dMaybe.Nothing); }
    if (v$1.tag === "Just") {
      if (v$1._1.tag === "Loop") { return Control$dMonad$dRec$dClass.$Step("Loop", $0(v$1._1._1.a, v$1._1._1.b)); }
      if (v$1._1.tag === "Done") { return Control$dMonad$dRec$dClass.$Step("Done", Data$dMaybe.$Maybe("Just", v$1._1._1)); }
    }
    $runtime.fail();
  };
  const go = go$a0$copy => {
    let go$a0 = go$a0$copy, go$c = true, go$r;
    while (go$c) {
      const v$1 = go$a0;
      if (v$1.tag === "Loop") {
        go$a0 = $1(v$1._1);
        continue;
      }
      if (v$1.tag === "Done") {
        go$c = false;
        go$r = v$1._1;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go($1($0(v, s)));
};
const span = v => v1 => {
  if (v1.tag === "Cons" && v(v1._1)) {
    const v2 = span(v)(v1._2);
    return {init: Data$dList$dTypes.$List("Cons", v1._1, v2.init), rest: v2.rest};
  }
  return {init: Data$dList$dTypes.Nil, rest: v1};
};
const snoc = xs => x => Data$dList$dTypes.foldableList.foldr(Data$dList$dTypes.Cons)(Data$dList$dTypes.$List("Cons", x, Data$dList$dTypes.Nil))(xs);
const singleton = a => Data$dList$dTypes.$List("Cons", a, Data$dList$dTypes.Nil);
const sortBy = cmp => {
  const merge = v => v1 => {
    if (v.tag === "Cons") {
      if (v1.tag === "Cons") {
        if (cmp(v._1)(v1._1) === "GT") { return Data$dList$dTypes.$List("Cons", v1._1, merge(v)(v1._2)); }
        return Data$dList$dTypes.$List("Cons", v._1, merge(v._2)(v1));
      }
      if (v1.tag === "Nil") { return v; }
      $runtime.fail();
    }
    if (v.tag === "Nil") { return v1; }
    if (v1.tag === "Nil") { return v; }
    $runtime.fail();
  };
  const mergePairs = v => {
    if (v.tag === "Cons" && v._2.tag === "Cons") { return Data$dList$dTypes.$List("Cons", merge(v._1)(v._2._1), mergePairs(v._2._2)); }
    return v;
  };
  const mergeAll = mergeAll$a0$copy => {
    let mergeAll$a0 = mergeAll$a0$copy, mergeAll$c = true, mergeAll$r;
    while (mergeAll$c) {
      const v = mergeAll$a0;
      if (v.tag === "Cons" && v._2.tag === "Nil") {
        mergeAll$c = false;
        mergeAll$r = v._1;
        continue;
      }
      mergeAll$a0 = mergePairs(v);
    }
    return mergeAll$r;
  };
  const $sequedesceascen = ($sequedesceascen$b$copy, $sequedesceascen$a0$copy, $sequedesceascen$a1$copy, $sequedesceascen$a2$copy) => {
    let $sequedesceascen$b = $sequedesceascen$b$copy;
    let $sequedesceascen$a0 = $sequedesceascen$a0$copy;
    let $sequedesceascen$a1 = $sequedesceascen$a1$copy;
    let $sequedesceascen$a2 = $sequedesceascen$a2$copy;
    let $sequedesceascen$c = true;
    let $sequedesceascen$r;
    while ($sequedesceascen$c) {
      if ($sequedesceascen$b === 0) {
        const v = $sequedesceascen$a0;
        if (v.tag === "Cons" && v._2.tag === "Cons") {
          if (cmp(v._1)(v._2._1) === "GT") {
            $sequedesceascen$b = 1;
            $sequedesceascen$a0 = v._2._1;
            $sequedesceascen$a1 = Data$dList$dTypes.$List("Cons", v._1, Data$dList$dTypes.Nil);
            $sequedesceascen$a2 = v._2._2;
            continue;
          }
          const $0 = v._1;
          $sequedesceascen$b = 2;
          $sequedesceascen$a0 = v._2._1;
          $sequedesceascen$a1 = v1 => Data$dList$dTypes.$List("Cons", $0, v1);
          $sequedesceascen$a2 = v._2._2;
          continue;
        }
        $sequedesceascen$c = false;
        $sequedesceascen$r = Data$dList$dTypes.$List("Cons", v, Data$dList$dTypes.Nil);
        continue;
      }
      if ($sequedesceascen$b === 1) {
        const v = $sequedesceascen$a0, v1 = $sequedesceascen$a1, v2 = $sequedesceascen$a2;
        if (v2.tag === "Cons" && cmp(v)(v2._1) === "GT") {
          $sequedesceascen$b = 1;
          $sequedesceascen$a0 = v2._1;
          $sequedesceascen$a1 = Data$dList$dTypes.$List("Cons", v, v1);
          $sequedesceascen$a2 = v2._2;
          continue;
        }
        $sequedesceascen$c = false;
        $sequedesceascen$r = Data$dList$dTypes.$List("Cons", Data$dList$dTypes.$List("Cons", v, v1), sequences(v2));
        continue;
      }
      if ($sequedesceascen$b === 2) {
        const v = $sequedesceascen$a0, v1 = $sequedesceascen$a1, v2 = $sequedesceascen$a2;
        if (
          v2.tag === "Cons" && (() => {
            const $0 = cmp(v)(v2._1);
            return $0 === "LT" || $0 !== "GT";
          })()
        ) {
          $sequedesceascen$b = 2;
          $sequedesceascen$a0 = v2._1;
          $sequedesceascen$a1 = ys => v1(Data$dList$dTypes.$List("Cons", v, ys));
          $sequedesceascen$a2 = v2._2;
          continue;
        }
        $sequedesceascen$c = false;
        $sequedesceascen$r = Data$dList$dTypes.$List("Cons", v1(Data$dList$dTypes.$List("Cons", v, Data$dList$dTypes.Nil)), sequences(v2));
      }
    }
    return $sequedesceascen$r;
  };
  const sequences = v => $sequedesceascen(0, v);
  const descending = v => v1 => v2 => $sequedesceascen(1, v, v1, v2);
  const ascending = v => v1 => v2 => $sequedesceascen(2, v, v1, v2);
  return x => mergeAll(sequences(x));
};
const sort = dictOrd => {
  const compare = dictOrd.compare;
  return xs => sortBy(compare)(xs);
};
const showPattern = dictShow => ({show: v => "(Pattern " + Data$dList$dTypes.showList(dictShow).show(v) + ")"});
const reverse = /* #__PURE__ */ (() => {
  const go = go$a0$copy => go$a1$copy => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0, v1 = go$a1;
      if (v1.tag === "Nil") {
        go$c = false;
        go$r = v;
        continue;
      }
      if (v1.tag === "Cons") {
        go$a0 = Data$dList$dTypes.$List("Cons", v1._1, v);
        go$a1 = v1._2;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go(Data$dList$dTypes.Nil);
})();
const take = /* #__PURE__ */ (() => {
  const go = go$a0$copy => go$a1$copy => go$a2$copy => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$a2 = go$a2$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0, v1 = go$a1, v2 = go$a2;
      if (v1 < 1) {
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
      if (v2.tag === "Nil") {
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
      if (v2.tag === "Cons") {
        go$a0 = Data$dList$dTypes.$List("Cons", v2._1, v);
        go$a1 = v1 - 1 | 0;
        go$a2 = v2._2;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go(Data$dList$dTypes.Nil);
})();
const takeWhile = p => {
  const go = go$a0$copy => go$a1$copy => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0, v1 = go$a1;
      if (v1.tag === "Cons" && p(v1._1)) {
        go$a0 = Data$dList$dTypes.$List("Cons", v1._1, v);
        go$a1 = v1._2;
        continue;
      }
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
    }
    return go$r;
  };
  return go(Data$dList$dTypes.Nil);
};
const unsnoc = lst => {
  const go = go$a0$copy => go$a1$copy => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0, v1 = go$a1;
      if (v.tag === "Nil") {
        go$c = false;
        go$r = Data$dMaybe.Nothing;
        continue;
      }
      if (v.tag === "Cons") {
        if (v._2.tag === "Nil") {
          go$c = false;
          go$r = Data$dMaybe.$Maybe("Just", {revInit: v1, last: v._1});
          continue;
        }
        go$a0 = v._2;
        go$a1 = Data$dList$dTypes.$List("Cons", v._1, v1);
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  const $0 = go(lst)(Data$dList$dTypes.Nil);
  if ($0.tag === "Just") {
    return Data$dMaybe.$Maybe(
      "Just",
      {
        init: (() => {
          const go$1 = go$1$a0$copy => go$1$a1$copy => {
            let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
            while (go$1$c) {
              const v = go$1$a0, v1 = go$1$a1;
              if (v1.tag === "Nil") {
                go$1$c = false;
                go$1$r = v;
                continue;
              }
              if (v1.tag === "Cons") {
                go$1$a0 = Data$dList$dTypes.$List("Cons", v1._1, v);
                go$1$a1 = v1._2;
                continue;
              }
              $runtime.fail();
            }
            return go$1$r;
          };
          return go$1(Data$dList$dTypes.Nil)($0._1.revInit);
        })(),
        last: $0._1.last
      }
    );
  }
  return Data$dMaybe.Nothing;
};
const zipWith = f => xs => ys => {
  const go = go$a0$copy => go$a1$copy => go$a2$copy => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$a2 = go$a2$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0, v1 = go$a1, v2 = go$a2;
      if (v.tag === "Nil") {
        go$c = false;
        go$r = v2;
        continue;
      }
      if (v1.tag === "Nil") {
        go$c = false;
        go$r = v2;
        continue;
      }
      if (v.tag === "Cons" && v1.tag === "Cons") {
        go$a0 = v._2;
        go$a1 = v1._2;
        go$a2 = Data$dList$dTypes.$List("Cons", f(v._1)(v1._1), v2);
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  const go$1 = go$1$a0$copy => go$1$a1$copy => {
    let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
    while (go$1$c) {
      const v = go$1$a0, v1 = go$1$a1;
      if (v1.tag === "Nil") {
        go$1$c = false;
        go$1$r = v;
        continue;
      }
      if (v1.tag === "Cons") {
        go$1$a0 = Data$dList$dTypes.$List("Cons", v1._1, v);
        go$1$a1 = v1._2;
        continue;
      }
      $runtime.fail();
    }
    return go$1$r;
  };
  return go$1(Data$dList$dTypes.Nil)(go(xs)(ys)(Data$dList$dTypes.Nil));
};
const zip = xs => ys => {
  const go = go$a0$copy => go$a1$copy => go$a2$copy => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$a2 = go$a2$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0, v1 = go$a1, v2 = go$a2;
      if (v.tag === "Nil") {
        go$c = false;
        go$r = v2;
        continue;
      }
      if (v1.tag === "Nil") {
        go$c = false;
        go$r = v2;
        continue;
      }
      if (v.tag === "Cons" && v1.tag === "Cons") {
        go$a0 = v._2;
        go$a1 = v1._2;
        go$a2 = Data$dList$dTypes.$List("Cons", Data$dTuple.$Tuple(v._1, v1._1), v2);
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  const go$1 = go$1$a0$copy => go$1$a1$copy => {
    let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
    while (go$1$c) {
      const v = go$1$a0, v1 = go$1$a1;
      if (v1.tag === "Nil") {
        go$1$c = false;
        go$1$r = v;
        continue;
      }
      if (v1.tag === "Cons") {
        go$1$a0 = Data$dList$dTypes.$List("Cons", v1._1, v);
        go$1$a1 = v1._2;
        continue;
      }
      $runtime.fail();
    }
    return go$1$r;
  };
  return go$1(Data$dList$dTypes.Nil)(go(xs)(ys)(Data$dList$dTypes.Nil));
};
const zipWithA = dictApplicative => {
  const sequence1 = Data$dList$dTypes.traversableList.traverse(dictApplicative)(Data$dList$dTypes.identity);
  return f => xs => ys => sequence1((() => {
    const go = go$a0$copy => go$a1$copy => go$a2$copy => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$a2 = go$a2$copy, go$c = true, go$r;
      while (go$c) {
        const v = go$a0, v1 = go$a1, v2 = go$a2;
        if (v.tag === "Nil") {
          go$c = false;
          go$r = v2;
          continue;
        }
        if (v1.tag === "Nil") {
          go$c = false;
          go$r = v2;
          continue;
        }
        if (v.tag === "Cons" && v1.tag === "Cons") {
          go$a0 = v._2;
          go$a1 = v1._2;
          go$a2 = Data$dList$dTypes.$List("Cons", f(v._1)(v1._1), v2);
          continue;
        }
        $runtime.fail();
      }
      return go$r;
    };
    const go$1 = go$1$a0$copy => go$1$a1$copy => {
      let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
      while (go$1$c) {
        const v = go$1$a0, v1 = go$1$a1;
        if (v1.tag === "Nil") {
          go$1$c = false;
          go$1$r = v;
          continue;
        }
        if (v1.tag === "Cons") {
          go$1$a0 = Data$dList$dTypes.$List("Cons", v1._1, v);
          go$1$a1 = v1._2;
          continue;
        }
        $runtime.fail();
      }
      return go$1$r;
    };
    return go$1(Data$dList$dTypes.Nil)(go(xs)(ys)(Data$dList$dTypes.Nil));
  })());
};
const range = start => end => {
  if (start === end) { return Data$dList$dTypes.$List("Cons", start, Data$dList$dTypes.Nil); }
  const go = go$a0$copy => go$a1$copy => go$a2$copy => go$a3$copy => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$a2 = go$a2$copy, go$a3 = go$a3$copy, go$c = true, go$r;
    while (go$c) {
      const s = go$a0, e = go$a1, step = go$a2, rest = go$a3;
      if (s === e) {
        go$c = false;
        go$r = Data$dList$dTypes.$List("Cons", s, rest);
        continue;
      }
      go$a0 = s + step | 0;
      go$a1 = e;
      go$a2 = step;
      go$a3 = Data$dList$dTypes.$List("Cons", s, rest);
    }
    return go$r;
  };
  return go(end)(start)(start > end ? 1 : -1)(Data$dList$dTypes.Nil);
};
const partition = p => xs => Data$dList$dTypes.foldableList.foldr(x => v => {
  if (p(x)) { return {no: v.no, yes: Data$dList$dTypes.$List("Cons", x, v.yes)}; }
  return {no: Data$dList$dTypes.$List("Cons", x, v.no), yes: v.yes};
})({no: Data$dList$dTypes.Nil, yes: Data$dList$dTypes.Nil})(xs);
const $$null = v => v.tag === "Nil";
const nubBy = p => {
  const go = go$a0$copy => go$a1$copy => go$a2$copy => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$a2 = go$a2$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0, v1 = go$a1, v2 = go$a2;
      if (v2.tag === "Nil") {
        go$c = false;
        go$r = v1;
        continue;
      }
      if (v2.tag === "Cons") {
        const v3 = Data$dList$dInternal.insertAndLookupBy(p)(v2._1)(v);
        if (v3.found) {
          go$a0 = v3.result;
          go$a1 = v1;
          go$a2 = v2._2;
          continue;
        }
        go$a0 = v3.result;
        go$a1 = Data$dList$dTypes.$List("Cons", v2._1, v1);
        go$a2 = v2._2;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  const $0 = go(Data$dList$dInternal.Leaf)(Data$dList$dTypes.Nil);
  return x => {
    const go$1 = go$1$a0$copy => go$1$a1$copy => {
      let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
      while (go$1$c) {
        const v = go$1$a0, v1 = go$1$a1;
        if (v1.tag === "Nil") {
          go$1$c = false;
          go$1$r = v;
          continue;
        }
        if (v1.tag === "Cons") {
          go$1$a0 = Data$dList$dTypes.$List("Cons", v1._1, v);
          go$1$a1 = v1._2;
          continue;
        }
        $runtime.fail();
      }
      return go$1$r;
    };
    return go$1(Data$dList$dTypes.Nil)($0(x));
  };
};
const nub = dictOrd => nubBy(dictOrd.compare);
const newtypePattern = {Coercible0: () => {}};
const mapMaybe = f => {
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
        const v2 = f(v1._1);
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
};
const manyRec = dictMonadRec => dictAlternative => {
  const Alt0 = dictAlternative.Plus1().Alt0();
  const $0 = dictAlternative.Applicative0();
  return p => dictMonadRec.tailRecM(acc => dictMonadRec.Monad0().Bind1().bind(Alt0.alt(Alt0.Functor0().map(Control$dMonad$dRec$dClass.Loop)(p))($0.pure(Control$dMonad$dRec$dClass.$Step(
    "Done",
    undefined
  ))))(aa => $0.pure((() => {
    if (aa.tag === "Loop") { return Control$dMonad$dRec$dClass.$Step("Loop", Data$dList$dTypes.$List("Cons", aa._1, acc)); }
    if (aa.tag === "Done") {
      return Control$dMonad$dRec$dClass.$Step(
        "Done",
        (() => {
          const go = go$a0$copy => go$a1$copy => {
            let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
            while (go$c) {
              const v = go$a0, v1 = go$a1;
              if (v1.tag === "Nil") {
                go$c = false;
                go$r = v;
                continue;
              }
              if (v1.tag === "Cons") {
                go$a0 = Data$dList$dTypes.$List("Cons", v1._1, v);
                go$a1 = v1._2;
                continue;
              }
              $runtime.fail();
            }
            return go$r;
          };
          return go(Data$dList$dTypes.Nil)(acc);
        })()
      );
    }
    $runtime.fail();
  })())))(Data$dList$dTypes.Nil);
};
const someRec = dictMonadRec => dictAlternative => {
  const manyRec2 = manyRec(dictMonadRec)(dictAlternative);
  return v => dictAlternative.Applicative0().Apply0().apply(dictAlternative.Plus1().Alt0().Functor0().map(Data$dList$dTypes.Cons)(v))(manyRec2(v));
};
const some = dictAlternative => dictLazy => v => dictAlternative.Applicative0().Apply0().apply(dictAlternative.Plus1().Alt0().Functor0().map(Data$dList$dTypes.Cons)(v))(dictLazy.defer(v1 => many(dictAlternative)(dictLazy)(v)));
const many = dictAlternative => dictLazy => v => dictAlternative.Plus1().Alt0().alt(some(dictAlternative)(dictLazy)(v))(dictAlternative.Applicative0().pure(Data$dList$dTypes.Nil));
const length = /* #__PURE__ */ (() => {
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
        go$a0 = b + 1 | 0;
        go$a1 = v._2;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go(0);
})();
const last = last$a0$copy => {
  let last$a0 = last$a0$copy, last$c = true, last$r;
  while (last$c) {
    const v = last$a0;
    if (v.tag === "Cons") {
      if (v._2.tag === "Nil") {
        last$c = false;
        last$r = Data$dMaybe.$Maybe("Just", v._1);
        continue;
      }
      last$a0 = v._2;
      continue;
    }
    last$c = false;
    last$r = Data$dMaybe.Nothing;
  }
  return last$r;
};
const insertBy = v => v1 => v2 => {
  if (v2.tag === "Nil") { return Data$dList$dTypes.$List("Cons", v1, Data$dList$dTypes.Nil); }
  if (v2.tag === "Cons") {
    if (v(v1)(v2._1) === "GT") { return Data$dList$dTypes.$List("Cons", v2._1, insertBy(v)(v1)(v2._2)); }
    return Data$dList$dTypes.$List("Cons", v1, v2);
  }
  $runtime.fail();
};
const insertAt = v => v1 => v2 => {
  if (v === 0) { return Data$dMaybe.$Maybe("Just", Data$dList$dTypes.$List("Cons", v1, v2)); }
  if (v2.tag === "Cons") {
    const $0 = insertAt(v - 1 | 0)(v1)(v2._2);
    if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dList$dTypes.$List("Cons", v2._1, $0._1)); }
  }
  return Data$dMaybe.Nothing;
};
const insert = dictOrd => insertBy(dictOrd.compare);
const init = lst => {
  const $0 = unsnoc(lst);
  if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", $0._1.init); }
  return Data$dMaybe.Nothing;
};
const index = index$a0$copy => index$a1$copy => {
  let index$a0 = index$a0$copy, index$a1 = index$a1$copy, index$c = true, index$r;
  while (index$c) {
    const v = index$a0, v1 = index$a1;
    if (v.tag === "Nil") {
      index$c = false;
      index$r = Data$dMaybe.Nothing;
      continue;
    }
    if (v.tag === "Cons") {
      if (v1 === 0) {
        index$c = false;
        index$r = Data$dMaybe.$Maybe("Just", v._1);
        continue;
      }
      index$a0 = v._2;
      index$a1 = v1 - 1 | 0;
      continue;
    }
    $runtime.fail();
  }
  return index$r;
};
const head = v => {
  if (v.tag === "Nil") { return Data$dMaybe.Nothing; }
  if (v.tag === "Cons") { return Data$dMaybe.$Maybe("Just", v._1); }
  $runtime.fail();
};
const transpose = v => {
  if (v.tag === "Nil") { return Data$dList$dTypes.Nil; }
  if (v.tag === "Cons") {
    if (v._1.tag === "Nil") { return transpose(v._2); }
    if (v._1.tag === "Cons") {
      return Data$dList$dTypes.$List(
        "Cons",
        Data$dList$dTypes.$List(
          "Cons",
          v._1._1,
          (() => {
            const go = go$a0$copy => go$a1$copy => {
              let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
              while (go$c) {
                const v$1 = go$a0, v1 = go$a1;
                if (v1.tag === "Nil") {
                  const go$1 = go$1$a0$copy => go$1$a1$copy => {
                    let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
                    while (go$1$c) {
                      const v$2 = go$1$a0, v1$1 = go$1$a1;
                      if (v1$1.tag === "Nil") {
                        go$1$c = false;
                        go$1$r = v$2;
                        continue;
                      }
                      if (v1$1.tag === "Cons") {
                        go$1$a0 = Data$dList$dTypes.$List("Cons", v1$1._1, v$2);
                        go$1$a1 = v1$1._2;
                        continue;
                      }
                      $runtime.fail();
                    }
                    return go$1$r;
                  };
                  go$c = false;
                  go$r = go$1(Data$dList$dTypes.Nil)(v$1);
                  continue;
                }
                if (v1.tag === "Cons") {
                  if (v1._1.tag === "Nil") {
                    go$a0 = v$1;
                    go$a1 = v1._2;
                    continue;
                  }
                  if (v1._1.tag === "Cons") {
                    go$a0 = Data$dList$dTypes.$List("Cons", v1._1._1, v$1);
                    go$a1 = v1._2;
                    continue;
                  }
                }
                $runtime.fail();
              }
              return go$r;
            };
            return go(Data$dList$dTypes.Nil)(v._2);
          })()
        ),
        transpose(Data$dList$dTypes.$List(
          "Cons",
          v._1._2,
          (() => {
            const go = go$a0$copy => go$a1$copy => {
              let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
              while (go$c) {
                const v$1 = go$a0, v1 = go$a1;
                if (v1.tag === "Nil") {
                  const go$1 = go$1$a0$copy => go$1$a1$copy => {
                    let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
                    while (go$1$c) {
                      const v$2 = go$1$a0, v1$1 = go$1$a1;
                      if (v1$1.tag === "Nil") {
                        go$1$c = false;
                        go$1$r = v$2;
                        continue;
                      }
                      if (v1$1.tag === "Cons") {
                        go$1$a0 = Data$dList$dTypes.$List("Cons", v1$1._1, v$2);
                        go$1$a1 = v1$1._2;
                        continue;
                      }
                      $runtime.fail();
                    }
                    return go$1$r;
                  };
                  go$c = false;
                  go$r = go$1(Data$dList$dTypes.Nil)(v$1);
                  continue;
                }
                if (v1.tag === "Cons") {
                  if (v1._1.tag === "Nil") {
                    go$a0 = v$1;
                    go$a1 = v1._2;
                    continue;
                  }
                  if (v1._1.tag === "Cons") {
                    go$a0 = Data$dList$dTypes.$List("Cons", v1._1._2, v$1);
                    go$a1 = v1._2;
                    continue;
                  }
                }
                $runtime.fail();
              }
              return go$r;
            };
            return go(Data$dList$dTypes.Nil)(v._2);
          })()
        ))
      );
    }
  }
  $runtime.fail();
};
const groupBy = v => v1 => {
  if (v1.tag === "Nil") { return Data$dList$dTypes.Nil; }
  if (v1.tag === "Cons") {
    const v2 = span(v(v1._1))(v1._2);
    return Data$dList$dTypes.$List("Cons", Data$dNonEmpty.$NonEmpty(v1._1, v2.init), groupBy(v)(v2.rest));
  }
  $runtime.fail();
};
const groupAllBy = p => {
  const $0 = groupBy(x => y => p(x)(y) === "EQ");
  const $1 = sortBy(p);
  return x => $0($1(x));
};
const group = dictEq => groupBy(dictEq.eq);
const groupAll = dictOrd => {
  const $0 = group(dictOrd.Eq0());
  const compare = dictOrd.compare;
  return x => $0(sortBy(compare)(x));
};
const fromFoldable = dictFoldable => dictFoldable.foldr(Data$dList$dTypes.Cons)(Data$dList$dTypes.Nil);
const foldM = dictMonad => v => v1 => v2 => {
  if (v2.tag === "Nil") { return dictMonad.Applicative0().pure(v1); }
  if (v2.tag === "Cons") {
    const $0 = v2._2;
    return dictMonad.Bind1().bind(v(v1)(v2._1))(b$p => foldM(dictMonad)(v)(b$p)($0));
  }
  $runtime.fail();
};
const findIndex = fn => {
  const go = go$a0$copy => go$a1$copy => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0, v1 = go$a1;
      if (v1.tag === "Cons") {
        if (fn(v1._1)) {
          go$c = false;
          go$r = Data$dMaybe.$Maybe("Just", v);
          continue;
        }
        go$a0 = v + 1 | 0;
        go$a1 = v1._2;
        continue;
      }
      if (v1.tag === "Nil") {
        go$c = false;
        go$r = Data$dMaybe.Nothing;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go(0);
};
const findLastIndex = fn => xs => {
  const go = go$a0$copy => go$a1$copy => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0, v1 = go$a1;
      if (v1.tag === "Cons") {
        if (fn(v1._1)) {
          go$c = false;
          go$r = Data$dMaybe.$Maybe("Just", v);
          continue;
        }
        go$a0 = v + 1 | 0;
        go$a1 = v1._2;
        continue;
      }
      if (v1.tag === "Nil") {
        go$c = false;
        go$r = Data$dMaybe.Nothing;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  const $0 = go(0)((() => {
    const go$1 = go$1$a0$copy => go$1$a1$copy => {
      let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
      while (go$1$c) {
        const v = go$1$a0, v1 = go$1$a1;
        if (v1.tag === "Nil") {
          go$1$c = false;
          go$1$r = v;
          continue;
        }
        if (v1.tag === "Cons") {
          go$1$a0 = Data$dList$dTypes.$List("Cons", v1._1, v);
          go$1$a1 = v1._2;
          continue;
        }
        $runtime.fail();
      }
      return go$1$r;
    };
    return go$1(Data$dList$dTypes.Nil)(xs);
  })());
  if ($0.tag === "Just") {
    return Data$dMaybe.$Maybe(
      "Just",
      (() => {
        const go$1 = go$1$a0$copy => go$1$a1$copy => {
          let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
          while (go$1$c) {
            const b = go$1$a0, v = go$1$a1;
            if (v.tag === "Nil") {
              go$1$c = false;
              go$1$r = b;
              continue;
            }
            if (v.tag === "Cons") {
              go$1$a0 = b + 1 | 0;
              go$1$a1 = v._2;
              continue;
            }
            $runtime.fail();
          }
          return go$1$r;
        };
        return (go$1(0)(xs) - 1 | 0) - $0._1 | 0;
      })()
    );
  }
  return Data$dMaybe.Nothing;
};
const filterM = dictMonad => {
  const $0 = dictMonad.Applicative0();
  const $1 = dictMonad.Bind1();
  return v => v1 => {
    if (v1.tag === "Nil") { return $0.pure(Data$dList$dTypes.Nil); }
    if (v1.tag === "Cons") {
      const $2 = v1._1;
      const $3 = v1._2;
      return $1.bind(v($2))(b => $1.bind(filterM(dictMonad)(v)($3))(xs$p => $0.pure(b ? Data$dList$dTypes.$List("Cons", $2, xs$p) : xs$p)));
    }
    $runtime.fail();
  };
};
const filter = p => {
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
        if (p(v1._1)) {
          go$a0 = Data$dList$dTypes.$List("Cons", v1._1, v);
          go$a1 = v1._2;
          continue;
        }
        go$a0 = v;
        go$a1 = v1._2;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go(Data$dList$dTypes.Nil);
};
const intersectBy = v => v1 => v2 => {
  if (v1.tag === "Nil") { return Data$dList$dTypes.Nil; }
  if (v2.tag === "Nil") { return Data$dList$dTypes.Nil; }
  const go = go$a0$copy => go$a1$copy => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
    while (go$c) {
      const v$1 = go$a0, v1$1 = go$a1;
      if (v1$1.tag === "Nil") {
        const go$1 = go$1$a0$copy => go$1$a1$copy => {
          let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
          while (go$1$c) {
            const v$2 = go$1$a0, v1$2 = go$1$a1;
            if (v1$2.tag === "Nil") {
              go$1$c = false;
              go$1$r = v$2;
              continue;
            }
            if (v1$2.tag === "Cons") {
              go$1$a0 = Data$dList$dTypes.$List("Cons", v1$2._1, v$2);
              go$1$a1 = v1$2._2;
              continue;
            }
            $runtime.fail();
          }
          return go$1$r;
        };
        go$c = false;
        go$r = go$1(Data$dList$dTypes.Nil)(v$1);
        continue;
      }
      if (v1$1.tag === "Cons") {
        if (any(v(v1$1._1))(v2)) {
          go$a0 = Data$dList$dTypes.$List("Cons", v1$1._1, v$1);
          go$a1 = v1$1._2;
          continue;
        }
        go$a0 = v$1;
        go$a1 = v1$1._2;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go(Data$dList$dTypes.Nil)(v1);
};
const intersect = dictEq => intersectBy(dictEq.eq);
const nubByEq = v => v1 => {
  if (v1.tag === "Nil") { return Data$dList$dTypes.Nil; }
  if (v1.tag === "Cons") {
    const $0 = v1._1;
    return Data$dList$dTypes.$List(
      "Cons",
      $0,
      nubByEq(v)((() => {
        const go = go$a0$copy => go$a1$copy => {
          let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
          while (go$c) {
            const v$1 = go$a0, v1$1 = go$a1;
            if (v1$1.tag === "Nil") {
              const go$1 = go$1$a0$copy => go$1$a1$copy => {
                let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
                while (go$1$c) {
                  const v$2 = go$1$a0, v1$2 = go$1$a1;
                  if (v1$2.tag === "Nil") {
                    go$1$c = false;
                    go$1$r = v$2;
                    continue;
                  }
                  if (v1$2.tag === "Cons") {
                    go$1$a0 = Data$dList$dTypes.$List("Cons", v1$2._1, v$2);
                    go$1$a1 = v1$2._2;
                    continue;
                  }
                  $runtime.fail();
                }
                return go$1$r;
              };
              go$c = false;
              go$r = go$1(Data$dList$dTypes.Nil)(v$1);
              continue;
            }
            if (v1$1.tag === "Cons") {
              if (!v($0)(v1$1._1)) {
                go$a0 = Data$dList$dTypes.$List("Cons", v1$1._1, v$1);
                go$a1 = v1$1._2;
                continue;
              }
              go$a0 = v$1;
              go$a1 = v1$1._2;
              continue;
            }
            $runtime.fail();
          }
          return go$r;
        };
        return go(Data$dList$dTypes.Nil)(v1._2);
      })())
    );
  }
  $runtime.fail();
};
const nubEq = dictEq => nubByEq(dictEq.eq);
const eqPattern = dictEq => (
  {
    eq: x => y => {
      const go = v => v1 => v2 => {
        if (!v2) { return false; }
        if (v.tag === "Nil") { return v1.tag === "Nil" && v2; }
        return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && dictEq.eq(v1._1)(v._1));
      };
      return go(x)(y)(true);
    }
  }
);
const ordPattern = dictOrd => {
  const $0 = dictOrd.Eq0();
  const eqPattern1 = {
    eq: x => y => {
      const go = v => v1 => v2 => {
        if (!v2) { return false; }
        if (v.tag === "Nil") { return v1.tag === "Nil" && v2; }
        return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && $0.eq(v1._1)(v._1));
      };
      return go(x)(y)(true);
    }
  };
  return {compare: x => y => Data$dList$dTypes.ordList(dictOrd).compare(x)(y), Eq0: () => eqPattern1};
};
const elemLastIndex = dictEq => x => findLastIndex(v => dictEq.eq(v)(x));
const elemIndex = dictEq => x => {
  const go = go$a0$copy => go$a1$copy => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0, v1 = go$a1;
      if (v1.tag === "Cons") {
        if (dictEq.eq(v1._1)(x)) {
          go$c = false;
          go$r = Data$dMaybe.$Maybe("Just", v);
          continue;
        }
        go$a0 = v + 1 | 0;
        go$a1 = v1._2;
        continue;
      }
      if (v1.tag === "Nil") {
        go$c = false;
        go$r = Data$dMaybe.Nothing;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go(0);
};
const dropWhile = p => {
  const go = go$a0$copy => {
    let go$a0 = go$a0$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0;
      if (v.tag === "Cons" && p(v._1)) {
        go$a0 = v._2;
        continue;
      }
      go$c = false;
      go$r = v;
    }
    return go$r;
  };
  return go;
};
const dropEnd = n => xs => take((() => {
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
        go$a0 = b + 1 | 0;
        go$a1 = v._2;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go(0)(xs) - n | 0;
})())(xs);
const drop = drop$a0$copy => drop$a1$copy => {
  let drop$a0 = drop$a0$copy, drop$a1 = drop$a1$copy, drop$c = true, drop$r;
  while (drop$c) {
    const v = drop$a0, v1 = drop$a1;
    if (v < 1) {
      drop$c = false;
      drop$r = v1;
      continue;
    }
    if (v1.tag === "Nil") {
      drop$c = false;
      drop$r = Data$dList$dTypes.Nil;
      continue;
    }
    if (v1.tag === "Cons") {
      drop$a0 = v - 1 | 0;
      drop$a1 = v1._2;
      continue;
    }
    $runtime.fail();
  }
  return drop$r;
};
const slice = start => end => xs => take(end - start | 0)(drop(start)(xs));
const takeEnd = n => xs => drop((() => {
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
        go$a0 = b + 1 | 0;
        go$a1 = v._2;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go(0)(xs) - n | 0;
})())(xs);
const deleteBy = v => v1 => v2 => {
  if (v2.tag === "Nil") { return Data$dList$dTypes.Nil; }
  if (v2.tag === "Cons") {
    if (v(v1)(v2._1)) { return v2._2; }
    return Data$dList$dTypes.$List("Cons", v2._1, deleteBy(v)(v1)(v2._2));
  }
  $runtime.fail();
};
const unionBy = eq2 => xs => ys => {
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
        go$a0 = deleteBy(eq2)(v._1)(b);
        go$a1 = v._2;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return Data$dList$dTypes.foldableList.foldr(Data$dList$dTypes.Cons)(go(nubByEq(eq2)(ys))(xs))(xs);
};
const union = dictEq => unionBy(dictEq.eq);
const deleteAt = v => v1 => {
  if (v1.tag === "Cons") {
    if (v === 0) { return Data$dMaybe.$Maybe("Just", v1._2); }
    const $0 = deleteAt(v - 1 | 0)(v1._2);
    if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dList$dTypes.$List("Cons", v1._1, $0._1)); }
  }
  return Data$dMaybe.Nothing;
};
const $$delete = dictEq => deleteBy(dictEq.eq);
const difference = dictEq => {
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
        go$a0 = deleteBy(dictEq.eq)(v._1)(b);
        go$a1 = v._2;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go;
};
const concatMap = b => a => Data$dList$dTypes.bindList.bind(a)(b);
const concat = v => Data$dList$dTypes.bindList.bind(v)(identity);
const catMaybes = /* #__PURE__ */ (() => {
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
        if (v1._1.tag === "Nothing") {
          go$a0 = v;
          go$a1 = v1._2;
          continue;
        }
        if (v1._1.tag === "Just") {
          go$a0 = Data$dList$dTypes.$List("Cons", v1._1._1, v);
          go$a1 = v1._2;
          continue;
        }
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go(Data$dList$dTypes.Nil);
})();
const alterAt = v => v1 => v2 => {
  if (v2.tag === "Cons") {
    if (v === 0) {
      return Data$dMaybe.$Maybe(
        "Just",
        (() => {
          const v3 = v1(v2._1);
          if (v3.tag === "Nothing") { return v2._2; }
          if (v3.tag === "Just") { return Data$dList$dTypes.$List("Cons", v3._1, v2._2); }
          $runtime.fail();
        })()
      );
    }
    const $0 = alterAt(v - 1 | 0)(v1)(v2._2);
    if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dList$dTypes.$List("Cons", v2._1, $0._1)); }
  }
  return Data$dMaybe.Nothing;
};
const modifyAt = n => f => alterAt(n)(x => Data$dMaybe.$Maybe("Just", f(x)));
export {
  
  alterAt,
  
  
  
  
  
  
  deleteBy,
  difference,
  drop,
  
  
  
  
  
  
  filterM,
  
  findLastIndex,
  foldM,
  
  group,
  groupAll,
  groupAllBy,
  groupBy,
  
  identity,
  index,
  
  
  insertAt,
  
  intersect,
  intersectBy,
  last,
  length,
  many,
  manyRec,
  
  
  
  
  nubBy,
  nubByEq,
  nubEq,
  
  
  partition,
  
  reverse,
  
  
  
  
  
  
  
  sortBy,
  span,
  
  
  take,
  
  
  
  
  
  union,
  unionBy,
  unsnoc,
  unzip,
  updateAt,
  zip,
  zipWith,
  
};
