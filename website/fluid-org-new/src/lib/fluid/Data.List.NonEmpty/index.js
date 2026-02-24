import * as $runtime from "../runtime.js";
import * as Data$dList from "../Data.List/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNonEmpty from "../Data.NonEmpty/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Partial from "../Partial/index.js";
const identity = x => x;
const zipWith = f => v => v1 => Data$dNonEmpty.$NonEmpty(
  f(v._1)(v1._1),
  (() => {
    const go = go$a0$copy => go$a1$copy => go$a2$copy => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$a2 = go$a2$copy, go$c = true, go$r;
      while (go$c) {
        const v$1 = go$a0, v1$1 = go$a1, v2 = go$a2;
        if (v$1.tag === "Nil") {
          go$c = false;
          go$r = v2;
          continue;
        }
        if (v1$1.tag === "Nil") {
          go$c = false;
          go$r = v2;
          continue;
        }
        if (v$1.tag === "Cons" && v1$1.tag === "Cons") {
          go$a0 = v$1._2;
          go$a1 = v1$1._2;
          go$a2 = Data$dList$dTypes.$List("Cons", f(v$1._1)(v1$1._1), v2);
          continue;
        }
        $runtime.fail();
      }
      return go$r;
    };
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
    return go$1(Data$dList$dTypes.Nil)(go(v._2)(v1._2)(Data$dList$dTypes.Nil));
  })()
);
const zipWithA = dictApplicative => {
  const sequence11 = Data$dList$dTypes.traversable1NonEmptyList.traverse1(dictApplicative.Apply0())(Data$dList$dTypes.identity);
  return f => xs => ys => sequence11(zipWith(f)(xs)(ys));
};
const zip = /* #__PURE__ */ zipWith(Data$dTuple.Tuple);
const wrappedOperation2 = name => f => v => v1 => {
  const v2 = f(Data$dList$dTypes.$List("Cons", v._1, v._2))(Data$dList$dTypes.$List("Cons", v1._1, v1._2));
  if (v2.tag === "Cons") { return Data$dNonEmpty.$NonEmpty(v2._1, v2._2); }
  if (v2.tag === "Nil") { return Partial._crashWith("Impossible: empty list in NonEmptyList " + name); }
  $runtime.fail();
};
const wrappedOperation = name => f => v => {
  const v1 = f(Data$dList$dTypes.$List("Cons", v._1, v._2));
  if (v1.tag === "Cons") { return Data$dNonEmpty.$NonEmpty(v1._1, v1._2); }
  if (v1.tag === "Nil") { return Partial._crashWith("Impossible: empty list in NonEmptyList " + name); }
  $runtime.fail();
};
const updateAt = i => a => v => {
  if (i === 0) { return Data$dMaybe.$Maybe("Just", Data$dNonEmpty.$NonEmpty(a, v._2)); }
  const $0 = Data$dList.updateAt(i - 1 | 0)(a)(v._2);
  if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dNonEmpty.$NonEmpty(v._1, $0._1)); }
  return Data$dMaybe.Nothing;
};
const unzip = ts => Data$dTuple.$Tuple(
  Data$dNonEmpty.$NonEmpty(ts._1._1, Data$dList$dTypes.listMap(Data$dTuple.fst)(ts._2)),
  Data$dNonEmpty.$NonEmpty(ts._1._2, Data$dList$dTypes.listMap(Data$dTuple.snd)(ts._2))
);
const unsnoc = v => {
  const v1 = Data$dList.unsnoc(v._2);
  if (v1.tag === "Nothing") { return {init: Data$dList$dTypes.Nil, last: v._1}; }
  if (v1.tag === "Just") { return {init: Data$dList$dTypes.$List("Cons", v._1, v1._1.init), last: v1._1.last}; }
  $runtime.fail();
};
const unionBy = x => wrappedOperation2("unionBy")(Data$dList.unionBy(x));
const union = dictEq => wrappedOperation2("union")(Data$dList.union(dictEq));
const uncons = v => ({head: v._1, tail: v._2});
const toList = v => Data$dList$dTypes.$List("Cons", v._1, v._2);
const toUnfoldable = dictUnfoldable => {
  const $0 = dictUnfoldable.unfoldr(xs => {
    if (xs.tag === "Nil") { return Data$dMaybe.Nothing; }
    if (xs.tag === "Cons") { return Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple(xs._1, xs._2)); }
    $runtime.fail();
  });
  return x => $0(Data$dList$dTypes.$List("Cons", x._1, x._2));
};
const tail = v => v._2;
const sortBy = x => wrappedOperation("sortBy")(Data$dList.sortBy(x));
const sort = dictOrd => {
  const compare = dictOrd.compare;
  return xs => wrappedOperation("sortBy")(Data$dList.sortBy(compare))(xs);
};
const snoc = v => y => Data$dNonEmpty.$NonEmpty(v._1, Data$dList$dTypes.foldableList.foldr(Data$dList$dTypes.Cons)(Data$dList$dTypes.$List("Cons", y, Data$dList$dTypes.Nil))(v._2));
const singleton = x => Data$dNonEmpty.$NonEmpty(x, Data$dList$dTypes.Nil);
const snoc$p = v => v1 => {
  if (v.tag === "Cons") {
    return Data$dNonEmpty.$NonEmpty(v._1, Data$dList$dTypes.foldableList.foldr(Data$dList$dTypes.Cons)(Data$dList$dTypes.$List("Cons", v1, Data$dList$dTypes.Nil))(v._2));
  }
  if (v.tag === "Nil") { return Data$dNonEmpty.$NonEmpty(v1, Data$dList$dTypes.Nil); }
  $runtime.fail();
};
const reverse = /* #__PURE__ */ wrappedOperation("reverse")(Data$dList.reverse);
const nubEq = dictEq => wrappedOperation("nubEq")(Data$dList.nubEq(dictEq));
const nubByEq = x => wrappedOperation("nubByEq")(Data$dList.nubByEq(x));
const nubBy = x => wrappedOperation("nubBy")(Data$dList.nubBy(x));
const nub = dictOrd => wrappedOperation("nub")(Data$dList.nubBy(dictOrd.compare));
const modifyAt = i => f => v => {
  if (i === 0) { return Data$dMaybe.$Maybe("Just", Data$dNonEmpty.$NonEmpty(f(v._1), v._2)); }
  const $0 = Data$dList.alterAt(i - 1 | 0)(x => Data$dMaybe.$Maybe("Just", f(x)))(v._2);
  if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dNonEmpty.$NonEmpty(v._1, $0._1)); }
  return Data$dMaybe.Nothing;
};
const mapMaybe = x => {
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
        const v2 = x(v1._1);
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
  const $0 = go(Data$dList$dTypes.Nil);
  return v => $0(Data$dList$dTypes.$List("Cons", v._1, v._2));
};
const partition = x => v => Data$dList.partition(x)(Data$dList$dTypes.$List("Cons", v._1, v._2));
const span = x => v => Data$dList.span(x)(Data$dList$dTypes.$List("Cons", v._1, v._2));
const take = x => {
  const $0 = Data$dList.take(x);
  return v => $0(Data$dList$dTypes.$List("Cons", v._1, v._2));
};
const takeWhile = x => {
  const go = go$a0$copy => go$a1$copy => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0, v1 = go$a1;
      if (v1.tag === "Cons" && x(v1._1)) {
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
  const $0 = go(Data$dList$dTypes.Nil);
  return v => $0(Data$dList$dTypes.$List("Cons", v._1, v._2));
};
const length = v => {
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
        go$a0 = b + 1 | 0;
        go$a1 = v$1._2;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return 1 + go(0)(v._2) | 0;
};
const last = v => {
  if (v._2.tag === "Cons") {
    if (v._2._2.tag === "Nil") { return v._2._1; }
    if (Data$dList.last(v._2._2).tag === "Nothing") { return v._1; }
    if (Data$dList.last(v._2._2).tag === "Just") { return Data$dList.last(v._2._2)._1; }
    $runtime.fail();
  }
  return v._1;
};
const intersectBy = x => wrappedOperation2("intersectBy")(Data$dList.intersectBy(x));
const intersect = dictEq => wrappedOperation2("intersect")(Data$dList.intersect(dictEq));
const insertAt = i => a => v => {
  if (i === 0) { return Data$dMaybe.$Maybe("Just", Data$dNonEmpty.$NonEmpty(a, Data$dList$dTypes.$List("Cons", v._1, v._2))); }
  const $0 = Data$dList.insertAt(i - 1 | 0)(a)(v._2);
  if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dNonEmpty.$NonEmpty(v._1, $0._1)); }
  return Data$dMaybe.Nothing;
};
const init = v => {
  const $0 = Data$dList.unsnoc(v._2);
  if ($0.tag === "Just") { return Data$dList$dTypes.$List("Cons", v._1, $0._1.init); }
  return Data$dList$dTypes.Nil;
};
const index = v => i => {
  if (i === 0) { return Data$dMaybe.$Maybe("Just", v._1); }
  return Data$dList.index(v._2)(i - 1 | 0);
};
const head = v => v._1;
const groupBy = x => wrappedOperation("groupBy")(Data$dList.groupBy(x));
const groupAllBy = x => wrappedOperation("groupAllBy")(Data$dList.groupAllBy(x));
const groupAll = dictOrd => wrappedOperation("groupAll")(Data$dList.groupAll(dictOrd));
const group = dictEq => wrappedOperation("group")(Data$dList.group(dictEq));
const fromList = v => {
  if (v.tag === "Nil") { return Data$dMaybe.Nothing; }
  if (v.tag === "Cons") { return Data$dMaybe.$Maybe("Just", Data$dNonEmpty.$NonEmpty(v._1, v._2)); }
  $runtime.fail();
};
const fromFoldable = dictFoldable => {
  const $0 = dictFoldable.foldr(Data$dList$dTypes.Cons)(Data$dList$dTypes.Nil);
  return x => {
    const $1 = $0(x);
    if ($1.tag === "Nil") { return Data$dMaybe.Nothing; }
    if ($1.tag === "Cons") { return Data$dMaybe.$Maybe("Just", Data$dNonEmpty.$NonEmpty($1._1, $1._2)); }
    $runtime.fail();
  };
};
const foldM = dictMonad => f => b => v => {
  const $0 = v._2;
  return dictMonad.Bind1().bind(f(b)(v._1))(b$p => Data$dList.foldM(dictMonad)(f)(b$p)($0));
};
const findLastIndex = f => v => {
  const v1 = Data$dList.findLastIndex(f)(v._2);
  if (v1.tag === "Just") { return Data$dMaybe.$Maybe("Just", v1._1 + 1 | 0); }
  if (v1.tag === "Nothing") {
    if (f(v._1)) { return Data$dMaybe.$Maybe("Just", 0); }
    return Data$dMaybe.Nothing;
  }
  $runtime.fail();
};
const findIndex = f => v => {
  if (f(v._1)) { return Data$dMaybe.$Maybe("Just", 0); }
  const go = go$a0$copy => go$a1$copy => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
    while (go$c) {
      const v$1 = go$a0, v1 = go$a1;
      if (v1.tag === "Cons") {
        if (f(v1._1)) {
          go$c = false;
          go$r = Data$dMaybe.$Maybe("Just", v$1);
          continue;
        }
        go$a0 = v$1 + 1 | 0;
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
  const $0 = go(0)(v._2);
  if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", $0._1 + 1 | 0); }
  return Data$dMaybe.Nothing;
};
const filterM = dictMonad => {
  const $0 = Data$dList.filterM(dictMonad);
  return x => {
    const $1 = $0(x);
    return v => $1(Data$dList$dTypes.$List("Cons", v._1, v._2));
  };
};
const filter = x => {
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
        if (x(v1._1)) {
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
  const $0 = go(Data$dList$dTypes.Nil);
  return v => $0(Data$dList$dTypes.$List("Cons", v._1, v._2));
};
const elemLastIndex = dictEq => x => findLastIndex(v => dictEq.eq(v)(x));
const elemIndex = dictEq => x => v => {
  if (dictEq.eq(v._1)(x)) { return Data$dMaybe.$Maybe("Just", 0); }
  const go = go$a0$copy => go$a1$copy => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
    while (go$c) {
      const v$1 = go$a0, v1 = go$a1;
      if (v1.tag === "Cons") {
        if (dictEq.eq(v1._1)(x)) {
          go$c = false;
          go$r = Data$dMaybe.$Maybe("Just", v$1);
          continue;
        }
        go$a0 = v$1 + 1 | 0;
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
  const $0 = go(0)(v._2);
  if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", $0._1 + 1 | 0); }
  return Data$dMaybe.Nothing;
};
const dropWhile = x => {
  const go = go$a0$copy => {
    let go$a0 = go$a0$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0;
      if (v.tag === "Cons" && x(v._1)) {
        go$a0 = v._2;
        continue;
      }
      go$c = false;
      go$r = v;
    }
    return go$r;
  };
  return v => go(Data$dList$dTypes.$List("Cons", v._1, v._2));
};
const drop = x => v => Data$dList.drop(x)(Data$dList$dTypes.$List("Cons", v._1, v._2));
const cons$p = x => xs => Data$dNonEmpty.$NonEmpty(x, xs);
const cons = y => v => Data$dNonEmpty.$NonEmpty(y, Data$dList$dTypes.$List("Cons", v._1, v._2));
const concatMap = b => a => Data$dList$dTypes.bindNonEmptyList.bind(a)(b);
const concat = v => Data$dList$dTypes.bindNonEmptyList.bind(v)(identity);
const catMaybes = v => {
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
        if (v1._1.tag === "Nothing") {
          go$a0 = v$1;
          go$a1 = v1._2;
          continue;
        }
        if (v1._1.tag === "Just") {
          go$a0 = Data$dList$dTypes.$List("Cons", v1._1._1, v$1);
          go$a1 = v1._2;
          continue;
        }
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go(Data$dList$dTypes.Nil)(Data$dList$dTypes.$List("Cons", v._1, v._2));
};
const appendFoldable = dictFoldable => {
  const fromFoldable1 = dictFoldable.foldr(Data$dList$dTypes.Cons)(Data$dList$dTypes.Nil);
  return v => ys => Data$dNonEmpty.$NonEmpty(v._1, Data$dList$dTypes.foldableList.foldr(Data$dList$dTypes.Cons)(fromFoldable1(ys))(v._2));
};
export {
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  init,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  tail,
  
  
  
  
  
  
  
  unsnoc,
  
  
  wrappedOperation,
  
  
  
  
};
