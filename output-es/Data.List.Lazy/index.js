// | This module defines a type of _lazy_ linked lists, and associated helper
// | functions and type class instances.
// |
// | _Note_: Depending on your use-case, you may prefer to use
// | `Data.Sequence` instead, which might give better performance for certain
// | use cases. This module is an improvement over `Data.Array` when working with
// | immutable lists of data in a purely-functional setting, but does not have
// | good random-access performance.
import * as $runtime from "../runtime.js";
import * as Control$dMonad$dRec$dClass from "../Control.Monad.Rec.Class/index.js";
import * as Data$dLazy from "../Data.Lazy/index.js";
import * as Data$dList$dInternal from "../Data.List.Internal/index.js";
import * as Data$dList$dLazy$dTypes from "../Data.List.Lazy.Types/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNonEmpty from "../Data.NonEmpty/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const any = /* #__PURE__ */ (() => Data$dList$dLazy$dTypes.foldableList.foldMap((() => {
  const semigroupDisj1 = {append: v => v1 => v || v1};
  return {mempty: false, Semigroup0: () => semigroupDisj1};
})()))();
const identity = x => x;
const Pattern = x => x;
const zipWith = f => xs => ys => {
  const $0 = Data$dLazy.defer(v => {
    const $0 = Data$dLazy.force(xs);
    return v1 => {
      if ($0.tag === "Nil") { return Data$dList$dLazy$dTypes.Nil; }
      if (v1.tag === "Nil") { return Data$dList$dLazy$dTypes.Nil; }
      if ($0.tag === "Cons" && v1.tag === "Cons") { return Data$dList$dLazy$dTypes.$Step("Cons", f($0._1)(v1._1), zipWith(f)($0._2)(v1._2)); }
      $runtime.fail();
    };
  });
  return Data$dLazy.defer(v => Data$dLazy.force($0)(Data$dLazy.force(ys)));
};
const zipWithA = dictApplicative => {
  const sequence1 = Data$dList$dLazy$dTypes.traversableList.traverse(dictApplicative)(Data$dList$dLazy$dTypes.identity);
  return f => xs => ys => sequence1(zipWith(f)(xs)(ys));
};
const zip = /* #__PURE__ */ zipWith(Data$dTuple.Tuple);
const updateAt = n => x => xs => Data$dLazy.defer(v => {
  const $0 = Data$dLazy.force(xs);
  if ($0.tag === "Nil") { return Data$dList$dLazy$dTypes.Nil; }
  if ($0.tag === "Cons") {
    if (n === 0) { return Data$dList$dLazy$dTypes.$Step("Cons", x, $0._2); }
    return Data$dList$dLazy$dTypes.$Step("Cons", $0._1, updateAt(n - 1 | 0)(x)($0._2));
  }
  $runtime.fail();
});
const unzip = /* #__PURE__ */ (() => Data$dList$dLazy$dTypes.foldableList.foldr(v => {
  const $0 = v._1;
  const $1 = v._2;
  return v1 => {
    const $2 = v1._1;
    const $3 = v1._2;
    return Data$dTuple.$Tuple(Data$dLazy.defer(v$1 => Data$dList$dLazy$dTypes.$Step("Cons", $0, $2)), Data$dLazy.defer(v$1 => Data$dList$dLazy$dTypes.$Step("Cons", $1, $3)));
  };
})(Data$dTuple.$Tuple(Data$dList$dLazy$dTypes.nil, Data$dList$dLazy$dTypes.nil)))();
const uncons = xs => {
  const v = Data$dLazy.force(xs);
  if (v.tag === "Nil") { return Data$dMaybe.Nothing; }
  if (v.tag === "Cons") { return Data$dMaybe.$Maybe("Just", {head: v._1, tail: v._2}); }
  $runtime.fail();
};
const toUnfoldable = dictUnfoldable => dictUnfoldable.unfoldr(xs => {
  const $0 = uncons(xs);
  if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple($0._1.head, $0._1.tail)); }
  return Data$dMaybe.Nothing;
});
const takeWhile = p => Data$dLazy.functorLazy.map(v => {
  if (v.tag === "Cons" && p(v._1)) { return Data$dList$dLazy$dTypes.$Step("Cons", v._1, takeWhile(p)(v._2)); }
  return Data$dList$dLazy$dTypes.Nil;
});
const take = n => {
  if (n <= 0) { return v => Data$dList$dLazy$dTypes.nil; }
  return Data$dLazy.functorLazy.map(v1 => {
    if (v1.tag === "Nil") { return Data$dList$dLazy$dTypes.Nil; }
    if (v1.tag === "Cons") { return Data$dList$dLazy$dTypes.$Step("Cons", v1._1, take(n - 1 | 0)(v1._2)); }
    $runtime.fail();
  });
};
const tail = xs => {
  const $0 = uncons(xs);
  if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", $0._1.tail); }
  return Data$dMaybe.Nothing;
};
const stripPrefix = dictEq => v => s => {
  const $0 = (prefix, input) => {
    const v1 = Data$dLazy.force(prefix);
    if (v1.tag === "Nil") { return Data$dMaybe.$Maybe("Just", Control$dMonad$dRec$dClass.$Step("Done", input)); }
    if (v1.tag === "Cons") {
      const v2 = Data$dLazy.force(input);
      if (v2.tag === "Cons" && dictEq.eq(v1._1)(v2._1)) { return Data$dMaybe.$Maybe("Just", Control$dMonad$dRec$dClass.$Step("Loop", {a: v1._2, b: v2._2})); }
      return Data$dMaybe.Nothing;
    }
    $runtime.fail();
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
const span = p => xs => {
  const v = uncons(xs);
  if (v.tag === "Just" && p(v._1.head)) {
    const $0 = v._1.head;
    const v1 = span(p)(v._1.tail);
    const $1 = v1.init;
    return {init: Data$dLazy.defer(v$1 => Data$dList$dLazy$dTypes.$Step("Cons", $0, $1)), rest: v1.rest};
  }
  return {init: Data$dList$dLazy$dTypes.nil, rest: xs};
};
const snoc = xs => x => Data$dList$dLazy$dTypes.foldableList.foldr(Data$dList$dLazy$dTypes.cons)(Data$dLazy.defer(v => Data$dList$dLazy$dTypes.$Step(
  "Cons",
  x,
  Data$dList$dLazy$dTypes.nil
)))(xs);
const singleton = a => Data$dLazy.defer(v => Data$dList$dLazy$dTypes.$Step("Cons", a, Data$dList$dLazy$dTypes.nil));
const showPattern = dictShow => ({show: v => "(Pattern " + Data$dList$dLazy$dTypes.showList(dictShow).show(v) + ")"});
const scanlLazy = f => acc => xs => Data$dLazy.defer(v => {
  const $0 = Data$dLazy.force(xs);
  if ($0.tag === "Nil") { return Data$dList$dLazy$dTypes.Nil; }
  if ($0.tag === "Cons") {
    const acc$p = f(acc)($0._1);
    return Data$dList$dLazy$dTypes.$Step("Cons", acc$p, scanlLazy(f)(acc$p)($0._2));
  }
  $runtime.fail();
});
const reverse = xs => Data$dLazy.defer(x => Data$dLazy.force(Data$dList$dLazy$dTypes.foldableList.foldl(b => a => Data$dLazy.defer(v => Data$dList$dLazy$dTypes.$Step("Cons", a, b)))(Data$dList$dLazy$dTypes.nil)(xs)));
const replicateM = dictMonad => {
  const $0 = dictMonad.Applicative0();
  const $1 = dictMonad.Bind1();
  return n => m => {
    if (n < 1) { return $0.pure(Data$dList$dLazy$dTypes.nil); }
    return $1.bind(m)(a => $1.bind(replicateM(dictMonad)(n - 1 | 0)(m))(as => $0.pure(Data$dLazy.defer(v => Data$dList$dLazy$dTypes.$Step("Cons", a, as)))));
  };
};
const repeat = x => {
  const go$lazy = $runtime.binding(() => Data$dLazy.defer(x$1 => Data$dLazy.force(Data$dLazy.defer(v => Data$dList$dLazy$dTypes.$Step("Cons", x, go$lazy())))));
  const go = go$lazy();
  return go;
};
const replicate = i => xs => take(i)(repeat(xs));
const range = start => end => {
  if (start > end) {
    return Data$dList$dLazy$dTypes.unfoldableList.unfoldr(x => {
      if (x >= end) { return Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple(x, x - 1 | 0)); }
      return Data$dMaybe.Nothing;
    })(start);
  }
  return Data$dList$dLazy$dTypes.unfoldableList.unfoldr(x => {
    if (x <= end) { return Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple(x, x + 1 | 0)); }
    return Data$dMaybe.Nothing;
  })(start);
};
const partition = f => Data$dList$dLazy$dTypes.foldableList.foldr(x => v => {
  if (f(x)) { return {yes: Data$dLazy.defer(v$1 => Data$dList$dLazy$dTypes.$Step("Cons", x, v.yes)), no: v.no}; }
  return {yes: v.yes, no: Data$dLazy.defer(v$1 => Data$dList$dLazy$dTypes.$Step("Cons", x, v.no))};
})({yes: Data$dList$dLazy$dTypes.nil, no: Data$dList$dLazy$dTypes.nil});
const $$null = x => {
  const $0 = uncons(x);
  if ($0.tag === "Nothing") { return true; }
  if ($0.tag === "Just") { return false; }
  $runtime.fail();
};
const nubBy = p => {
  const goStep = v => v1 => {
    if (v1.tag === "Nil") { return Data$dList$dLazy$dTypes.Nil; }
    if (v1.tag === "Cons") {
      const v2 = Data$dList$dInternal.insertAndLookupBy(p)(v1._1)(v);
      if (v2.found) { return Data$dLazy.force(go(v2.result)(v1._2)); }
      return Data$dList$dLazy$dTypes.$Step("Cons", v1._1, go(v2.result)(v1._2));
    }
    $runtime.fail();
  };
  const go = s => v => {
    const $0 = goStep(s);
    return Data$dLazy.defer(v$1 => $0(Data$dLazy.force(v)));
  };
  return go(Data$dList$dInternal.Leaf);
};
const nub = dictOrd => nubBy(dictOrd.compare);
const newtypePattern = {Coercible0: () => {}};
const mapMaybe = f => {
  const go = go$a0$copy => {
    let go$a0 = go$a0$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0;
      if (v.tag === "Nil") {
        go$c = false;
        go$r = Data$dList$dLazy$dTypes.Nil;
        continue;
      }
      if (v.tag === "Cons") {
        const v1 = f(v._1);
        if (v1.tag === "Nothing") {
          go$a0 = Data$dLazy.force(v._2);
          continue;
        }
        if (v1.tag === "Just") {
          go$c = false;
          go$r = Data$dList$dLazy$dTypes.$Step("Cons", v1._1, mapMaybe(f)(v._2));
          continue;
        }
      }
      $runtime.fail();
    }
    return go$r;
  };
  return x => Data$dLazy.defer(v => go(Data$dLazy.force(x)));
};
const some = dictAlternative => dictLazy => v => dictAlternative.Applicative0().Apply0().apply(dictAlternative.Plus1().Alt0().Functor0().map(Data$dList$dLazy$dTypes.cons)(v))(dictLazy.defer(v1 => many(dictAlternative)(dictLazy)(v)));
const many = dictAlternative => dictLazy => v => dictAlternative.Plus1().Alt0().alt(some(dictAlternative)(dictLazy)(v))(dictAlternative.Applicative0().pure(Data$dList$dLazy$dTypes.nil));
const length = /* #__PURE__ */ (() => Data$dList$dLazy$dTypes.foldableList.foldl(l => v => l + 1 | 0)(0))();
const last = /* #__PURE__ */ (() => {
  const go = go$a0$copy => {
    let go$a0 = go$a0$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0;
      if (v.tag === "Cons") {
        if (
          (() => {
            const $0 = uncons(v._2);
            if ($0.tag === "Nothing") { return true; }
            if ($0.tag === "Just") { return false; }
            $runtime.fail();
          })()
        ) {
          go$c = false;
          go$r = Data$dMaybe.$Maybe("Just", v._1);
          continue;
        }
        go$a0 = Data$dLazy.force(v._2);
        continue;
      }
      go$c = false;
      go$r = Data$dMaybe.Nothing;
    }
    return go$r;
  };
  return x => go(Data$dLazy.force(x));
})();
const iterate = f => x => {
  const go$lazy = $runtime.binding(() => Data$dLazy.defer(x$1 => Data$dLazy.force((() => {
    const $0 = Data$dList$dLazy$dTypes.functorList.map(f)(go$lazy());
    return Data$dLazy.defer(v => Data$dList$dLazy$dTypes.$Step("Cons", x, $0));
  })())));
  const go = go$lazy();
  return go;
};
const insertAt = v => v1 => v2 => {
  if (v === 0) { return Data$dLazy.defer(v$1 => Data$dList$dLazy$dTypes.$Step("Cons", v1, v2)); }
  return Data$dLazy.defer(v$1 => {
    const $0 = Data$dLazy.force(v2);
    if ($0.tag === "Nil") { return Data$dList$dLazy$dTypes.$Step("Cons", v1, Data$dList$dLazy$dTypes.nil); }
    if ($0.tag === "Cons") { return Data$dList$dLazy$dTypes.$Step("Cons", $0._1, insertAt(v - 1 | 0)(v1)($0._2)); }
    $runtime.fail();
  });
};
const init = /* #__PURE__ */ (() => {
  const go = v => {
    if (v.tag === "Cons") {
      if (
        (() => {
          const $0 = uncons(v._2);
          if ($0.tag === "Nothing") { return true; }
          if ($0.tag === "Just") { return false; }
          $runtime.fail();
        })()
      ) {
        return Data$dMaybe.$Maybe("Just", Data$dList$dLazy$dTypes.nil);
      }
      const $0 = go(Data$dLazy.force(v._2));
      if ($0.tag === "Just") {
        return Data$dMaybe.$Maybe(
          "Just",
          (() => {
            const $1 = v._1;
            const $2 = $0._1;
            return Data$dLazy.defer(v$1 => Data$dList$dLazy$dTypes.$Step("Cons", $1, $2));
          })()
        );
      }
    }
    return Data$dMaybe.Nothing;
  };
  return x => go(Data$dLazy.force(x));
})();
const index = xs => {
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
        if (v1 === 0) {
          go$c = false;
          go$r = Data$dMaybe.$Maybe("Just", v._1);
          continue;
        }
        go$a0 = Data$dLazy.force(v._2);
        go$a1 = v1 - 1 | 0;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go(Data$dLazy.force(xs));
};
const head = xs => {
  const $0 = uncons(xs);
  if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", $0._1.head); }
  return Data$dMaybe.Nothing;
};
const transpose = xs => {
  const v = uncons(xs);
  if (v.tag === "Nothing") { return xs; }
  if (v.tag === "Just") {
    const v1 = uncons(v._1.head);
    if (v1.tag === "Nothing") { return transpose(v._1.tail); }
    if (v1.tag === "Just") {
      const $0 = v1._1.head;
      const $1 = v1._1.tail;
      const $2 = mapMaybe(head)(v._1.tail);
      const $3 = Data$dLazy.defer(v$1 => Data$dList$dLazy$dTypes.$Step("Cons", $0, $2));
      const $4 = transpose((() => {
        const $4 = mapMaybe(tail)(v._1.tail);
        return Data$dLazy.defer(v$1 => Data$dList$dLazy$dTypes.$Step("Cons", $1, $4));
      })());
      return Data$dLazy.defer(v$1 => Data$dList$dLazy$dTypes.$Step("Cons", $3, $4));
    }
  }
  $runtime.fail();
};
const groupBy = eq => Data$dLazy.functorLazy.map(v => {
  if (v.tag === "Nil") { return Data$dList$dLazy$dTypes.Nil; }
  if (v.tag === "Cons") {
    const $0 = v._1;
    const v1 = span(eq($0))(v._2);
    const $1 = v1.init;
    return Data$dList$dLazy$dTypes.$Step("Cons", Data$dLazy.defer(v2 => Data$dNonEmpty.$NonEmpty($0, $1)), groupBy(eq)(v1.rest));
  }
  $runtime.fail();
});
const group = dictEq => groupBy(dictEq.eq);
const insertBy = cmp => x => xs => Data$dLazy.defer(v => {
  const $0 = Data$dLazy.force(xs);
  if ($0.tag === "Nil") { return Data$dList$dLazy$dTypes.$Step("Cons", x, Data$dList$dLazy$dTypes.nil); }
  if ($0.tag === "Cons") {
    if (cmp(x)($0._1) === "GT") { return Data$dList$dLazy$dTypes.$Step("Cons", $0._1, insertBy(cmp)(x)($0._2)); }
    return Data$dList$dLazy$dTypes.$Step("Cons", x, Data$dLazy.defer(v$1 => $0));
  }
  $runtime.fail();
});
const insert = dictOrd => insertBy(dictOrd.compare);
const fromFoldable = dictFoldable => dictFoldable.foldr(Data$dList$dLazy$dTypes.cons)(Data$dList$dLazy$dTypes.nil);
const foldrLazy = dictLazy => op => z => {
  const go = xs => {
    const v = Data$dLazy.force(xs);
    if (v.tag === "Cons") {
      const $0 = v._1;
      const $1 = v._2;
      return dictLazy.defer(v1 => op($0)(go($1)));
    }
    if (v.tag === "Nil") { return z; }
    $runtime.fail();
  };
  return go;
};
const foldM = dictMonad => f => b => xs => {
  const v = uncons(xs);
  if (v.tag === "Nothing") { return dictMonad.Applicative0().pure(b); }
  if (v.tag === "Just") {
    const $0 = v._1.tail;
    return dictMonad.Bind1().bind(f(b)(v._1.head))(b$p => foldM(dictMonad)(f)(b$p)($0));
  }
  $runtime.fail();
};
const findIndex = fn => {
  const go = go$a0$copy => go$a1$copy => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
    while (go$c) {
      const n = go$a0, list = go$a1;
      const $0 = uncons(list);
      if ($0.tag === "Just") {
        if (fn($0._1.head)) {
          go$c = false;
          go$r = Data$dMaybe.$Maybe("Just", n);
          continue;
        }
        go$a0 = n + 1 | 0;
        go$a1 = $0._1.tail;
        continue;
      }
      if ($0.tag === "Nothing") {
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
  const $0 = findIndex(fn)(reverse(xs));
  if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", (length(xs) - 1 | 0) - $0._1 | 0); }
  return Data$dMaybe.Nothing;
};
const filterM = dictMonad => {
  const $0 = dictMonad.Applicative0();
  const $1 = dictMonad.Bind1();
  return p => list => {
    const v = uncons(list);
    if (v.tag === "Nothing") { return $0.pure(Data$dList$dLazy$dTypes.nil); }
    if (v.tag === "Just") {
      const $2 = v._1.head;
      const $3 = v._1.tail;
      return $1.bind(p($2))(b => $1.bind(filterM(dictMonad)(p)($3))(xs$p => $0.pure(b ? Data$dLazy.defer(v$1 => Data$dList$dLazy$dTypes.$Step("Cons", $2, xs$p)) : xs$p)));
    }
    $runtime.fail();
  };
};
const filter = p => {
  const go = go$a0$copy => {
    let go$a0 = go$a0$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0;
      if (v.tag === "Nil") {
        go$c = false;
        go$r = Data$dList$dLazy$dTypes.Nil;
        continue;
      }
      if (v.tag === "Cons") {
        if (p(v._1)) {
          go$c = false;
          go$r = Data$dList$dLazy$dTypes.$Step("Cons", v._1, filter(p)(v._2));
          continue;
        }
        go$a0 = Data$dLazy.force(v._2);
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return x => Data$dLazy.defer(v => go(Data$dLazy.force(x)));
};
const intersectBy = eq => xs => ys => filter(x => any(eq(x))(ys))(xs);
const intersect = dictEq => intersectBy(dictEq.eq);
const nubByEq = eq => Data$dLazy.functorLazy.map(v => {
  if (v.tag === "Nil") { return Data$dList$dLazy$dTypes.Nil; }
  if (v.tag === "Cons") {
    const $0 = v._1;
    return Data$dList$dLazy$dTypes.$Step("Cons", $0, nubByEq(eq)(filter(y => !eq($0)(y))(v._2)));
  }
  $runtime.fail();
});
const nubEq = dictEq => nubByEq(dictEq.eq);
const eqPattern = dictEq => ({eq: x => y => Data$dList$dLazy$dTypes.eq1List.eq1(dictEq)(x)(y)});
const ordPattern = dictOrd => {
  const $0 = dictOrd.Eq0();
  const eqPattern1 = {eq: x => y => Data$dList$dLazy$dTypes.eq1List.eq1($0)(x)(y)};
  return {compare: x => y => Data$dList$dLazy$dTypes.ordList(dictOrd).compare(x)(y), Eq0: () => eqPattern1};
};
const elemLastIndex = dictEq => x => findLastIndex(v => dictEq.eq(v)(x));
const elemIndex = dictEq => x => findIndex(v => dictEq.eq(v)(x));
const dropWhile = p => {
  const go = go$a0$copy => {
    let go$a0 = go$a0$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0;
      if (v.tag === "Cons" && p(v._1)) {
        go$a0 = Data$dLazy.force(v._2);
        continue;
      }
      go$c = false;
      go$r = Data$dLazy.defer(v$1 => v);
    }
    return go$r;
  };
  return x => go(Data$dLazy.force(x));
};
const drop = n => {
  const go = go$a0$copy => go$a1$copy => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
    while (go$c) {
      const v = go$a0, v1 = go$a1;
      if (v === 0) {
        go$c = false;
        go$r = v1;
        continue;
      }
      if (v1.tag === "Nil") {
        go$c = false;
        go$r = Data$dList$dLazy$dTypes.Nil;
        continue;
      }
      if (v1.tag === "Cons") {
        go$a0 = v - 1 | 0;
        go$a1 = Data$dLazy.force(v1._2);
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return Data$dLazy.functorLazy.map(go(n));
};
const slice = start => end => xs => take(end - start | 0)(drop(start)(xs));
const deleteBy = eq => x => xs => Data$dLazy.defer(v => {
  const $0 = Data$dLazy.force(xs);
  if ($0.tag === "Nil") { return Data$dList$dLazy$dTypes.Nil; }
  if ($0.tag === "Cons") {
    if (eq(x)($0._1)) { return Data$dLazy.force($0._2); }
    return Data$dList$dLazy$dTypes.$Step("Cons", $0._1, deleteBy(eq)(x)($0._2));
  }
  $runtime.fail();
});
const unionBy = eq => xs => ys => Data$dList$dLazy$dTypes.semigroupList.append(xs)(Data$dList$dLazy$dTypes.foldableList.foldl(b => a => deleteBy(eq)(a)(b))(nubByEq(eq)(ys))(xs));
const union = dictEq => unionBy(dictEq.eq);
const deleteAt = n => xs => Data$dLazy.defer(v => {
  const $0 = Data$dLazy.force(xs);
  if ($0.tag === "Nil") { return Data$dList$dLazy$dTypes.Nil; }
  if ($0.tag === "Cons") {
    if (n === 0) { return Data$dLazy.force($0._2); }
    return Data$dList$dLazy$dTypes.$Step("Cons", $0._1, deleteAt(n - 1 | 0)($0._2));
  }
  $runtime.fail();
});
const $$delete = dictEq => deleteBy(dictEq.eq);
const difference = dictEq => Data$dList$dLazy$dTypes.foldableList.foldl(b => a => deleteBy(dictEq.eq)(a)(b));
const cycle = xs => {
  const go$lazy = $runtime.binding(() => Data$dLazy.defer(x => Data$dLazy.force(Data$dList$dLazy$dTypes.semigroupList.append(xs)(go$lazy()))));
  const go = go$lazy();
  return go;
};
const concatMap = b => a => Data$dList$dLazy$dTypes.bindList.bind(a)(b);
const concat = v => Data$dList$dLazy$dTypes.bindList.bind(v)(identity);
const catMaybes = /* #__PURE__ */ mapMaybe(identity);
const alterAt = n => f => xs => Data$dLazy.defer(v => {
  const $0 = Data$dLazy.force(xs);
  if ($0.tag === "Nil") { return Data$dList$dLazy$dTypes.Nil; }
  if ($0.tag === "Cons") {
    if (n === 0) {
      const v2 = f($0._1);
      if (v2.tag === "Nothing") { return Data$dLazy.force($0._2); }
      if (v2.tag === "Just") { return Data$dList$dLazy$dTypes.$Step("Cons", v2._1, $0._2); }
      $runtime.fail();
    }
    return Data$dList$dLazy$dTypes.$Step("Cons", $0._1, alterAt(n - 1 | 0)(f)($0._2));
  }
  $runtime.fail();
});
const modifyAt = n => f => alterAt(n)(x => Data$dMaybe.$Maybe("Just", f(x)));
export {
  Pattern,
  alterAt,
  any,
  catMaybes,
  concat,
  concatMap,
  cycle,
  $$delete as delete,
  deleteAt,
  deleteBy,
  difference,
  drop,
  dropWhile,
  elemIndex,
  elemLastIndex,
  eqPattern,
  filter,
  filterM,
  findIndex,
  findLastIndex,
  foldM,
  foldrLazy,
  fromFoldable,
  group,
  groupBy,
  head,
  identity,
  index,
  init,
  insert,
  insertAt,
  insertBy,
  intersect,
  intersectBy,
  iterate,
  last,
  length,
  many,
  mapMaybe,
  modifyAt,
  newtypePattern,
  nub,
  nubBy,
  nubByEq,
  nubEq,
  $$null as null,
  ordPattern,
  partition,
  range,
  repeat,
  replicate,
  replicateM,
  reverse,
  scanlLazy,
  showPattern,
  singleton,
  slice,
  snoc,
  some,
  span,
  stripPrefix,
  tail,
  take,
  takeWhile,
  toUnfoldable,
  transpose,
  uncons,
  union,
  unionBy,
  unzip,
  updateAt,
  zip,
  zipWith,
  zipWithA
};
