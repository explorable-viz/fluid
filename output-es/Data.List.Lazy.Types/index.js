import * as $runtime from "../runtime.js";
import * as Data$dLazy from "../Data.Lazy/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNonEmpty from "../Data.NonEmpty/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const $Step = (tag, _1, _2) => ({tag, _1, _2});
const identity = x => x;
const List = x => x;
const Nil = /* #__PURE__ */ $Step("Nil");
const Cons = value0 => value1 => $Step("Cons", value0, value1);
const NonEmptyList = x => x;
const nil = /* #__PURE__ */ Data$dLazy.defer(v => Nil);
const newtypeNonEmptyList = {Coercible0: () => {}};
const newtypeList = {Coercible0: () => {}};
const step = x => Data$dLazy.force(x);
const semigroupList = {
  append: xs => ys => Data$dLazy.defer(v => {
    const $0 = Data$dLazy.force(xs);
    if ($0.tag === "Nil") { return Data$dLazy.force(ys); }
    if ($0.tag === "Cons") { return $Step("Cons", $0._1, semigroupList.append($0._2)(ys)); }
    $runtime.fail();
  })
};
const monoidList = {mempty: nil, Semigroup0: () => semigroupList};
const lazyList = {defer: f => Data$dLazy.defer(x => Data$dLazy.force(f(x)))};
const functorList = {
  map: f => xs => Data$dLazy.defer(v => {
    const $0 = Data$dLazy.force(xs);
    if ($0.tag === "Nil") { return Nil; }
    if ($0.tag === "Cons") { return $Step("Cons", f($0._1), functorList.map(f)($0._2)); }
    $runtime.fail();
  })
};
const functorNonEmptyList = {
  map: f => v => Data$dLazy.defer(v$1 => {
    const $0 = Data$dLazy.force(v);
    return Data$dNonEmpty.$NonEmpty(f($0._1), functorList.map(f)($0._2));
  })
};
const eq1List = {
  eq1: dictEq => xs => ys => {
    const go = v => v1 => {
      if (v.tag === "Nil") { return v1.tag === "Nil"; }
      return v.tag === "Cons" && v1.tag === "Cons" && dictEq.eq(v._1)(v1._1) && go(Data$dLazy.force(v._2))(Data$dLazy.force(v1._2));
    };
    return go(Data$dLazy.force(xs))(Data$dLazy.force(ys));
  }
};
const eqNonEmpty = dictEq => ({eq: x => y => dictEq.eq(x._1)(y._1) && eq1List.eq1(dictEq)(x._2)(y._2)});
const eq1NonEmptyList = {eq1: dictEq => Data$dLazy.eq1Lazy.eq1(eqNonEmpty(dictEq))};
const eqList = dictEq => ({eq: eq1List.eq1(dictEq)});
const eqNonEmptyList = dictEq => {
  const $0 = eqNonEmpty(dictEq);
  return {eq: x => y => $0.eq(Data$dLazy.force(x))(Data$dLazy.force(y))};
};
const ord1List = {
  compare1: dictOrd => xs => ys => {
    const go = go$a0$copy => go$a1$copy => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const v = go$a0, v1 = go$a1;
        if (v.tag === "Nil") {
          if (v1.tag === "Nil") {
            go$c = false;
            go$r = Data$dOrdering.EQ;
            continue;
          }
          go$c = false;
          go$r = Data$dOrdering.LT;
          continue;
        }
        if (v1.tag === "Nil") {
          go$c = false;
          go$r = Data$dOrdering.GT;
          continue;
        }
        if (v.tag === "Cons" && v1.tag === "Cons") {
          const v2 = dictOrd.compare(v._1)(v1._1);
          if (v2 === "EQ") {
            go$a0 = Data$dLazy.force(v._2);
            go$a1 = Data$dLazy.force(v1._2);
            continue;
          }
          go$c = false;
          go$r = v2;
          continue;
        }
        $runtime.fail();
      }
      return go$r;
    };
    return go(Data$dLazy.force(xs))(Data$dLazy.force(ys));
  },
  Eq10: () => eq1List
};
const ordNonEmpty = /* #__PURE__ */ Data$dNonEmpty.ordNonEmpty(ord1List);
const ord1NonEmptyList = {compare1: dictOrd => Data$dLazy.ordLazy(ordNonEmpty(dictOrd)).compare, Eq10: () => eq1NonEmptyList};
const ordList = dictOrd => ({compare: ord1List.compare1(dictOrd), Eq0: () => ({eq: eq1List.eq1(dictOrd.Eq0())})});
const ordNonEmptyList = dictOrd => Data$dLazy.ordLazy(ordNonEmpty(dictOrd));
const cons = x => xs => Data$dLazy.defer(v => $Step("Cons", x, xs));
const foldableList = {
  foldr: op => z => xs => foldableList.foldl(b => a => op(a)(b))(z)(foldableList.foldl(b => a => Data$dLazy.defer(v => $Step("Cons", a, b)))(nil)(xs)),
  foldl: op => {
    const go = go$a0$copy => go$a1$copy => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const b = go$a0, xs = go$a1;
        const v = Data$dLazy.force(xs);
        if (v.tag === "Nil") {
          go$c = false;
          go$r = b;
          continue;
        }
        if (v.tag === "Cons") {
          go$a0 = op(b)(v._1);
          go$a1 = v._2;
          continue;
        }
        $runtime.fail();
      }
      return go$r;
    };
    return go;
  },
  foldMap: dictMonoid => {
    const mempty = dictMonoid.mempty;
    return f => foldableList.foldl(b => a => dictMonoid.Semigroup0().append(b)(f(a)))(mempty);
  }
};
const foldableNonEmpty = {
  foldMap: dictMonoid => {
    const foldMap1 = foldableList.foldMap(dictMonoid);
    return f => v => dictMonoid.Semigroup0().append(f(v._1))(foldMap1(f)(v._2));
  },
  foldl: f => b => v => foldableList.foldl(f)(f(b)(v._1))(v._2),
  foldr: f => b => v => f(v._1)(foldableList.foldr(f)(b)(v._2))
};
const extendList = {
  extend: f => l => {
    const v = Data$dLazy.force(l);
    if (v.tag === "Nil") { return nil; }
    if (v.tag === "Cons") {
      const $0 = f(l);
      const $1 = foldableList.foldr(a => v$1 => {
        const $1 = v$1.acc;
        const $2 = v$1.val;
        const acc$p = Data$dLazy.defer(v$2 => $Step("Cons", a, $1));
        return {
          val: (() => {
            const $3 = f(acc$p);
            return Data$dLazy.defer(v$2 => $Step("Cons", $3, $2));
          })(),
          acc: acc$p
        };
      })({val: nil, acc: nil})(v._2).val;
      return Data$dLazy.defer(v$1 => $Step("Cons", $0, $1));
    }
    $runtime.fail();
  },
  Functor0: () => functorList
};
const extendNonEmptyList = {
  extend: f => v => {
    const $0 = Data$dLazy.force(v)._2;
    return Data$dLazy.defer(v2 => Data$dNonEmpty.$NonEmpty(
      f(v),
      foldableList.foldr(a => v1 => {
        const $1 = v1.acc;
        const $2 = v1.val;
        return {
          val: (() => {
            const $3 = f(Data$dLazy.defer(v2$1 => Data$dNonEmpty.$NonEmpty(a, $1)));
            return Data$dLazy.defer(v$1 => $Step("Cons", $3, $2));
          })(),
          acc: Data$dLazy.defer(v$1 => $Step("Cons", a, $1))
        };
      })({val: nil, acc: nil})($0).val
    ));
  },
  Functor0: () => functorNonEmptyList
};
const foldableNonEmptyList = {
  foldr: f => b => v => {
    const $0 = Data$dLazy.force(v);
    return f($0._1)(foldableList.foldr(f)(b)($0._2));
  },
  foldl: f => b => v => {
    const $0 = Data$dLazy.force(v);
    return foldableList.foldl(f)(f(b)($0._1))($0._2);
  },
  foldMap: dictMonoid => {
    const foldMap1 = foldableNonEmpty.foldMap(dictMonoid);
    return f => v => foldMap1(f)(Data$dLazy.force(v));
  }
};
const showList = dictShow => (
  {
    show: xs => {
      const v = Data$dLazy.force(xs);
      if (v.tag === "Nil") { return "(fromFoldable [])"; }
      if (v.tag === "Cons") { return "(fromFoldable [" + dictShow.show(v._1) + foldableList.foldl(shown => x$p => shown + "," + dictShow.show(x$p))("")(v._2) + "])"; }
      $runtime.fail();
    }
  }
);
const showNonEmptyList = dictShow => {
  const $0 = showList(dictShow);
  return {
    show: v => {
      const $1 = Data$dLazy.force(v);
      return "(NonEmptyList (defer \\_ -> (NonEmpty " + dictShow.show($1._1) + " " + $0.show($1._2) + ")))";
    }
  };
};
const showStep = dictShow => (
  {
    show: v => {
      if (v.tag === "Nil") { return "Nil"; }
      if (v.tag === "Cons") { return "(" + dictShow.show(v._1) + " : " + showList(dictShow).show(v._2) + ")"; }
      $runtime.fail();
    }
  }
);
const foldableWithIndexList = {
  foldrWithIndex: f => b => xs => {
    const v = foldableList.foldl(v1 => {
      const $0 = v1._2;
      const $1 = v1._1;
      return a => Data$dTuple.$Tuple($1 + 1 | 0, Data$dLazy.defer(v => $Step("Cons", a, $0)));
    })(Data$dTuple.$Tuple(0, nil))(xs);
    return foldableList.foldl(v1 => {
      const $0 = v1._2;
      const $1 = v1._1;
      return a => Data$dTuple.$Tuple($1 - 1 | 0, f($1 - 1 | 0)(a)($0));
    })(Data$dTuple.$Tuple(v._1, b))(v._2)._2;
  },
  foldlWithIndex: f => acc => {
    const $0 = foldableList.foldl(v => {
      const $0 = v._2;
      const $1 = v._1;
      return a => Data$dTuple.$Tuple($1 + 1 | 0, f($1)($0)(a));
    })(Data$dTuple.$Tuple(0, acc));
    return x => $0(x)._2;
  },
  foldMapWithIndex: dictMonoid => {
    const mempty = dictMonoid.mempty;
    return f => foldableWithIndexList.foldlWithIndex(i => acc => {
      const $0 = dictMonoid.Semigroup0().append(acc);
      const $1 = f(i);
      return x => $0($1(x));
    })(mempty);
  },
  Foldable0: () => foldableList
};
const foldableWithIndexNonEmpty = /* #__PURE__ */ Data$dNonEmpty.foldableWithIndexNonEmpty(foldableWithIndexList);
const foldableWithIndexNonEmptyList = {
  foldMapWithIndex: dictMonoid => {
    const foldMapWithIndex1 = foldableWithIndexNonEmpty.foldMapWithIndex(dictMonoid);
    return f => v => foldMapWithIndex1(x => f((() => {
      if (x.tag === "Nothing") { return 0; }
      if (x.tag === "Just") { return 1 + x._1 | 0; }
      $runtime.fail();
    })()))(Data$dLazy.force(v));
  },
  foldlWithIndex: f => b => v => foldableWithIndexNonEmpty.foldlWithIndex(x => f((() => {
    if (x.tag === "Nothing") { return 0; }
    if (x.tag === "Just") { return 1 + x._1 | 0; }
    $runtime.fail();
  })()))(b)(Data$dLazy.force(v)),
  foldrWithIndex: f => b => v => foldableWithIndexNonEmpty.foldrWithIndex(x => f((() => {
    if (x.tag === "Nothing") { return 0; }
    if (x.tag === "Just") { return 1 + x._1 | 0; }
    $runtime.fail();
  })()))(b)(Data$dLazy.force(v)),
  Foldable0: () => foldableNonEmptyList
};
const functorWithIndexList = {
  mapWithIndex: f => foldableWithIndexList.foldrWithIndex(i => x => acc => {
    const $0 = f(i)(x);
    return Data$dLazy.defer(v => $Step("Cons", $0, acc));
  })(nil),
  Functor0: () => functorList
};
const mapWithIndex = f => v => Data$dNonEmpty.$NonEmpty(f(Data$dMaybe.Nothing)(v._1), functorWithIndexList.mapWithIndex(x => f(Data$dMaybe.$Maybe("Just", x)))(v._2));
const functorWithIndexNonEmptyList = {
  mapWithIndex: f => v => Data$dLazy.defer(v1 => mapWithIndex(x => f((() => {
    if (x.tag === "Nothing") { return 0; }
    if (x.tag === "Just") { return 1 + x._1 | 0; }
    $runtime.fail();
  })()))(Data$dLazy.force(v))),
  Functor0: () => functorNonEmptyList
};
const toList = v => Data$dLazy.defer(x => Data$dLazy.force((() => {
  const v2 = Data$dLazy.force(v);
  const $0 = v2._1;
  const $1 = v2._2;
  return Data$dLazy.defer(v$1 => $Step("Cons", $0, $1));
})()));
const semigroupNonEmptyList = {
  append: v => as$p => {
    const v1 = Data$dLazy.force(v);
    const $0 = v1._1;
    const $1 = v1._2;
    return Data$dLazy.defer(v2 => Data$dNonEmpty.$NonEmpty($0, semigroupList.append($1)(toList(as$p))));
  }
};
const traversableList = {
  traverse: dictApplicative => {
    const Apply0 = dictApplicative.Apply0();
    return f => foldableList.foldr(a => b => Apply0.apply(Apply0.Functor0().map(cons)(f(a)))(b))(dictApplicative.pure(nil));
  },
  sequence: dictApplicative => traversableList.traverse(dictApplicative)(identity),
  Functor0: () => functorList,
  Foldable1: () => foldableList
};
const traversableNonEmpty = /* #__PURE__ */ Data$dNonEmpty.traversableNonEmpty(traversableList);
const traversableNonEmptyList = {
  traverse: dictApplicative => {
    const traverse1 = traversableNonEmpty.traverse(dictApplicative);
    return f => v => dictApplicative.Apply0().Functor0().map(xxs => Data$dLazy.defer(v1 => xxs))(traverse1(f)(Data$dLazy.force(v)));
  },
  sequence: dictApplicative => {
    const sequence1 = traversableNonEmpty.sequence(dictApplicative);
    return v => dictApplicative.Apply0().Functor0().map(xxs => Data$dLazy.defer(v1 => xxs))(sequence1(Data$dLazy.force(v)));
  },
  Functor0: () => functorNonEmptyList,
  Foldable1: () => foldableNonEmptyList
};
const traversableWithIndexList = {
  traverseWithIndex: dictApplicative => {
    const Apply0 = dictApplicative.Apply0();
    return f => foldableWithIndexList.foldrWithIndex(i => a => b => Apply0.apply(Apply0.Functor0().map(cons)(f(i)(a)))(b))(dictApplicative.pure(nil));
  },
  FunctorWithIndex0: () => functorWithIndexList,
  FoldableWithIndex1: () => foldableWithIndexList,
  Traversable2: () => traversableList
};
const traverseWithIndex = /* #__PURE__ */ (() => Data$dNonEmpty.traversableWithIndexNonEmpty(traversableWithIndexList).traverseWithIndex)();
const traversableWithIndexNonEmptyList = {
  traverseWithIndex: dictApplicative => {
    const traverseWithIndex1 = traverseWithIndex(dictApplicative);
    return f => v => dictApplicative.Apply0().Functor0().map(xxs => Data$dLazy.defer(v1 => xxs))(traverseWithIndex1(x => f((() => {
      if (x.tag === "Nothing") { return 0; }
      if (x.tag === "Just") { return 1 + x._1 | 0; }
      $runtime.fail();
    })()))(Data$dLazy.force(v)));
  },
  FunctorWithIndex0: () => functorWithIndexNonEmptyList,
  FoldableWithIndex1: () => foldableWithIndexNonEmptyList,
  Traversable2: () => traversableNonEmptyList
};
const unfoldable1List = {
  unfoldr1: /* #__PURE__ */ (() => {
    const go = f => b => Data$dLazy.defer(x => Data$dLazy.force((() => {
      const v1 = f(b);
      if (v1._2.tag === "Just") {
        const $0 = v1._1;
        const $1 = go(f)(v1._2._1);
        return Data$dLazy.defer(v => $Step("Cons", $0, $1));
      }
      if (v1._2.tag === "Nothing") {
        const $0 = v1._1;
        return Data$dLazy.defer(v => $Step("Cons", $0, nil));
      }
      $runtime.fail();
    })()));
    return go;
  })()
};
const unfoldableList = {
  unfoldr: /* #__PURE__ */ (() => {
    const go = f => b => Data$dLazy.defer(x => Data$dLazy.force((() => {
      const v1 = f(b);
      if (v1.tag === "Nothing") { return nil; }
      if (v1.tag === "Just") {
        const $0 = v1._1._1;
        const $1 = go(f)(v1._1._2);
        return Data$dLazy.defer(v => $Step("Cons", $0, $1));
      }
      $runtime.fail();
    })()));
    return go;
  })(),
  Unfoldable10: () => unfoldable1List
};
const unfoldr1 = f => b => {
  const $0 = f(b);
  return Data$dNonEmpty.$NonEmpty(
    $0._1,
    unfoldableList.unfoldr(v1 => {
      if (v1.tag === "Just") { return Data$dMaybe.$Maybe("Just", f(v1._1)); }
      return Data$dMaybe.Nothing;
    })($0._2)
  );
};
const unfoldable1NonEmptyList = {unfoldr1: f => b => Data$dLazy.defer(v => unfoldr1(f)(b))};
const comonadNonEmptyList = {extract: v => Data$dLazy.force(v)._1, Extend0: () => extendNonEmptyList};
const monadList = {Applicative0: () => applicativeList, Bind1: () => bindList};
const bindList = {
  bind: xs => f => Data$dLazy.defer(v => {
    const $0 = Data$dLazy.force(xs);
    if ($0.tag === "Nil") { return Nil; }
    if ($0.tag === "Cons") { return Data$dLazy.force(semigroupList.append(f($0._1))(bindList.bind($0._2)(f))); }
    $runtime.fail();
  }),
  Apply0: () => applyList
};
const applyList = {apply: f => a => bindList.bind(f)(f$p => bindList.bind(a)(a$p => applicativeList.pure(f$p(a$p)))), Functor0: () => functorList};
const applicativeList = {pure: a => Data$dLazy.defer(v => $Step("Cons", a, nil)), Apply0: () => applyList};
const applyNonEmptyList = {
  apply: v => v1 => {
    const v2 = Data$dLazy.force(v1);
    const v3 = Data$dLazy.force(v);
    const $0 = v2._1;
    const $1 = v2._2;
    const $2 = v3._1;
    const $3 = v3._2;
    return Data$dLazy.defer(v4 => Data$dNonEmpty.$NonEmpty(
      $2($0),
      semigroupList.append(applyList.apply($3)(Data$dLazy.defer(v$1 => $Step("Cons", $0, nil))))(applyList.apply(Data$dLazy.defer(v$1 => $Step("Cons", $2, $3)))($1))
    ));
  },
  Functor0: () => functorNonEmptyList
};
const bindNonEmptyList = {
  bind: v => f => {
    const v1 = Data$dLazy.force(v);
    const $0 = v1._2;
    const v2 = Data$dLazy.force(f(v1._1));
    const $1 = v2._1;
    const $2 = v2._2;
    return Data$dLazy.defer(v3 => Data$dNonEmpty.$NonEmpty($1, semigroupList.append($2)(bindList.bind($0)(x => toList(f(x))))));
  },
  Apply0: () => applyNonEmptyList
};
const altNonEmptyList = /* #__PURE__ */ (() => ({alt: semigroupNonEmptyList.append, Functor0: () => functorNonEmptyList}))();
const altList = /* #__PURE__ */ (() => ({alt: semigroupList.append, Functor0: () => functorList}))();
const plusList = {empty: nil, Alt0: () => altList};
const alternativeList = {Applicative0: () => applicativeList, Plus1: () => plusList};
const monadPlusList = {Monad0: () => monadList, Alternative1: () => alternativeList};
const applicativeNonEmptyList = {pure: a => Data$dLazy.defer(v => Data$dNonEmpty.$NonEmpty(a, nil)), Apply0: () => applyNonEmptyList};
const monadNonEmptyList = {Applicative0: () => applicativeNonEmptyList, Bind1: () => bindNonEmptyList};
export {
  $Step,
  Cons,
  List,
  Nil,
  NonEmptyList,
  altList,
  altNonEmptyList,
  alternativeList,
  applicativeList,
  applicativeNonEmptyList,
  applyList,
  applyNonEmptyList,
  bindList,
  bindNonEmptyList,
  comonadNonEmptyList,
  cons,
  eq1List,
  eq1NonEmptyList,
  eqList,
  eqNonEmpty,
  eqNonEmptyList,
  extendList,
  extendNonEmptyList,
  foldableList,
  foldableNonEmpty,
  foldableNonEmptyList,
  foldableWithIndexList,
  foldableWithIndexNonEmpty,
  foldableWithIndexNonEmptyList,
  functorList,
  functorNonEmptyList,
  functorWithIndexList,
  functorWithIndexNonEmptyList,
  identity,
  lazyList,
  mapWithIndex,
  monadList,
  monadNonEmptyList,
  monadPlusList,
  monoidList,
  newtypeList,
  newtypeNonEmptyList,
  nil,
  ord1List,
  ord1NonEmptyList,
  ordList,
  ordNonEmpty,
  ordNonEmptyList,
  plusList,
  semigroupList,
  semigroupNonEmptyList,
  showList,
  showNonEmptyList,
  showStep,
  step,
  toList,
  traversableList,
  traversableNonEmpty,
  traversableNonEmptyList,
  traversableWithIndexList,
  traversableWithIndexNonEmptyList,
  traverseWithIndex,
  unfoldable1List,
  unfoldable1NonEmptyList,
  unfoldableList,
  unfoldr1
};
