import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNonEmpty from "../Data.NonEmpty/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const $List = (tag, _1, _2) => ({tag, _1, _2});
const identity = x => x;
const Nil = /* #__PURE__ */ $List("Nil");
const Cons = value0 => value1 => $List("Cons", value0, value1);
const NonEmptyList = x => x;
const toList = v => $List("Cons", v._1, v._2);
const newtypeNonEmptyList = {Coercible0: () => {}};
const nelCons = a => v => Data$dNonEmpty.$NonEmpty(a, $List("Cons", v._1, v._2));
const listMap = f => {
  const chunkedRevMap = chunkedRevMap$a0$copy => chunkedRevMap$a1$copy => {
    let chunkedRevMap$a0 = chunkedRevMap$a0$copy, chunkedRevMap$a1 = chunkedRevMap$a1$copy, chunkedRevMap$c = true, chunkedRevMap$r;
    while (chunkedRevMap$c) {
      const v = chunkedRevMap$a0, v1 = chunkedRevMap$a1;
      if (v1.tag === "Cons" && v1._2.tag === "Cons" && v1._2._2.tag === "Cons") {
        chunkedRevMap$a0 = $List("Cons", v1, v);
        chunkedRevMap$a1 = v1._2._2._2;
        continue;
      }
      const reverseUnrolledMap = reverseUnrolledMap$a0$copy => reverseUnrolledMap$a1$copy => {
        let reverseUnrolledMap$a0 = reverseUnrolledMap$a0$copy, reverseUnrolledMap$a1 = reverseUnrolledMap$a1$copy, reverseUnrolledMap$c = true, reverseUnrolledMap$r;
        while (reverseUnrolledMap$c) {
          const v2 = reverseUnrolledMap$a0, v3 = reverseUnrolledMap$a1;
          if (v2.tag === "Cons" && v2._1.tag === "Cons" && v2._1._2.tag === "Cons" && v2._1._2._2.tag === "Cons") {
            reverseUnrolledMap$a0 = v2._2;
            reverseUnrolledMap$a1 = $List("Cons", f(v2._1._1), $List("Cons", f(v2._1._2._1), $List("Cons", f(v2._1._2._2._1), v3)));
            continue;
          }
          reverseUnrolledMap$c = false;
          reverseUnrolledMap$r = v3;
        }
        return reverseUnrolledMap$r;
      };
      chunkedRevMap$c = false;
      chunkedRevMap$r = reverseUnrolledMap(v)((() => {
        if (v1.tag === "Cons") {
          if (v1._2.tag === "Cons") {
            if (v1._2._2.tag === "Nil") { return $List("Cons", f(v1._1), $List("Cons", f(v1._2._1), Nil)); }
            return Nil;
          }
          if (v1._2.tag === "Nil") { return $List("Cons", f(v1._1), Nil); }
        }
        return Nil;
      })());
    }
    return chunkedRevMap$r;
  };
  return chunkedRevMap(Nil);
};
const functorList = {map: listMap};
const functorNonEmptyList = {map: f => m => Data$dNonEmpty.$NonEmpty(f(m._1), listMap(f)(m._2))};
const foldableList = {
  foldr: f => b => {
    const $0 = foldableList.foldl(b$1 => a => f(a)(b$1))(b);
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
          go$a0 = $List("Cons", v1._1, v);
          go$a1 = v1._2;
          continue;
        }
        $runtime.fail();
      }
      return go$r;
    };
    const $1 = go(Nil);
    return x => $0($1(x));
  },
  foldl: f => {
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
    return go;
  },
  foldMap: dictMonoid => {
    const mempty = dictMonoid.mempty;
    return f => foldableList.foldl(acc => {
      const $0 = dictMonoid.Semigroup0().append(acc);
      return x => $0(f(x));
    })(mempty);
  }
};
const foldableNonEmptyList = {
  foldMap: dictMonoid => {
    const foldMap1 = foldableList.foldMap(dictMonoid);
    return f => v => dictMonoid.Semigroup0().append(f(v._1))(foldMap1(f)(v._2));
  },
  foldl: f => b => v => {
    const go = go$a0$copy => go$a1$copy => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const b$1 = go$a0, v$1 = go$a1;
        if (v$1.tag === "Nil") {
          go$c = false;
          go$r = b$1;
          continue;
        }
        if (v$1.tag === "Cons") {
          go$a0 = f(b$1)(v$1._1);
          go$a1 = v$1._2;
          continue;
        }
        $runtime.fail();
      }
      return go$r;
    };
    return go(f(b)(v._1))(v._2);
  },
  foldr: f => b => v => f(v._1)(foldableList.foldr(f)(b)(v._2))
};
const foldableWithIndexList = {
  foldrWithIndex: f => b => xs => {
    const go = go$a0$copy => go$a1$copy => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const b$1 = go$a0, v = go$a1;
        if (v.tag === "Nil") {
          go$c = false;
          go$r = b$1;
          continue;
        }
        if (v.tag === "Cons") {
          go$a0 = Data$dTuple.$Tuple(b$1._1 + 1 | 0, $List("Cons", v._1, b$1._2));
          go$a1 = v._2;
          continue;
        }
        $runtime.fail();
      }
      return go$r;
    };
    const v = go(Data$dTuple.$Tuple(0, Nil))(xs);
    const go$1 = go$1$a0$copy => go$1$a1$copy => {
      let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
      while (go$1$c) {
        const b$1 = go$1$a0, v$1 = go$1$a1;
        if (v$1.tag === "Nil") {
          go$1$c = false;
          go$1$r = b$1;
          continue;
        }
        if (v$1.tag === "Cons") {
          go$1$a0 = Data$dTuple.$Tuple(b$1._1 - 1 | 0, f(b$1._1 - 1 | 0)(v$1._1)(b$1._2));
          go$1$a1 = v$1._2;
          continue;
        }
        $runtime.fail();
      }
      return go$1$r;
    };
    return go$1(Data$dTuple.$Tuple(v._1, b))(v._2)._2;
  },
  foldlWithIndex: f => acc => {
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
          go$a0 = Data$dTuple.$Tuple(b._1 + 1 | 0, f(b._1)(b._2)(v._1));
          go$a1 = v._2;
          continue;
        }
        $runtime.fail();
      }
      return go$r;
    };
    const $0 = go(Data$dTuple.$Tuple(0, acc));
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
    })()))(v);
  },
  foldlWithIndex: f => b => v => foldableWithIndexNonEmpty.foldlWithIndex(x => f((() => {
    if (x.tag === "Nothing") { return 0; }
    if (x.tag === "Just") { return 1 + x._1 | 0; }
    $runtime.fail();
  })()))(b)(v),
  foldrWithIndex: f => b => v => foldableWithIndexNonEmpty.foldrWithIndex(x => f((() => {
    if (x.tag === "Nothing") { return 0; }
    if (x.tag === "Just") { return 1 + x._1 | 0; }
    $runtime.fail();
  })()))(b)(v),
  Foldable0: () => foldableNonEmptyList
};
const functorWithIndexList = {mapWithIndex: f => foldableWithIndexList.foldrWithIndex(i => x => acc => $List("Cons", f(i)(x), acc))(Nil), Functor0: () => functorList};
const mapWithIndex = f => v => Data$dNonEmpty.$NonEmpty(
  f(Data$dMaybe.Nothing)(v._1),
  foldableWithIndexList.foldrWithIndex(i => x => acc => $List("Cons", f(Data$dMaybe.$Maybe("Just", i))(x), acc))(Nil)(v._2)
);
const functorWithIndexNonEmptyList = {
  mapWithIndex: fn => v => mapWithIndex(x => fn((() => {
    if (x.tag === "Nothing") { return 0; }
    if (x.tag === "Just") { return 1 + x._1 | 0; }
    $runtime.fail();
  })()))(v),
  Functor0: () => functorNonEmptyList
};
const semigroupList = {append: xs => ys => foldableList.foldr(Cons)(ys)(xs)};
const monoidList = {mempty: Nil, Semigroup0: () => semigroupList};
const semigroupNonEmptyList = {append: v => as$p => Data$dNonEmpty.$NonEmpty(v._1, foldableList.foldr(Cons)($List("Cons", as$p._1, as$p._2))(v._2))};
const showList = dictShow => {
  const show = dictShow.show;
  return {
    show: v => {
      if (v.tag === "Nil") { return "Nil"; }
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
            go$a0 = b.init ? {init: false, acc: v$1._1} : {init: false, acc: b.acc + " : " + v$1._1};
            go$a1 = v$1._2;
            continue;
          }
          $runtime.fail();
        }
        return go$r;
      };
      return "(" + go({init: true, acc: ""})(listMap(show)(v)).acc + " : Nil)";
    }
  };
};
const showNonEmptyList = dictShow => {
  const $0 = showList(dictShow);
  return {show: v => "(NonEmptyList (NonEmpty " + dictShow.show(v._1) + " " + $0.show(v._2) + "))"};
};
const traversableList = {
  traverse: dictApplicative => {
    const Apply0 = dictApplicative.Apply0();
    return f => {
      const $0 = Apply0.Functor0().map((() => {
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
              go$a0 = $List("Cons", v._1, b);
              go$a1 = v._2;
              continue;
            }
            $runtime.fail();
          }
          return go$r;
        };
        return go(Nil);
      })());
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
            go$a0 = Apply0.apply(Apply0.Functor0().map(b$1 => a => $List("Cons", a, b$1))(b))(f(v._1));
            go$a1 = v._2;
            continue;
          }
          $runtime.fail();
        }
        return go$r;
      };
      const $1 = go(dictApplicative.pure(Nil));
      return x => $0($1(x));
    };
  },
  sequence: dictApplicative => traversableList.traverse(dictApplicative)(identity),
  Functor0: () => functorList,
  Foldable1: () => foldableList
};
const traversableNonEmptyList = /* #__PURE__ */ Data$dNonEmpty.traversableNonEmpty(traversableList);
const traversableWithIndexList = {
  traverseWithIndex: dictApplicative => {
    const Apply0 = dictApplicative.Apply0();
    return f => {
      const $0 = Apply0.Functor0().map((() => {
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
              go$a0 = $List("Cons", v._1, b);
              go$a1 = v._2;
              continue;
            }
            $runtime.fail();
          }
          return go$r;
        };
        return go(Nil);
      })());
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
            go$a0 = Data$dTuple.$Tuple(b._1 + 1 | 0, Apply0.apply(Apply0.Functor0().map(b$1 => a => $List("Cons", a, b$1))(b._2))(f(b._1)(v._1)));
            go$a1 = v._2;
            continue;
          }
          $runtime.fail();
        }
        return go$r;
      };
      const $1 = go(Data$dTuple.$Tuple(0, dictApplicative.pure(Nil)));
      return x => $0($1(x)._2);
    };
  },
  FunctorWithIndex0: () => functorWithIndexList,
  FoldableWithIndex1: () => foldableWithIndexList,
  Traversable2: () => traversableList
};
const traverseWithIndex = /* #__PURE__ */ (() => Data$dNonEmpty.traversableWithIndexNonEmpty(traversableWithIndexList).traverseWithIndex)();
const traversableWithIndexNonEmptyList = {
  traverseWithIndex: dictApplicative => {
    const traverseWithIndex1 = traverseWithIndex(dictApplicative);
    return f => v => dictApplicative.Apply0().Functor0().map(NonEmptyList)(traverseWithIndex1(x => f((() => {
      if (x.tag === "Nothing") { return 0; }
      if (x.tag === "Just") { return 1 + x._1 | 0; }
      $runtime.fail();
    })()))(v));
  },
  FunctorWithIndex0: () => functorWithIndexNonEmptyList,
  FoldableWithIndex1: () => foldableWithIndexNonEmptyList,
  Traversable2: () => traversableNonEmptyList
};
const unfoldable1List = {
  unfoldr1: f => b => {
    const go = go$a0$copy => go$a1$copy => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const source = go$a0, memo = go$a1;
        const v = f(source);
        if (v._2.tag === "Just") {
          go$a0 = v._2._1;
          go$a1 = $List("Cons", v._1, memo);
          continue;
        }
        if (v._2.tag === "Nothing") {
          const go$1 = go$1$a0$copy => go$1$a1$copy => {
            let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
            while (go$1$c) {
              const b$1 = go$1$a0, v$1 = go$1$a1;
              if (v$1.tag === "Nil") {
                go$1$c = false;
                go$1$r = b$1;
                continue;
              }
              if (v$1.tag === "Cons") {
                go$1$a0 = $List("Cons", v$1._1, b$1);
                go$1$a1 = v$1._2;
                continue;
              }
              $runtime.fail();
            }
            return go$1$r;
          };
          go$c = false;
          go$r = go$1(Nil)($List("Cons", v._1, memo));
          continue;
        }
        $runtime.fail();
      }
      return go$r;
    };
    return go(b)(Nil);
  }
};
const unfoldableList = {
  unfoldr: f => b => {
    const go = go$a0$copy => go$a1$copy => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const source = go$a0, memo = go$a1;
        const v = f(source);
        if (v.tag === "Nothing") {
          const go$1 = go$1$a0$copy => go$1$a1$copy => {
            let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
            while (go$1$c) {
              const b$1 = go$1$a0, v$1 = go$1$a1;
              if (v$1.tag === "Nil") {
                go$1$c = false;
                go$1$r = b$1;
                continue;
              }
              if (v$1.tag === "Cons") {
                go$1$a0 = $List("Cons", v$1._1, b$1);
                go$1$a1 = v$1._2;
                continue;
              }
              $runtime.fail();
            }
            return go$1$r;
          };
          go$c = false;
          go$r = go$1(Nil)(memo);
          continue;
        }
        if (v.tag === "Just") {
          go$a0 = v._1._2;
          go$a1 = $List("Cons", v._1._1, memo);
          continue;
        }
        $runtime.fail();
      }
      return go$r;
    };
    return go(b)(Nil);
  },
  Unfoldable10: () => unfoldable1List
};
const unfoldable1NonEmptyList = {
  unfoldr1: f => b => {
    const $0 = f(b);
    return Data$dNonEmpty.$NonEmpty(
      $0._1,
      (() => {
        const go = go$a0$copy => go$a1$copy => {
          let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
          while (go$c) {
            const source = go$a0, memo = go$a1;
            if (source.tag === "Just") {
              go$a0 = f(source._1)._2;
              go$a1 = $List("Cons", f(source._1)._1, memo);
              continue;
            }
            const go$1 = go$1$a0$copy => go$1$a1$copy => {
              let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
              while (go$1$c) {
                const b$1 = go$1$a0, v = go$1$a1;
                if (v.tag === "Nil") {
                  go$1$c = false;
                  go$1$r = b$1;
                  continue;
                }
                if (v.tag === "Cons") {
                  go$1$a0 = $List("Cons", v._1, b$1);
                  go$1$a1 = v._2;
                  continue;
                }
                $runtime.fail();
              }
              return go$1$r;
            };
            go$c = false;
            go$r = go$1(Nil)(memo);
          }
          return go$r;
        };
        return go($0._2)(Nil);
      })()
    );
  }
};
const foldable1NonEmptyList = /* #__PURE__ */ Data$dNonEmpty.foldable1NonEmpty(foldableList);
const extendNonEmptyList = {
  extend: f => v => Data$dNonEmpty.$NonEmpty(
    f(v),
    foldableList.foldr(a => v1 => ({val: $List("Cons", f(Data$dNonEmpty.$NonEmpty(a, v1.acc)), v1.val), acc: $List("Cons", a, v1.acc)}))({val: Nil, acc: Nil})(v._2).val
  ),
  Functor0: () => functorNonEmptyList
};
const extendList = {
  extend: v => v1 => {
    if (v1.tag === "Nil") { return Nil; }
    if (v1.tag === "Cons") {
      return $List(
        "Cons",
        v(v1),
        foldableList.foldr(a$p => v2 => ({val: $List("Cons", v($List("Cons", a$p, v2.acc)), v2.val), acc: $List("Cons", a$p, v2.acc)}))({val: Nil, acc: Nil})(v1._2).val
      );
    }
    $runtime.fail();
  },
  Functor0: () => functorList
};
const eq1List = {
  eq1: dictEq => xs => ys => {
    const go = v => v1 => v2 => {
      if (!v2) { return false; }
      if (v.tag === "Nil") { return v1.tag === "Nil" && v2; }
      return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && dictEq.eq(v1._1)(v._1));
    };
    return go(xs)(ys)(true);
  }
};
const eq1NonEmptyList = {
  eq1: dictEq => x => y => dictEq.eq(x._1)(y._1) && (() => {
    const go = v => v1 => v2 => {
      if (!v2) { return false; }
      if (v.tag === "Nil") { return v1.tag === "Nil" && v2; }
      return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && dictEq.eq(v1._1)(v._1));
    };
    return go(x._2)(y._2)(true);
  })()
};
const eqList = dictEq => (
  {
    eq: xs => ys => {
      const go = v => v1 => v2 => {
        if (!v2) { return false; }
        if (v.tag === "Nil") { return v1.tag === "Nil" && v2; }
        return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && dictEq.eq(v1._1)(v._1));
      };
      return go(xs)(ys)(true);
    }
  }
);
const eqNonEmptyList = dictEq => (
  {
    eq: x => y => dictEq.eq(x._1)(y._1) && (() => {
      const go = v => v1 => v2 => {
        if (!v2) { return false; }
        if (v.tag === "Nil") { return v1.tag === "Nil" && v2; }
        return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && dictEq.eq(v1._1)(v._1));
      };
      return go(x._2)(y._2)(true);
    })()
  }
);
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
            go$a0 = v._2;
            go$a1 = v1._2;
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
    return go(xs)(ys);
  },
  Eq10: () => eq1List
};
const ordNonEmpty = /* #__PURE__ */ Data$dNonEmpty.ordNonEmpty(ord1List);
const ord1NonEmptyList = /* #__PURE__ */ Data$dNonEmpty.ord1NonEmpty(ord1List);
const ordList = dictOrd => {
  const $0 = dictOrd.Eq0();
  const eqList1 = {
    eq: xs => ys => {
      const go = v => v1 => v2 => {
        if (!v2) { return false; }
        if (v.tag === "Nil") { return v1.tag === "Nil" && v2; }
        return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && $0.eq(v1._1)(v._1));
      };
      return go(xs)(ys)(true);
    }
  };
  return {
    compare: xs => ys => {
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
              go$a0 = v._2;
              go$a1 = v1._2;
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
      return go(xs)(ys);
    },
    Eq0: () => eqList1
  };
};
const ordNonEmptyList = dictOrd => ordNonEmpty(dictOrd);
const comonadNonEmptyList = {extract: v => v._1, Extend0: () => extendNonEmptyList};
const applyList = {
  apply: v => v1 => {
    if (v.tag === "Nil") { return Nil; }
    if (v.tag === "Cons") { return foldableList.foldr(Cons)(applyList.apply(v._2)(v1))(listMap(v._1)(v1)); }
    $runtime.fail();
  },
  Functor0: () => functorList
};
const applyNonEmptyList = {
  apply: v => v1 => Data$dNonEmpty.$NonEmpty(
    v._1(v1._1),
    foldableList.foldr(Cons)(applyList.apply($List("Cons", v._1, v._2))(v1._2))(applyList.apply(v._2)($List("Cons", v1._1, Nil)))
  ),
  Functor0: () => functorNonEmptyList
};
const bindList = {
  bind: v => v1 => {
    if (v.tag === "Nil") { return Nil; }
    if (v.tag === "Cons") { return foldableList.foldr(Cons)(bindList.bind(v._2)(v1))(v1(v._1)); }
    $runtime.fail();
  },
  Apply0: () => applyList
};
const bindNonEmptyList = {
  bind: v => f => {
    const v1 = f(v._1);
    return Data$dNonEmpty.$NonEmpty(
      v1._1,
      foldableList.foldr(Cons)(bindList.bind(v._2)(x => {
        const $0 = f(x);
        return $List("Cons", $0._1, $0._2);
      }))(v1._2)
    );
  },
  Apply0: () => applyNonEmptyList
};
const applicativeList = {pure: a => $List("Cons", a, Nil), Apply0: () => applyList};
const monadList = {Applicative0: () => applicativeList, Bind1: () => bindList};
const altNonEmptyList = /* #__PURE__ */ (() => ({alt: semigroupNonEmptyList.append, Functor0: () => functorNonEmptyList}))();
const altList = /* #__PURE__ */ (() => ({alt: semigroupList.append, Functor0: () => functorList}))();
const plusList = {empty: Nil, Alt0: () => altList};
const alternativeList = {Applicative0: () => applicativeList, Plus1: () => plusList};
const monadPlusList = {Monad0: () => monadList, Alternative1: () => alternativeList};
const applicativeNonEmptyList = {pure: x => Data$dNonEmpty.$NonEmpty(x, Nil), Apply0: () => applyNonEmptyList};
const monadNonEmptyList = {Applicative0: () => applicativeNonEmptyList, Bind1: () => bindNonEmptyList};
const traversable1NonEmptyList = {
  traverse1: dictApply => {
    const Functor0 = dictApply.Functor0();
    return f => v => Functor0.map(v1 => {
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
            go$a0 = Data$dNonEmpty.$NonEmpty(v$1._1, $List("Cons", b._1, b._2));
            go$a1 = v$1._2;
            continue;
          }
          $runtime.fail();
        }
        return go$r;
      };
      return go(Data$dNonEmpty.$NonEmpty(v1._1, Nil))(v1._2);
    })((() => {
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
            go$a0 = dictApply.apply(dictApply.Functor0().map(b$1 => a => Data$dNonEmpty.$NonEmpty(a, $List("Cons", b$1._1, b$1._2)))(b))(f(v$1._1));
            go$a1 = v$1._2;
            continue;
          }
          $runtime.fail();
        }
        return go$r;
      };
      return go(Functor0.map(applicativeNonEmptyList.pure)(f(v._1)))(v._2);
    })());
  },
  sequence1: dictApply => traversable1NonEmptyList.traverse1(dictApply)(identity),
  Foldable10: () => foldable1NonEmptyList,
  Traversable1: () => traversableNonEmptyList
};
export {
  $List,
  Cons,
  Nil,
  
  
  
  
  applicativeList,
  applicativeNonEmptyList,
  
  
  bindList,
  bindNonEmptyList,
  
  
  
  
  
  
  
  
  foldableList,
  foldableNonEmptyList,
  
  
  
  
  
  
  
  identity,
  listMap,
  
  
  
  
  
  
  
  
  
  ordList,
  
  
  
  
  semigroupNonEmptyList,
  showList,
  showNonEmptyList,
  
  traversable1NonEmptyList,
  traversableList,
  traversableNonEmptyList,
  
  
  
  
  
  unfoldableList
};
