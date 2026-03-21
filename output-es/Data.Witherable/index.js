import * as $runtime from "../runtime.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dFilterable from "../Data.Filterable/index.js";
import * as Data$dIdentity from "../Data.Identity/index.js";
import * as Data$dList from "../Data.List/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMap$dInternal from "../Data.Map.Internal/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dTraversable from "../Data.Traversable/index.js";
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
const identity = x => x;
const witherableMaybe = {
  wilt: dictApplicative => v => v1 => {
    if (v1.tag === "Nothing") { return dictApplicative.pure({left: Data$dMaybe.Nothing, right: Data$dMaybe.Nothing}); }
    if (v1.tag === "Just") {
      return dictApplicative.Apply0().Functor0().map(v2 => {
        if (v2.tag === "Left") { return {left: Data$dMaybe.$Maybe("Just", v2._1), right: Data$dMaybe.Nothing}; }
        if (v2.tag === "Right") { return {left: Data$dMaybe.Nothing, right: Data$dMaybe.$Maybe("Just", v2._1)}; }
        $runtime.fail();
      })(v(v1._1));
    }
    $runtime.fail();
  },
  wither: dictApplicative => v => v1 => {
    if (v1.tag === "Nothing") { return dictApplicative.pure(Data$dMaybe.Nothing); }
    if (v1.tag === "Just") { return v(v1._1); }
    $runtime.fail();
  },
  Filterable0: () => Data$dFilterable.filterableMaybe,
  Traversable1: () => Data$dTraversable.traversableMaybe
};
const witherableMap = dictOrd => {
  const filterableMap = Data$dFilterable.filterableMap(dictOrd);
  return {
    wilt: dictApplicative => {
      const Apply0 = dictApplicative.Apply0();
      return p => {
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
                const $0 = v._1._1;
                return Apply0.apply(Apply0.Functor0().map(v1 => {
                  const $1 = v1.left;
                  const $2 = v1.right;
                  return v2 => {
                    if (v2.tag === "Left") { return {left: Data$dMap$dInternal.insert(dictOrd)($0)(v2._1)($1), right: $2}; }
                    if (v2.tag === "Right") { return {left: $1, right: Data$dMap$dInternal.insert(dictOrd)($0)(v2._1)($2)}; }
                    $runtime.fail();
                  };
                })(b))(p(v._1._2));
              })();
              go$a1 = v._2;
              continue;
            }
            $runtime.fail();
          }
          return go$r;
        };
        const $0 = go(dictApplicative.pure({left: Data$dMap$dInternal.Leaf, right: Data$dMap$dInternal.Leaf}));
        return x => $0(toUnfoldable(x));
      };
    },
    wither: dictApplicative => {
      const Apply0 = dictApplicative.Apply0();
      return p => {
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
                const $0 = v._1._1;
                return Apply0.apply(Apply0.Functor0().map(comp => v1 => {
                  if (v1.tag === "Nothing") { return comp; }
                  if (v1.tag === "Just") { return Data$dMap$dInternal.insert(dictOrd)($0)(v1._1)(comp); }
                  $runtime.fail();
                })(b))(p(v._1._2));
              })();
              go$a1 = v._2;
              continue;
            }
            $runtime.fail();
          }
          return go$r;
        };
        const $0 = go(dictApplicative.pure(Data$dMap$dInternal.Leaf));
        return x => $0(toUnfoldable(x));
      };
    },
    Filterable0: () => filterableMap,
    Traversable1: () => Data$dMap$dInternal.traversableMap
  };
};
const witherableList = {
  wilt: dictApplicative => {
    const Apply0 = dictApplicative.Apply0();
    const $0 = Apply0.Functor0();
    return p => {
      const $1 = $0.map(v => (
        {
          left: (() => {
            const go = go$a0$copy => go$a1$copy => {
              let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
              while (go$c) {
                const v$1 = go$a0, v1 = go$a1;
                if (v1.tag === "Nil") {
                  go$c = false;
                  go$r = v$1;
                  continue;
                }
                if (v1.tag === "Cons") {
                  go$a0 = Data$dList$dTypes.$List("Cons", v1._1, v$1);
                  go$a1 = v1._2;
                  continue;
                }
                $runtime.fail();
              }
              return go$r;
            };
            return go(Data$dList$dTypes.Nil)(v.left);
          })(),
          right: (() => {
            const go = go$a0$copy => go$a1$copy => {
              let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
              while (go$c) {
                const v$1 = go$a0, v1 = go$a1;
                if (v1.tag === "Nil") {
                  go$c = false;
                  go$r = v$1;
                  continue;
                }
                if (v1.tag === "Cons") {
                  go$a0 = Data$dList$dTypes.$List("Cons", v1._1, v$1);
                  go$a1 = v1._2;
                  continue;
                }
                $runtime.fail();
              }
              return go$r;
            };
            return go(Data$dList$dTypes.Nil)(v.right);
          })()
        }
      ));
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
            go$a0 = Apply0.apply($0.map(v$1 => {
              const $2 = v$1.left;
              const $3 = v$1.right;
              return v1 => {
                if (v1.tag === "Left") { return {left: Data$dList$dTypes.$List("Cons", v1._1, $2), right: $3}; }
                if (v1.tag === "Right") { return {left: $2, right: Data$dList$dTypes.$List("Cons", v1._1, $3)}; }
                $runtime.fail();
              };
            })(b))(p(v._1));
            go$a1 = v._2;
            continue;
          }
          $runtime.fail();
        }
        return go$r;
      };
      const $2 = go(dictApplicative.pure({left: Data$dList$dTypes.Nil, right: Data$dList$dTypes.Nil}));
      return x => $1($2(x));
    };
  },
  wither: dictApplicative => {
    const Apply0 = dictApplicative.Apply0();
    const $0 = Apply0.Functor0();
    return p => {
      const $1 = $0.map(Data$dList.reverse);
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
            go$a0 = Apply0.apply($0.map(comp => v$1 => {
              if (v$1.tag === "Nothing") { return comp; }
              if (v$1.tag === "Just") { return Data$dList$dTypes.$List("Cons", v$1._1, comp); }
              $runtime.fail();
            })(b))(p(v._1));
            go$a1 = v._2;
            continue;
          }
          $runtime.fail();
        }
        return go$r;
      };
      const $2 = go(dictApplicative.pure(Data$dList$dTypes.Nil));
      return x => $1($2(x));
    };
  },
  Filterable0: () => Data$dFilterable.filterableList,
  Traversable1: () => Data$dList$dTypes.traversableList
};
const witherableEither = dictMonoid => {
  const mempty = dictMonoid.mempty;
  const filterableEither = Data$dFilterable.filterableEither(dictMonoid);
  return {
    wilt: dictApplicative => v => v1 => {
      if (v1.tag === "Left") { return dictApplicative.pure({left: Data$dEither.$Either("Left", v1._1), right: Data$dEither.$Either("Left", v1._1)}); }
      if (v1.tag === "Right") {
        return dictApplicative.Apply0().Functor0().map(v2 => {
          if (v2.tag === "Left") { return {left: Data$dEither.$Either("Right", v2._1), right: Data$dEither.$Either("Left", mempty)}; }
          if (v2.tag === "Right") { return {left: Data$dEither.$Either("Left", mempty), right: Data$dEither.$Either("Right", v2._1)}; }
          $runtime.fail();
        })(v(v1._1));
      }
      $runtime.fail();
    },
    wither: dictApplicative => v => v1 => {
      if (v1.tag === "Left") { return dictApplicative.pure(Data$dEither.$Either("Left", v1._1)); }
      if (v1.tag === "Right") {
        return dictApplicative.Apply0().Functor0().map(v2 => {
          if (v2.tag === "Nothing") { return Data$dEither.$Either("Left", mempty); }
          if (v2.tag === "Just") { return Data$dEither.$Either("Right", v2._1); }
          $runtime.fail();
        })(v(v1._1));
      }
      $runtime.fail();
    },
    Filterable0: () => filterableEither,
    Traversable1: () => Data$dTraversable.traversableEither
  };
};
const witherDefault = dictWitherable => {
  const compact = dictWitherable.Filterable0().Compactable0().compact;
  return dictApplicative => {
    const traverse1 = dictWitherable.Traversable1().traverse(dictApplicative);
    return p => {
      const $0 = dictApplicative.Apply0().Functor0().map(compact);
      const $1 = traverse1(p);
      return x => $0($1(x));
    };
  };
};
const wither = dict => dict.wither;
const withered = dictWitherable => dictApplicative => dictWitherable.wither(dictApplicative)(identity);
const wiltDefault = dictWitherable => {
  const separate = dictWitherable.Filterable0().Compactable0().separate;
  return dictApplicative => {
    const traverse1 = dictWitherable.Traversable1().traverse(dictApplicative);
    return p => {
      const $0 = dictApplicative.Apply0().Functor0().map(separate);
      const $1 = traverse1(p);
      return x => $0($1(x));
    };
  };
};
const witherableArray = {
  wilt: dictApplicative => {
    const separate = witherableArray.Filterable0().Compactable0().separate;
    const traverse1 = witherableArray.Traversable1().traverse(dictApplicative);
    return p => {
      const $0 = dictApplicative.Apply0().Functor0().map(separate);
      const $1 = traverse1(p);
      return x => $0($1(x));
    };
  },
  wither: dictApplicative => {
    const compact = witherableArray.Filterable0().Compactable0().compact;
    const traverse1 = witherableArray.Traversable1().traverse(dictApplicative);
    return p => {
      const $0 = dictApplicative.Apply0().Functor0().map(compact);
      const $1 = traverse1(p);
      return x => $0($1(x));
    };
  },
  Filterable0: () => Data$dFilterable.filterableArray,
  Traversable1: () => Data$dTraversable.traversableArray
};
const wilt = dict => dict.wilt;
const wilted = dictWitherable => dictApplicative => dictWitherable.wilt(dictApplicative)(identity);
const traverseByWither = dictWitherable => dictApplicative => {
  const wither2 = dictWitherable.wither(dictApplicative);
  return f => wither2((() => {
    const $0 = dictApplicative.Apply0().Functor0().map(Data$dMaybe.Just);
    return x => $0(f(x));
  })());
};
const partitionMapByWilt = dictWitherable => {
  const wilt1 = dictWitherable.wilt(Data$dIdentity.applicativeIdentity);
  return p => wilt1(x => p(x));
};
const filterMapByWither = dictWitherable => {
  const wither1 = dictWitherable.wither(Data$dIdentity.applicativeIdentity);
  return p => wither1(x => p(x));
};
export {
  filterMapByWither,
  identity,
  partitionMapByWilt,
  toUnfoldable,
  traverseByWither,
  wilt,
  wiltDefault,
  wilted,
  wither,
  witherDefault,
  witherableArray,
  witherableEither,
  witherableList,
  witherableMap,
  witherableMaybe,
  withered
};
