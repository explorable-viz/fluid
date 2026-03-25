// | Helper functions for working with immutable Javascript arrays.
// |
// | _Note_: Depending on your use-case, you may prefer to use `Data.List` or
// | `Data.Sequence` instead, which might give better performance for certain
// | use cases. This module is useful when integrating with JavaScript libraries
// | which use arrays, but immutable arrays are not a practical data structure
// | for many use cases due to their poor asymptotics.
// |
// | In addition to the functions in this module, Arrays have a number of
// | useful instances:
// |
// | * `Functor`, which provides `map :: forall a b. (a -> b) -> Array a ->
// |   Array b`
// | * `Apply`, which provides `(<*>) :: forall a b. Array (a -> b) -> Array a
// |   -> Array b`. This function works a bit like a Cartesian product; the
// |   result array is constructed by applying each function in the first
// |   array to each value in the second, so that the result array ends up with
// |   a length equal to the product of the two arguments' lengths.
// | * `Bind`, which provides `(>>=) :: forall a b. (a -> Array b) -> Array a
// |   -> Array b` (this is the same as `concatMap`).
// | * `Semigroup`, which provides `(<>) :: forall a. Array a -> Array a ->
// |   Array a`, for concatenating arrays.
// | * `Foldable`, which provides a slew of functions for *folding* (also known
// |   as *reducing*) arrays down to one value. For example,
// |   `Data.Foldable.or` tests whether an array of `Boolean` values contains
// |   at least one `true` value.
// | * `Traversable`, which provides the PureScript version of a for-loop,
// |   allowing you to STAI.iterate over an array and accumulate effects.
// |
import * as $runtime from "../runtime.js";
import * as Control$dBind from "../Control.Bind/index.js";
import * as Control$dMonad$dRec$dClass from "../Control.Monad.Rec.Class/index.js";
import * as Control$dMonad$dST$dInternal from "../Control.Monad.ST.Internal/index.js";
import * as Data$dArray$dST from "../Data.Array.ST/index.js";
import * as Data$dArray$dST$dIterator from "../Data.Array.ST.Iterator/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dFunctorWithIndex from "../Data.FunctorWithIndex/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dTraversable from "../Data.Traversable/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import {
  _deleteAt,
  _insertAt,
  _updateAt,
  allImpl,
  anyImpl,
  concat,
  filterImpl,
  findIndexImpl,
  findLastIndexImpl,
  findMapImpl,
  fromFoldableImpl,
  indexImpl,
  length,
  partitionImpl,
  rangeImpl,
  replicateImpl,
  reverse,
  scanlImpl,
  scanrImpl,
  sliceImpl,
  sortByImpl,
  unconsImpl,
  unsafeIndexImpl,
  zipWithImpl
} from "./foreign.js";
const traverse_ = /* #__PURE__ */ Data$dFoldable.traverse_(Control$dMonad$dST$dInternal.applicativeST);
const intercalate1 = dictMonoid => {
  const $0 = dictMonoid.Semigroup0();
  const mempty = dictMonoid.mempty;
  return sep => xs => Data$dFoldable.foldlArray(v => v1 => {
    if (v.init) { return {init: false, acc: v1}; }
    return {init: false, acc: $0.append(v.acc)($0.append(sep)(v1))};
  })({init: true, acc: mempty})(xs).acc;
};
const zipWith = $0 => $1 => $2 => zipWithImpl($0, $1, $2);
const zipWithA = dictApplicative => {
  const sequence1 = Data$dTraversable.traversableArray.traverse(dictApplicative)(Data$dTraversable.identity);
  return f => xs => ys => sequence1(zipWithImpl(f, xs, ys));
};
const zip = /* #__PURE__ */ zipWith(Data$dTuple.Tuple);
const updateAtIndices = dictFoldable => {
  const traverse_1 = traverse_(dictFoldable);
  return us => xs => {
    const result = [...xs];
    traverse_1(v => {
      const $0 = v._2;
      const $1 = v._1;
      return () => Data$dArray$dST.pokeImpl($1, $0, result);
    })(us)();
    return result;
  };
};
const updateAt = $0 => $1 => $2 => _updateAt(Data$dMaybe.Just, Data$dMaybe.Nothing, $0, $1, $2);
const unsafeIndex = () => $0 => $1 => $0[$1];
const uncons = $0 => unconsImpl(v => Data$dMaybe.Nothing, x => xs => Data$dMaybe.$Maybe("Just", {head: x, tail: xs}), $0);
const toUnfoldable = dictUnfoldable => xs => {
  const len = xs.length;
  return dictUnfoldable.unfoldr(i => {
    if (i < len) { return Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple(xs[i], i + 1 | 0)); }
    return Data$dMaybe.Nothing;
  })(0);
};
const tail = $0 => unconsImpl(v => Data$dMaybe.Nothing, v => xs => Data$dMaybe.$Maybe("Just", xs), $0);
const sortBy = comp => $0 => sortByImpl(
  comp,
  v => {
    if (v === "GT") { return 1; }
    if (v === "EQ") { return 0; }
    if (v === "LT") { return -1; }
    $runtime.fail();
  },
  $0
);
const sortWith = dictOrd => f => sortBy(x => y => dictOrd.compare(f(x))(f(y)));
const sort = dictOrd => {
  const compare = dictOrd.compare;
  return xs => sortBy(compare)(xs);
};
const snoc = xs => x => (() => {
  const $0 = Data$dArray$dST.push(x);
  return () => {
    const result = [...xs];
    $0(result)();
    return result;
  };
})()();
const slice = $0 => $1 => $2 => sliceImpl($0, $1, $2);
const splitAt = v => v1 => {
  if (v <= 0) { return {before: [], after: v1}; }
  return {before: sliceImpl(0, v, v1), after: sliceImpl(v, v1.length, v1)};
};
const take = n => xs => {
  if (n < 1) { return []; }
  return sliceImpl(0, n, xs);
};
const singleton = a => [a];
const scanr = $0 => $1 => $2 => scanrImpl($0, $1, $2);
const scanl = $0 => $1 => $2 => scanlImpl($0, $1, $2);
const replicate = $0 => $1 => replicateImpl($0, $1);
const range = $0 => $1 => rangeImpl($0, $1);
const partition = $0 => $1 => partitionImpl($0, $1);
const $$null = xs => xs.length === 0;
const modifyAtIndices = dictFoldable => {
  const traverse_1 = traverse_(dictFoldable);
  return is => f => xs => {
    const result = [...xs];
    traverse_1(i => Data$dArray$dST.modify(i)(f)(result))(is)();
    return result;
  };
};
const mapWithIndex = Data$dFunctorWithIndex.mapWithIndexArray;
const intersperse = a => arr => {
  const v = arr.length;
  if (v < 2) { return arr; }
  const out = [];
  out.push(arr[0]);
  for (const idx of $runtime.range(1, v)) {
    out.push(a);
    out.push(arr[idx]);
  }
  return out;
};
const intercalate = dictMonoid => intercalate1(dictMonoid);
const insertAt = $0 => $1 => $2 => _insertAt(Data$dMaybe.Just, Data$dMaybe.Nothing, $0, $1, $2);
const init = xs => {
  if (xs.length === 0) { return Data$dMaybe.Nothing; }
  return Data$dMaybe.$Maybe("Just", sliceImpl(0, xs.length - 1 | 0, xs));
};
const index = $0 => $1 => {
  if ($1 >= 0 && $1 < $0.length) { return Data$dMaybe.$Maybe("Just", $0[$1]); }
  return Data$dMaybe.Nothing;
};
const last = xs => {
  const $0 = xs.length - 1 | 0;
  if ($0 >= 0 && $0 < xs.length) { return Data$dMaybe.$Maybe("Just", xs[$0]); }
  return Data$dMaybe.Nothing;
};
const unsnoc = xs => {
  if (xs.length === 0) {
    const $0 = xs.length - 1 | 0;
    return Data$dMaybe.Nothing;
  }
  const $0 = xs.length - 1 | 0;
  if ($0 >= 0 && $0 < xs.length) { return Data$dMaybe.$Maybe("Just", {init: sliceImpl(0, xs.length - 1 | 0, xs), last: xs[$0]}); }
  return Data$dMaybe.Nothing;
};
const modifyAt = i => f => xs => {
  if (i >= 0 && i < xs.length) { return _updateAt(Data$dMaybe.Just, Data$dMaybe.Nothing, i, f(xs[i]), xs); }
  return Data$dMaybe.Nothing;
};
const span = p => arr => {
  const go = go$a0$copy => {
    let go$a0 = go$a0$copy, go$c = true, go$r;
    while (go$c) {
      const i = go$a0;
      if (i >= 0 && i < arr.length) {
        if (p(arr[i])) {
          go$a0 = i + 1 | 0;
          continue;
        }
        go$c = false;
        go$r = Data$dMaybe.$Maybe("Just", i);
        continue;
      }
      go$c = false;
      go$r = Data$dMaybe.Nothing;
    }
    return go$r;
  };
  const breakIndex = go(0);
  if (breakIndex.tag === "Just") {
    if (breakIndex._1 === 0) { return {init: [], rest: arr}; }
    return {init: sliceImpl(0, breakIndex._1, arr), rest: sliceImpl(breakIndex._1, arr.length, arr)};
  }
  if (breakIndex.tag === "Nothing") { return {init: arr, rest: []}; }
  $runtime.fail();
};
const takeWhile = p => xs => span(p)(xs).init;
const unzip = xs => {
  const fsts = [];
  const snds = [];
  const $0 = {value: 0};
  Data$dArray$dST$dIterator.iterate(Data$dArray$dST$dIterator.$Iterator(
    v => {
      if (v >= 0 && v < xs.length) { return Data$dMaybe.$Maybe("Just", xs[v]); }
      return Data$dMaybe.Nothing;
    },
    $0
  ))(v => {
    const $1 = v._2;
    return () => {
      fsts.push(v._1);
      snds.push($1);
    };
  })();
  return Data$dTuple.$Tuple(fsts, snds);
};
const head = xs => {
  if (0 < xs.length) { return Data$dMaybe.$Maybe("Just", xs[0]); }
  return Data$dMaybe.Nothing;
};
const nubBy = comp => xs => {
  const indexedAndSorted = sortBy(x => y => comp(x._2)(y._2))(Data$dFunctorWithIndex.mapWithIndexArray(Data$dTuple.Tuple)(xs));
  if (0 < indexedAndSorted.length) {
    return Data$dFunctor.arrayMap(Data$dTuple.snd)(sortWith(Data$dOrd.ordInt)(Data$dTuple.fst)((() => {
      const result = [indexedAndSorted[0]];
      for (const v1 of indexedAndSorted) {
        const $0 = comp((() => {
          const $0 = result.length - 1 | 0;
          if ($0 >= 0 && $0 < result.length) { return result[$0]._2; }
          $runtime.fail();
        })())(v1._2);
        if ($0 === "LT" || $0 === "GT" || $0 !== "EQ") { result.push(v1); }
      }
      return result;
    })()));
  }
  return [];
};
const nub = dictOrd => nubBy(dictOrd.compare);
const groupBy = op => xs => {
  const result = [];
  const $0 = {value: 0};
  const iter = Data$dArray$dST$dIterator.$Iterator(
    v => {
      if (v >= 0 && v < xs.length) { return Data$dMaybe.$Maybe("Just", xs[v]); }
      return Data$dMaybe.Nothing;
    },
    $0
  );
  Data$dArray$dST$dIterator.iterate(iter)(x => () => {
    const sub1 = [];
    sub1.push(x);
    Data$dArray$dST$dIterator.pushWhile(op(x))(iter)(sub1)();
    result.push(sub1);
  })();
  return result;
};
const groupAllBy = cmp => {
  const $0 = groupBy(x => y => cmp(x)(y) === "EQ");
  return x => $0(sortBy(cmp)(x));
};
const groupAll = dictOrd => groupAllBy(dictOrd.compare);
const group = dictEq => {
  const eq2 = dictEq.eq;
  return xs => groupBy(eq2)(xs);
};
const fromFoldable = dictFoldable => {
  const $0 = dictFoldable.foldr;
  return $1 => fromFoldableImpl($0, $1);
};
const foldr = Data$dFoldable.foldrArray;
const foldl = Data$dFoldable.foldlArray;
const transpose = xs => {
  const go = go$a0$copy => go$a1$copy => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
    while (go$c) {
      const idx = go$a0, allArrays = go$a1;
      const v = Data$dFoldable.foldlArray(acc => nextArr => {
        if (idx >= 0 && idx < nextArr.length) {
          const $0 = nextArr[idx];
          return Data$dMaybe.$Maybe(
            "Just",
            (() => {
              if (acc.tag === "Nothing") { return [$0]; }
              if (acc.tag === "Just") { return snoc(acc._1)($0); }
              $runtime.fail();
            })()
          );
        }
        return acc;
      })(Data$dMaybe.Nothing)(xs);
      if (v.tag === "Nothing") {
        go$c = false;
        go$r = allArrays;
        continue;
      }
      if (v.tag === "Just") {
        go$a0 = idx + 1 | 0;
        go$a1 = snoc(allArrays)(v._1);
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go(0)([]);
};
const foldRecM = dictMonadRec => {
  const Monad0 = dictMonadRec.Monad0();
  const $0 = Monad0.Applicative0();
  return f => b => array => dictMonadRec.tailRecM(o => {
    if (o.b >= array.length) { return $0.pure(Control$dMonad$dRec$dClass.$Step("Done", o.a)); }
    return Monad0.Bind1().bind(f(o.a)(array[o.b]))(res$p => $0.pure(Control$dMonad$dRec$dClass.$Step("Loop", {a: res$p, b: o.b + 1 | 0})));
  })({a: b, b: 0});
};
const foldMap = dictMonoid => Data$dFoldable.foldableArray.foldMap(dictMonoid);
const foldM = dictMonad => f => b => $0 => unconsImpl(v => dictMonad.Applicative0().pure(b), a => as => dictMonad.Bind1().bind(f(b)(a))(b$p => foldM(dictMonad)(f)(b$p)(as)), $0);
const fold = dictMonoid => Data$dFoldable.foldableArray.foldMap(dictMonoid)(Data$dFoldable.identity);
const findMap = $0 => $1 => findMapImpl(Data$dMaybe.Nothing, Data$dMaybe.isJust, $0, $1);
const findLastIndex = $0 => $1 => findLastIndexImpl(Data$dMaybe.Just, Data$dMaybe.Nothing, $0, $1);
const insertBy = cmp => x => ys => {
  const $0 = _insertAt(
    Data$dMaybe.Just,
    Data$dMaybe.Nothing,
    (() => {
      const $0 = findLastIndexImpl(Data$dMaybe.Just, Data$dMaybe.Nothing, y => cmp(x)(y) === "GT", ys);
      if ($0.tag === "Nothing") { return 0; }
      if ($0.tag === "Just") { return $0._1 + 1 | 0; }
      $runtime.fail();
    })(),
    x,
    ys
  );
  if ($0.tag === "Just") { return $0._1; }
  $runtime.fail();
};
const insert = dictOrd => insertBy(dictOrd.compare);
const findIndex = $0 => $1 => findIndexImpl(Data$dMaybe.Just, Data$dMaybe.Nothing, $0, $1);
const find = f => xs => {
  const $0 = findIndexImpl(Data$dMaybe.Just, Data$dMaybe.Nothing, f, xs);
  if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", xs[$0._1]); }
  return Data$dMaybe.Nothing;
};
const filter = $0 => $1 => filterImpl($0, $1);
const intersectBy = eq2 => xs => ys => filterImpl(
  x => {
    const $0 = findIndexImpl(Data$dMaybe.Just, Data$dMaybe.Nothing, eq2(x), ys);
    if ($0.tag === "Nothing") { return false; }
    if ($0.tag === "Just") { return true; }
    $runtime.fail();
  },
  xs
);
const intersect = dictEq => intersectBy(dictEq.eq);
const elemLastIndex = dictEq => x => findLastIndex(v => dictEq.eq(v)(x));
const elemIndex = dictEq => x => findIndex(v => dictEq.eq(v)(x));
const notElem = dictEq => a => arr => {
  const $0 = findIndexImpl(Data$dMaybe.Just, Data$dMaybe.Nothing, v => dictEq.eq(v)(a), arr);
  if ($0.tag === "Nothing") { return true; }
  if ($0.tag === "Just") { return false; }
  $runtime.fail();
};
const elem = dictEq => a => arr => {
  const $0 = findIndexImpl(Data$dMaybe.Just, Data$dMaybe.Nothing, v => dictEq.eq(v)(a), arr);
  if ($0.tag === "Nothing") { return false; }
  if ($0.tag === "Just") { return true; }
  $runtime.fail();
};
const dropWhile = p => xs => span(p)(xs).rest;
const dropEnd = n => xs => {
  const $0 = xs.length - n | 0;
  if ($0 < 1) { return []; }
  return sliceImpl(0, $0, xs);
};
const drop = n => xs => {
  if (n < 1) { return xs; }
  return sliceImpl(n, xs.length, xs);
};
const takeEnd = n => xs => {
  const $0 = xs.length - n | 0;
  if ($0 < 1) { return xs; }
  return sliceImpl($0, xs.length, xs);
};
const deleteAt = $0 => $1 => _deleteAt(Data$dMaybe.Just, Data$dMaybe.Nothing, $0, $1);
const deleteBy = v => v1 => v2 => {
  if (v2.length === 0) { return []; }
  const $0 = findIndexImpl(Data$dMaybe.Just, Data$dMaybe.Nothing, v(v1), v2);
  if ($0.tag === "Nothing") { return v2; }
  if ($0.tag === "Just") {
    const $1 = _deleteAt(Data$dMaybe.Just, Data$dMaybe.Nothing, $0._1, v2);
    if ($1.tag === "Just") { return $1._1; }
  }
  $runtime.fail();
};
const $$delete = dictEq => deleteBy(dictEq.eq);
const difference = dictEq => Data$dFoldable.foldrArray($$delete(dictEq));
const cons = x => xs => [x, ...xs];
const some = dictAlternative => dictLazy => v => dictAlternative.Applicative0().Apply0().apply(dictAlternative.Plus1().Alt0().Functor0().map(cons)(v))(dictLazy.defer(v1 => many(dictAlternative)(dictLazy)(v)));
const many = dictAlternative => dictLazy => v => dictAlternative.Plus1().Alt0().alt(some(dictAlternative)(dictLazy)(v))(dictAlternative.Applicative0().pure([]));
const concatMap = b => a => Control$dBind.arrayBind(a)(b);
const mapMaybe = f => concatMap(x => {
  const $0 = f(x);
  if ($0.tag === "Nothing") { return []; }
  if ($0.tag === "Just") { return [$0._1]; }
  $runtime.fail();
});
const filterA = dictApplicative => {
  const traverse1 = Data$dTraversable.traversableArray.traverse(dictApplicative);
  const $0 = dictApplicative.Apply0().Functor0();
  return p => {
    const $1 = traverse1(x => $0.map(Data$dTuple.Tuple(x))(p(x)));
    const $2 = $0.map(mapMaybe(v => {
      if (v._2) { return Data$dMaybe.$Maybe("Just", v._1); }
      return Data$dMaybe.Nothing;
    }));
    return x => $2($1(x));
  };
};
const catMaybes = /* #__PURE__ */ mapMaybe(x => x);
const any = $0 => $1 => anyImpl($0, $1);
const nubByEq = eq2 => xs => {
  const arr = [];
  for (const x of xs) {
    if (!anyImpl(v => eq2(v)(x), arr)) { arr.push(x); }
  }
  return arr;
};
const nubEq = dictEq => nubByEq(dictEq.eq);
const unionBy = eq2 => xs => ys => [...xs, ...Data$dFoldable.foldlArray(b => a => deleteBy(eq2)(a)(b))(nubByEq(eq2)(ys))(xs)];
const union = dictEq => unionBy(dictEq.eq);
const alterAt = i => f => xs => {
  if (i >= 0 && i < xs.length) {
    const v = f(xs[i]);
    if (v.tag === "Nothing") { return _deleteAt(Data$dMaybe.Just, Data$dMaybe.Nothing, i, xs); }
    if (v.tag === "Just") { return _updateAt(Data$dMaybe.Just, Data$dMaybe.Nothing, i, v._1, xs); }
    $runtime.fail();
  }
  return Data$dMaybe.Nothing;
};
const all = $0 => $1 => allImpl($0, $1);
export {
  
  alterAt,
  any,
  
  
  
  $$delete as delete,
  
  deleteBy,
  
  
  
  
  elem,
  
  
  
  filterA,
  find,
  
  
  
  
  foldM,
  
  
  
  
  
  
  
  groupAllBy,
  groupBy,
  
  
  
  
  
  insertBy,
  
  
  
  intersectBy,
  intersperse,
  
  many,
  mapMaybe,
  
  modifyAt,
  modifyAtIndices,
  notElem,
  
  nubBy,
  nubByEq,
  
  
  
  
  
  
  
  
  
  snoc,
  some,
  
  sortBy,
  sortWith,
  span,
  splitAt,
  
  
  takeEnd,
  
  
  transpose,
  
  
  
  unionBy,
  
  unsnoc,
  unzip,
  
  updateAtIndices,
  
  zipWith,
  zipWithA
};
export * from "./foreign.js";
