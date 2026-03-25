// Better name and more consistent interface to Foreign.Object, plus some additional functions.
// Newtype wrapper so we can fix Ord instance be consistent with Eq (i.e. to use isSubmap vs. toAscArray).
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFoldableWithIndex from "../Data.FoldableWithIndex/index.js";
import * as Data$dFunction from "../Data.Function/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
import * as Util$dMap from "../Util.Map/index.js";
import * as Util$dSet from "../Util.Set/index.js";
const identity = x => x;
const Dict = x => x;
const newtypeDict_ = {Coercible0: () => {}};
const mapFDictString = {
  intersectionWith: f => v => v1 => Util$dMap.intersectionWith_Object(f)(v)(v1),
  difference: v => v1 => Util$dMap.mapFObjectString.difference(v)(v1),
  mapWithKey: f => v => Foreign$dObject._mapWithKey(v, f)
};
const isEmptyDict = {isEmpty: v => Foreign$dObject.isEmpty(v)};
const setDictString = {
  empty: Foreign$dObject.empty,
  filter: p => v => Foreign$dObject.filterWithKey(x => {
    const $0 = p(x);
    return v$1 => $0;
  })(v),
  size: v => Foreign$dObject.size(v),
  member: x => v => Object.hasOwn(v, x),
  difference: v => v1 => Util$dSet.setObjectString.difference(v)(v1),
  union: v => v1 => Foreign$dObject.union(v)(v1),
  IsEmpty0: () => isEmptyDict
};
const mapDictString = {
  maplet: k => v => {
    const $0 = {};
    $0[k] = v;
    return $0;
  },
  keys: v => Util$dMap.mapObjectString.keys(v),
  values: v => Util$dMap.mapObjectString.values(v),
  filterKeys: p => v => Foreign$dObject.filterWithKey(x => {
    const $0 = p(x);
    return v$1 => $0;
  })(v),
  unionWith: f => v => v1 => Foreign$dObject.unionWith(f)(v)(v1),
  lookup: k => v => Foreign$dObject._lookup(Data$dMaybe.Nothing, Data$dMaybe.Just, k, v),
  delete: k => v => Foreign$dObject.mutate($0 => () => {
    delete $0[k];
    return $0;
  })(v),
  insert: k => v => v1 => Foreign$dObject.mutate($0 => () => {
    $0[k] = v;
    return $0;
  })(v1),
  toUnfoldable: dictUnfoldable => Foreign$dObject.toAscUnfoldable(dictUnfoldable),
  Set0: () => setDictString
};
const functorDict = {map: f => m => Foreign$dObject._fmapObject(m, f)};
const foldableDict = {
  foldl: f => z => m => Foreign$dObject.fold(z$1 => v => f(z$1))(z)(m),
  foldr: f => z => m => Data$dFoldable.foldrArray(f)(z)(Foreign$dObject.values(m)),
  foldMap: dictMonoid => {
    const foldMap1 = Foreign$dObject.foldMap(dictMonoid);
    return f => foldMap1(v => f);
  }
};
const foldableWithIndexStringDi = {
  foldlWithIndex: f => z => v => Foreign$dObject.fold(b => a => f(a)(b))(z)(v),
  foldrWithIndex: f => Data$dFoldableWithIndex.foldrWithIndexDefault(foldableWithIndexStringDi)(f),
  foldMapWithIndex: dictMonoid => f => foldableWithIndexStringDi.foldlWithIndex(i => acc => x => dictMonoid.Semigroup0().append(acc)(f(i)(x)))(dictMonoid.mempty),
  Foldable0: () => foldableDict
};
const traversableDict = {
  traverse: dictApplicative => {
    const $0 = Foreign$dObject.traversableWithIndexObject.traverseWithIndex(dictApplicative);
    return f => m => dictApplicative.Apply0().Functor0().map(v1 => v1)($0(v => f)(m));
  },
  sequence: dictApplicative => v => traversableDict.traverse(dictApplicative)(identity)(v),
  Functor0: () => functorDict,
  Foldable1: () => foldableDict
};
const eqDict = dictEq => Foreign$dObject.eqObject(dictEq);
const ordDict = dictOrd => {
  const Eq0 = dictOrd.Eq0();
  const eqDict1 = Foreign$dObject.eqObject(Eq0);
  return {
    compare: v => v1 => {
      if (Foreign$dObject.isSubmap(Eq0)(v)(v1)) {
        if (Foreign$dObject.isSubmap(Eq0)(v1)(v)) { return Data$dOrdering.EQ; }
        return Data$dOrdering.LT;
      }
      return Data$dOrdering.GT;
    },
    Eq0: () => eqDict1
  };
};
const applyDict = {apply: v => v1 => Util$dMap.intersectionWith_Object(Data$dFunction.apply)(v)(v1), Functor0: () => functorDict};
const unions = dictFoldable => dictFoldable.foldl(setDictString.union)(Foreign$dObject.empty);
const toArrayWithKey = f => v => Foreign$dObject.toArrayWithKey(f)(v);
const fromFoldable = dictFoldable => Foreign$dObject.fromFoldable(dictFoldable);
export {
  
  
  
  foldableDict,
  
  
  
  identity,
  
  mapDictString,
  
  
  ordDict,
  
  
  traversableDict,
  
};
