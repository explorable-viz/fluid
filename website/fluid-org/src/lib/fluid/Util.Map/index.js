import * as $runtime from "../runtime.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunction from "../Data.Function/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMap$dInternal from "../Data.Map.Internal/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Effect$dException from "../Effect.Exception/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
import * as Util from "../Util/index.js";
import * as Util$dSet from "../Util.Set/index.js";
import {intersectionWith_Object} from "./foreign.js";
const identity = x => x;
const mapObjectString = {
  maplet: Foreign$dObject.singleton,
  keys: /* #__PURE__ */ (() => {
    const $0 = Data$dFoldable.foldlArray(m => a => Data$dMap$dInternal.insert(Data$dOrd.ordString)(a)()(m))(Data$dMap$dInternal.Leaf);
    return x => $0(Object.keys(x));
  })(),
  values: /* #__PURE__ */ (() => {
    const $0 = Data$dFoldable.foldrArray(Data$dList$dTypes.Cons)(Data$dList$dTypes.Nil);
    return x => $0(Foreign$dObject.values(x));
  })(),
  filterKeys: Foreign$dObject.filterKeys,
  unionWith: Foreign$dObject.unionWith,
  lookup: Foreign$dObject.lookup,
  delete: Foreign$dObject.delete,
  insert: Foreign$dObject.insert,
  toUnfoldable: dictUnfoldable => Foreign$dObject.toAscUnfoldable(dictUnfoldable),
  Set0: () => Util$dSet.setObjectString
};
const values = dict => dict.values;
const unionWith = dict => dict.unionWith;
const toUnfoldable = dict => dict.toUnfoldable;
const maplet = dict => dict.maplet;
const mapWithKey = dict => dict.mapWithKey;
const lookup = dict => dict.lookup;
const keys = dict => dict.keys;
const keyExists = dictShow => k => "Key " + dictShow.show(k) + " exists in map";
const lookup$p = dictMonadThrow => dictShow => dictMap => k => γ => Util.orElse(dictMonadThrow)("Key " + dictShow.show(k) + " exists in map")(dictMap.lookup(k)(γ));
const intersectionWith = dict => dict.intersectionWith;
const intersection = dictMapF => dictMapF.intersectionWith(Data$dFunction.const);
const insert = dict => dict.insert;
const $$get = dictShow => dictMap => k => {
  const $0 = dictMap.lookup(k);
  const $1 = Util.definitely("Key " + dictShow.show(k) + " exists in map");
  return x => $1($0(x));
};
const filterKeys = dict => dict.filterKeys;
const restrict = dictOrd => dictMap => xs => dictMap.filterKeys(v => Util$dSet.setSet(dictOrd).member(v)(xs));
const disjointUnion_inv = dictOrd => {
  const $0 = Util$dSet.setSet(dictOrd);
  return dictMap => ks => m => Data$dTuple.$Tuple(dictMap.filterKeys(v => $0.member(v)(ks))(m), dictMap.filterKeys(v => !$0.member(v)(ks))(m));
};
const disjointUnion = dictMap => dictMap.unionWith(v => v1 => Effect$dException.throwException(Effect$dException.error("not disjoint"))());
const difference = dict => dict.difference;
const $$delete = dict => dict.delete;
const mapFObjectString = {
  intersectionWith: intersectionWith_Object,
  difference: m1 => m2 => Data$dFoldable.foldlArray(b => a => Foreign$dObject.mutate($0 => () => {
    delete $0[a];
    return $0;
  })(b))(m1)(Object.keys(m2)),
  mapWithKey: Foreign$dObject.mapWithKey
};
const asMaplet = dictMap => {
  const toUnfoldable1 = dictMap.toUnfoldable(Data$dList$dTypes.unfoldableList);
  return m => Util.assertWith("")(dictMap.Set0().size(m) === 1)(Util.definitely("singleton map")((() => {
    const $0 = toUnfoldable1(m);
    if ($0.tag === "Nil") { return Data$dMaybe.Nothing; }
    if ($0.tag === "Cons") { return Data$dMaybe.$Maybe("Just", $0._1); }
    $runtime.fail();
  })()));
};
const append_inv = dictOrd => dictMap => xs => γ => Data$dTuple.$Tuple(
  dictMap.filterKeys(v => !Util$dSet.setSet(dictOrd).member(v)(xs))(γ),
  dictMap.filterKeys(v => Util$dSet.setSet(dictOrd).member(v)(xs))(γ)
);
const append = dictMap => dictMap.unionWith(v => identity);
const alter = dictMap => f => k => m => {
  const v = f(dictMap.lookup(k)(m));
  if (v.tag === "Nothing") { return dictMap.delete(k)(m); }
  if (v.tag === "Just") { return dictMap.insert(k)(v._1)(m); }
  $runtime.fail();
};
const insertWith = dictMap => f => k => v => m => {
  const $0 = dictMap.lookup(k)(m);
  return dictMap.insert(k)((() => {
    if ($0.tag === "Nothing") { return v; }
    if ($0.tag === "Just") { return f($0._1)(v); }
    $runtime.fail();
  })())(m);
};
const update = dictShow => dictMap => f => k => {
  const $0 = Util.definitely("Key " + dictShow.show(k) + " exists in map");
  return m => dictMap.insert(k)(f($0(dictMap.lookup(k)(m))))(m);
};
export {
  
  
  
  asMaplet,
  
  
  disjointUnion,
  
  
  $$get as get,
  identity,
  
  
  
  
  
  
  
  lookup$p,
  mapFObjectString,
  mapObjectString,
  
  
  
  
  
  update,
  
};
export * from "./foreign.js";
