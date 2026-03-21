import * as $runtime from "../runtime.js";
import * as Data$dFunction from "../Data.Function/index.js";
import * as Data$dMap$dInternal from "../Data.Map.Internal/index.js";
const SemigroupMap = x => x;
const traversableWithIndexSemigroupMap = Data$dMap$dInternal.traversableWithIndexMap;
const traversableSemigroupMap = Data$dMap$dInternal.traversableMap;
const showSemigroupMap = dictShow => dictShow1 => Data$dMap$dInternal.showMap(dictShow)(dictShow1);
const semigroupSemigroupMap = dictOrd => {
  const compare = dictOrd.compare;
  return dictSemigroup => {
    const append = dictSemigroup.append;
    return {append: v => v1 => Data$dMap$dInternal.unsafeUnionWith(compare, append, v, v1)};
  };
};
const plusSemigroupMap = dictOrd => Data$dMap$dInternal.plusMap(dictOrd);
const ordSemigroupMap = dictOrd => Data$dMap$dInternal.ordMap(dictOrd);
const ord1SemigroupMap = dictOrd => Data$dMap$dInternal.ord1Map(dictOrd);
const newtypeSemigroupMap = {Coercible0: () => {}};
const monoidSemigroupMap = dictOrd => {
  const semigroupSemigroupMap1 = semigroupSemigroupMap(dictOrd);
  return dictSemigroup => {
    const semigroupSemigroupMap2 = semigroupSemigroupMap1(dictSemigroup);
    return {mempty: Data$dMap$dInternal.Leaf, Semigroup0: () => semigroupSemigroupMap2};
  };
};
const keys = /* #__PURE__ */ (() => {
  const go = v => {
    if (v.tag === "Leaf") { return Data$dMap$dInternal.Leaf; }
    if (v.tag === "Node") { return Data$dMap$dInternal.$$$Map("Node", v._1, v._2, v._3, undefined, go(v._5), go(v._6)); }
    $runtime.fail();
  };
  return x => go(x);
})();
const functorWithIndexSemigroupMap = Data$dMap$dInternal.functorWithIndexMap;
const functorSemigroupMap = Data$dMap$dInternal.functorMap;
const foldableWithIndexSemigroupMap = Data$dMap$dInternal.foldableWithIndexMap;
const foldableSemigroupMap = Data$dMap$dInternal.foldableMap;
const eqSemigroupMap = dictEq => dictEq1 => Data$dMap$dInternal.eqMap(dictEq)(dictEq1);
const eq1SemigroupMap = dictEq => ({eq1: dictEq1 => Data$dMap$dInternal.eqMap(dictEq)(dictEq1).eq});
const bindSemigroupMap = dictOrd => Data$dMap$dInternal.bindMap(dictOrd);
const applySemigroupMap = dictOrd => (
  {
    apply: (() => {
      const compare = dictOrd.compare;
      return m1 => m2 => Data$dMap$dInternal.unsafeIntersectionWith(compare, Data$dMap$dInternal.identity, m1, m2);
    })(),
    Functor0: () => Data$dMap$dInternal.functorMap
  }
);
const altSemigroupMap = dictOrd => (
  {
    alt: (() => {
      const compare = dictOrd.compare;
      return m1 => m2 => Data$dMap$dInternal.unsafeUnionWith(compare, Data$dFunction.const, m1, m2);
    })(),
    Functor0: () => Data$dMap$dInternal.functorMap
  }
);
export {
  SemigroupMap,
  altSemigroupMap,
  applySemigroupMap,
  bindSemigroupMap,
  eq1SemigroupMap,
  eqSemigroupMap,
  foldableSemigroupMap,
  foldableWithIndexSemigroupMap,
  functorSemigroupMap,
  functorWithIndexSemigroupMap,
  keys,
  monoidSemigroupMap,
  newtypeSemigroupMap,
  ord1SemigroupMap,
  ordSemigroupMap,
  plusSemigroupMap,
  semigroupSemigroupMap,
  showSemigroupMap,
  traversableSemigroupMap,
  traversableWithIndexSemigroupMap
};
