import * as $runtime from "../runtime.js";
import * as Control$dApply from "../Control.Apply/index.js";
const identity = x => x;
const monoidEndo = /* #__PURE__ */ (() => {
  const semigroupEndo1 = {append: v => v1 => x => v(v1(x))};
  return {mempty: x => x, Semigroup0: () => semigroupEndo1};
})();
const monoidDual = /* #__PURE__ */ (() => {
  const $0 = monoidEndo.Semigroup0();
  const semigroupDual1 = {append: v => v1 => $0.append(v1)(v)};
  return {mempty: monoidEndo.mempty, Semigroup0: () => semigroupDual1};
})();
const bifoldr = dict => dict.bifoldr;
const bitraverse_ = dictBifoldable => dictApplicative => {
  const $0 = dictApplicative.Apply0();
  const applySecond = a => b => $0.apply($0.Functor0().map(v => Control$dApply.identity)(a))(b);
  return f => g => dictBifoldable.bifoldr(x => applySecond(f(x)))(x => applySecond(g(x)))(dictApplicative.pure());
};
const bifor_ = dictBifoldable => dictApplicative => {
  const bitraverse_2 = bitraverse_(dictBifoldable)(dictApplicative);
  return t => f => g => bitraverse_2(f)(g)(t);
};
const bisequence_ = dictBifoldable => dictApplicative => bitraverse_(dictBifoldable)(dictApplicative)(identity)(identity);
const bifoldl = dict => dict.bifoldl;
const bifoldableTuple = {
  bifoldMap: dictMonoid => f => g => v => dictMonoid.Semigroup0().append(f(v._1))(g(v._2)),
  bifoldr: f => g => z => v => f(v._1)(g(v._2)(z)),
  bifoldl: f => g => z => v => g(f(z)(v._1))(v._2)
};
const bifoldableJoker = dictFoldable => (
  {
    bifoldr: v => r => u => v1 => dictFoldable.foldr(r)(u)(v1),
    bifoldl: v => r => u => v1 => dictFoldable.foldl(r)(u)(v1),
    bifoldMap: dictMonoid => {
      const foldMap1 = dictFoldable.foldMap(dictMonoid);
      return v => r => v1 => foldMap1(r)(v1);
    }
  }
);
const bifoldableEither = {
  bifoldr: v => v1 => v2 => v3 => {
    if (v3.tag === "Left") { return v(v3._1)(v2); }
    if (v3.tag === "Right") { return v1(v3._1)(v2); }
    $runtime.fail();
  },
  bifoldl: v => v1 => v2 => v3 => {
    if (v3.tag === "Left") { return v(v2)(v3._1); }
    if (v3.tag === "Right") { return v1(v2)(v3._1); }
    $runtime.fail();
  },
  bifoldMap: dictMonoid => v => v1 => v2 => {
    if (v2.tag === "Left") { return v(v2._1); }
    if (v2.tag === "Right") { return v1(v2._1); }
    $runtime.fail();
  }
};
const bifoldableConst = {bifoldr: f => v => z => v1 => f(v1)(z), bifoldl: f => v => z => v1 => f(z)(v1), bifoldMap: dictMonoid => f => v => v1 => f(v1)};
const bifoldableClown = dictFoldable => (
  {
    bifoldr: l => v => u => v1 => dictFoldable.foldr(l)(u)(v1),
    bifoldl: l => v => u => v1 => dictFoldable.foldl(l)(u)(v1),
    bifoldMap: dictMonoid => {
      const foldMap1 = dictFoldable.foldMap(dictMonoid);
      return l => v => v1 => foldMap1(l)(v1);
    }
  }
);
const bifoldMapDefaultR = dictBifoldable => dictMonoid => {
  const $0 = dictMonoid.Semigroup0();
  const mempty = dictMonoid.mempty;
  return f => g => dictBifoldable.bifoldr(x => $0.append(f(x)))(x => $0.append(g(x)))(mempty);
};
const bifoldMapDefaultL = dictBifoldable => dictMonoid => {
  const $0 = dictMonoid.Semigroup0();
  const mempty = dictMonoid.mempty;
  return f => g => dictBifoldable.bifoldl(m => a => $0.append(m)(f(a)))(m => b => $0.append(m)(g(b)))(mempty);
};
const bifoldMap = dict => dict.bifoldMap;
const bifoldableFlip = dictBifoldable => (
  {
    bifoldr: r => l => u => v => dictBifoldable.bifoldr(l)(r)(u)(v),
    bifoldl: r => l => u => v => dictBifoldable.bifoldl(l)(r)(u)(v),
    bifoldMap: dictMonoid => {
      const bifoldMap2 = dictBifoldable.bifoldMap(dictMonoid);
      return r => l => v => bifoldMap2(l)(r)(v);
    }
  }
);
const bifoldlDefault = dictBifoldable => {
  const bifoldMap1 = dictBifoldable.bifoldMap(monoidDual);
  return f => g => z => p => bifoldMap1(x => a => f(a)(x))(x => a => g(a)(x))(p)(z);
};
const bifoldrDefault = dictBifoldable => {
  const bifoldMap1 = dictBifoldable.bifoldMap(monoidEndo);
  return f => g => z => p => bifoldMap1(x => f(x))(x => g(x))(p)(z);
};
const bifoldableProduct2 = dictBifoldable => dictBifoldable1 => (
  {
    bifoldr: l => r => u => m => bifoldrDefault(bifoldableProduct2(dictBifoldable)(dictBifoldable1))(l)(r)(u)(m),
    bifoldl: l => r => u => m => bifoldlDefault(bifoldableProduct2(dictBifoldable)(dictBifoldable1))(l)(r)(u)(m),
    bifoldMap: dictMonoid => {
      const bifoldMap3 = dictBifoldable.bifoldMap(dictMonoid);
      const bifoldMap4 = dictBifoldable1.bifoldMap(dictMonoid);
      return l => r => v => dictMonoid.Semigroup0().append(bifoldMap3(l)(r)(v._1))(bifoldMap4(l)(r)(v._2));
    }
  }
);
const bifold = dictBifoldable => dictMonoid => dictBifoldable.bifoldMap(dictMonoid)(identity)(identity);
const biany = dictBifoldable => dictBooleanAlgebra => {
  const bifoldMap2 = dictBifoldable.bifoldMap((() => {
    const $0 = dictBooleanAlgebra.HeytingAlgebra0();
    const semigroupDisj1 = {append: v => v1 => $0.disj(v)(v1)};
    return {mempty: $0.ff, Semigroup0: () => semigroupDisj1};
  })());
  return p => q => bifoldMap2(x => p(x))(x => q(x));
};
const biall = dictBifoldable => dictBooleanAlgebra => {
  const bifoldMap2 = dictBifoldable.bifoldMap((() => {
    const $0 = dictBooleanAlgebra.HeytingAlgebra0();
    const semigroupConj1 = {append: v => v1 => $0.conj(v)(v1)};
    return {mempty: $0.tt, Semigroup0: () => semigroupConj1};
  })());
  return p => q => bifoldMap2(x => p(x))(x => q(x));
};
export {
  
  
  
  
  
  
  
  bifoldableConst,
  bifoldableEither,
  
  
  bifoldableProduct2,
  bifoldableTuple,
  
  
  
  
  
  
  
  
  
  
};
