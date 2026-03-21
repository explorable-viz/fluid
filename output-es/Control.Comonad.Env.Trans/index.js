// | This module defines the environment comonad transformer, `EnvT`.
import * as Data$dTuple from "../Data.Tuple/index.js";
const EnvT = x => x;
const withEnvT = f => v => Data$dTuple.$Tuple(f(v._1), v._2);
const runEnvT = v => v;
const newtypeEnvT = {Coercible0: () => {}};
const mapEnvT = f => v => Data$dTuple.$Tuple(v._1, f(v._2));
const functorEnvT = dictFunctor => ({map: f => v => Data$dTuple.$Tuple(v._1, dictFunctor.map(f)(v._2))});
const functorWithIndexEnvT = dictFunctorWithIndex => {
  const $0 = dictFunctorWithIndex.Functor0();
  const functorEnvT1 = {map: f => v => Data$dTuple.$Tuple(v._1, $0.map(f)(v._2))};
  return {mapWithIndex: f => v => Data$dTuple.$Tuple(v._1, dictFunctorWithIndex.mapWithIndex(f)(v._2)), Functor0: () => functorEnvT1};
};
const foldableEnvT = dictFoldable => (
  {
    foldl: fn => a => v => dictFoldable.foldl(fn)(a)(v._2),
    foldr: fn => a => v => dictFoldable.foldr(fn)(a)(v._2),
    foldMap: dictMonoid => {
      const foldMap1 = dictFoldable.foldMap(dictMonoid);
      return fn => v => foldMap1(fn)(v._2);
    }
  }
);
const foldableWithIndexEnvT = dictFoldableWithIndex => {
  const $0 = dictFoldableWithIndex.Foldable0();
  const foldableEnvT1 = {
    foldl: fn => a => v => $0.foldl(fn)(a)(v._2),
    foldr: fn => a => v => $0.foldr(fn)(a)(v._2),
    foldMap: dictMonoid => {
      const foldMap1 = $0.foldMap(dictMonoid);
      return fn => v => foldMap1(fn)(v._2);
    }
  };
  return {
    foldlWithIndex: f => a => v => dictFoldableWithIndex.foldlWithIndex(f)(a)(v._2),
    foldrWithIndex: f => a => v => dictFoldableWithIndex.foldrWithIndex(f)(a)(v._2),
    foldMapWithIndex: dictMonoid => {
      const foldMapWithIndex1 = dictFoldableWithIndex.foldMapWithIndex(dictMonoid);
      return f => v => foldMapWithIndex1(f)(v._2);
    },
    Foldable0: () => foldableEnvT1
  };
};
const traversableEnvT = dictTraversable => {
  const $0 = dictTraversable.Functor0();
  const functorEnvT1 = {map: f => v => Data$dTuple.$Tuple(v._1, $0.map(f)(v._2))};
  const $1 = dictTraversable.Foldable1();
  const foldableEnvT1 = {
    foldl: fn => a => v => $1.foldl(fn)(a)(v._2),
    foldr: fn => a => v => $1.foldr(fn)(a)(v._2),
    foldMap: dictMonoid => {
      const foldMap1 = $1.foldMap(dictMonoid);
      return fn => v => foldMap1(fn)(v._2);
    }
  };
  return {
    sequence: dictApplicative => {
      const sequence1 = dictTraversable.sequence(dictApplicative);
      return v => dictApplicative.Apply0().Functor0().map(Data$dTuple.Tuple(v._1))(sequence1(v._2));
    },
    traverse: dictApplicative => {
      const traverse1 = dictTraversable.traverse(dictApplicative);
      return f => v => dictApplicative.Apply0().Functor0().map(Data$dTuple.Tuple(v._1))(traverse1(f)(v._2));
    },
    Functor0: () => functorEnvT1,
    Foldable1: () => foldableEnvT1
  };
};
const traversableWithIndexEnvT = dictTraversableWithIndex => {
  const $0 = dictTraversableWithIndex.FunctorWithIndex0();
  const $1 = $0.Functor0();
  const functorWithIndexEnvT1 = (() => {
    const functorEnvT1 = {map: f => v => Data$dTuple.$Tuple(v._1, $1.map(f)(v._2))};
    return {mapWithIndex: f => v => Data$dTuple.$Tuple(v._1, $0.mapWithIndex(f)(v._2)), Functor0: () => functorEnvT1};
  })();
  const foldableWithIndexEnvT1 = foldableWithIndexEnvT(dictTraversableWithIndex.FoldableWithIndex1());
  const traversableEnvT1 = traversableEnvT(dictTraversableWithIndex.Traversable2());
  return {
    traverseWithIndex: dictApplicative => {
      const traverseWithIndex1 = dictTraversableWithIndex.traverseWithIndex(dictApplicative);
      return f => v => dictApplicative.Apply0().Functor0().map(Data$dTuple.Tuple(v._1))(traverseWithIndex1(f)(v._2));
    },
    FunctorWithIndex0: () => functorWithIndexEnvT1,
    FoldableWithIndex1: () => foldableWithIndexEnvT1,
    Traversable2: () => traversableEnvT1
  };
};
const extendEnvT = dictExtend => {
  const Functor0 = dictExtend.Functor0();
  const functorEnvT1 = {map: f => v => Data$dTuple.$Tuple(v._1, Functor0.map(f)(v._2))};
  return {extend: f => v => Data$dTuple.$Tuple(v._1, Functor0.map(f)(dictExtend.extend(Data$dTuple.Tuple(v._1))(v._2))), Functor0: () => functorEnvT1};
};
const comonadTransEnvT = {lower: dictComonad => v => v._2};
const comonadEnvT = dictComonad => {
  const extendEnvT1 = extendEnvT(dictComonad.Extend0());
  return {extract: v => dictComonad.extract(v._2), Extend0: () => extendEnvT1};
};
export {
  EnvT,
  comonadEnvT,
  comonadTransEnvT,
  extendEnvT,
  foldableEnvT,
  foldableWithIndexEnvT,
  functorEnvT,
  functorWithIndexEnvT,
  mapEnvT,
  newtypeEnvT,
  runEnvT,
  traversableEnvT,
  traversableWithIndexEnvT,
  withEnvT
};
