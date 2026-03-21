import * as Data$dIdentity from "../Data.Identity/index.js";
import * as Data$dMonoid$dDual from "../Data.Monoid.Dual/index.js";
import * as Data$dMonoid$dMultiplicative from "../Data.Monoid.Multiplicative/index.js";
import * as Data$dSemigroup$dFoldable from "../Data.Semigroup.Foldable/index.js";
import * as Data$dTraversable from "../Data.Traversable/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const identity = x => x;
const traverse1 = dict => dict.traverse1;
const traversableTuple = {
  traverse1: dictApply => f => v => dictApply.Functor0().map(Data$dTuple.Tuple(v._1))(f(v._2)),
  sequence1: dictApply => v => dictApply.Functor0().map(Data$dTuple.Tuple(v._1))(v._2),
  Foldable10: () => Data$dSemigroup$dFoldable.foldableTuple,
  Traversable1: () => Data$dTraversable.traversableTuple
};
const traversableIdentity = {
  traverse1: dictApply => f => v => dictApply.Functor0().map(Data$dIdentity.Identity)(f(v)),
  sequence1: dictApply => v => dictApply.Functor0().map(Data$dIdentity.Identity)(v),
  Foldable10: () => Data$dSemigroup$dFoldable.foldableIdentity,
  Traversable1: () => Data$dTraversable.traversableIdentity
};
const sequence1Default = dictTraversable1 => dictApply => dictTraversable1.traverse1(dictApply)(identity);
const traversableDual = {
  traverse1: dictApply => f => v => dictApply.Functor0().map(Data$dMonoid$dDual.Dual)(f(v)),
  sequence1: dictApply => traversableDual.traverse1(dictApply)(identity),
  Foldable10: () => Data$dSemigroup$dFoldable.foldableDual,
  Traversable1: () => Data$dTraversable.traversableDual
};
const traversableMultiplicative = {
  traverse1: dictApply => f => v => dictApply.Functor0().map(Data$dMonoid$dMultiplicative.Multiplicative)(f(v)),
  sequence1: dictApply => traversableMultiplicative.traverse1(dictApply)(identity),
  Foldable10: () => Data$dSemigroup$dFoldable.foldableMultiplicative,
  Traversable1: () => Data$dTraversable.traversableMultiplicative
};
const sequence1 = dict => dict.sequence1;
const traverse1Default = dictTraversable1 => dictApply => {
  const sequence12 = dictTraversable1.sequence1(dictApply);
  return f => ta => sequence12(dictTraversable1.Traversable1().Functor0().map(f)(ta));
};
export {identity,        };
