import * as Data$dFoldable from "../Data.Foldable/index.js";
const identity = x => x;
const parTraverse_ = dictParallel => dictApplicative => {
  const traverse_ = Data$dFoldable.traverse_(dictApplicative);
  return dictFoldable => {
    const traverse_1 = traverse_(dictFoldable);
    return f => {
      const $0 = traverse_1(x => dictParallel.parallel(f(x)));
      return x => dictParallel.sequential($0(x));
    };
  };
};
const parTraverse = dictParallel => dictApplicative => dictTraversable => {
  const traverse = dictTraversable.traverse(dictApplicative);
  return f => {
    const $0 = traverse(x => dictParallel.parallel(f(x)));
    return x => dictParallel.sequential($0(x));
  };
};
const parSequence_ = dictParallel => dictApplicative => {
  const parTraverse_2 = parTraverse_(dictParallel)(dictApplicative);
  return dictFoldable => parTraverse_2(dictFoldable)(identity);
};
const parSequence = dictParallel => dictApplicative => dictTraversable => {
  const $0 = dictTraversable.traverse(dictApplicative)(x => dictParallel.parallel(x));
  return x => dictParallel.sequential($0(x));
};
const parOneOfMap = dictParallel => dictAlternative => {
  const Plus1 = dictAlternative.Plus1();
  return dictFoldable => {
    const empty = Plus1.empty;
    return dictFunctor => f => {
      const $0 = dictFoldable.foldr(x => Plus1.Alt0().alt(dictParallel.parallel(f(x))))(empty);
      return x => dictParallel.sequential($0(x));
    };
  };
};
const parOneOf = dictParallel => dictAlternative => {
  const Plus1 = dictAlternative.Plus1();
  return dictFoldable => {
    const empty = Plus1.empty;
    return dictFunctor => {
      const $0 = dictFoldable.foldr(x => Plus1.Alt0().alt(dictParallel.parallel(x)))(empty);
      return x => dictParallel.sequential($0(x));
    };
  };
};
const parApply = dictParallel => mf => ma => dictParallel.sequential(dictParallel.Apply1().apply(dictParallel.parallel(mf))(dictParallel.parallel(ma)));
export {identity,       parTraverse_};
