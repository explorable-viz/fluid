import * as Data$dDivide from "../Data.Divide/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
const divisiblePredicate = {conquer: v => true, Divide0: () => Data$dDivide.dividePredicate};
const divisibleOp = dictMonoid => {
  const divideOp = Data$dDivide.divideOp(dictMonoid.Semigroup0());
  return {
    conquer: (() => {
      const $0 = dictMonoid.mempty;
      return v => $0;
    })(),
    Divide0: () => divideOp
  };
};
const divisibleEquivalence = {conquer: v => v1 => true, Divide0: () => Data$dDivide.divideEquivalence};
const divisibleComparison = {conquer: v => v1 => Data$dOrdering.EQ, Divide0: () => Data$dDivide.divideComparison};
const conquer = dict => dict.conquer;
export {conquer, divisibleComparison, divisibleEquivalence, divisibleOp, divisiblePredicate};
