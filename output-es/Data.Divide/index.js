import * as $runtime from "../runtime.js";
import * as Data$dComparison from "../Data.Comparison/index.js";
import * as Data$dEquivalence from "../Data.Equivalence/index.js";
import * as Data$dOp from "../Data.Op/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Data$dPredicate from "../Data.Predicate/index.js";
const identity = x => x;
const dividePredicate = {
  divide: f => v => v1 => a => {
    const v2 = f(a);
    return v(v2._1) && v1(v2._2);
  },
  Contravariant0: () => Data$dPredicate.contravariantPredicate
};
const divideOp = dictSemigroup => (
  {
    divide: f => v => v1 => a => {
      const v2 = f(a);
      return dictSemigroup.append(v(v2._1))(v1(v2._2));
    },
    Contravariant0: () => Data$dOp.contravariantOp
  }
);
const divideEquivalence = {
  divide: f => v => v1 => a => b => {
    const v2 = f(a);
    const v3 = f(b);
    return v(v2._1)(v3._1) && v1(v2._2)(v3._2);
  },
  Contravariant0: () => Data$dEquivalence.contravariantEquivalence
};
const divideComparison = {
  divide: f => v => v1 => a => b => {
    const v2 = f(a);
    const v3 = f(b);
    const $0 = v(v2._1)(v3._1);
    const $1 = v1(v2._2)(v3._2);
    if ($0 === "LT") { return Data$dOrdering.LT; }
    if ($0 === "GT") { return Data$dOrdering.GT; }
    if ($0 === "EQ") { return $1; }
    $runtime.fail();
  },
  Contravariant0: () => Data$dComparison.contravariantComparison
};
const divide = dict => dict.divide;
const divided = dictDivide => dictDivide.divide(identity);
export {divide, divideComparison, divideEquivalence, divideOp, dividePredicate, divided, identity};
