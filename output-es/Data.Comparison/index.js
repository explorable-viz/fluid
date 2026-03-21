import * as $runtime from "../runtime.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
const Comparison = x => x;
const semigroupComparison = {
  append: v => v1 => x => {
    const $0 = v(x);
    const $1 = v1(x);
    return x$1 => {
      const $2 = $0(x$1);
      const $3 = $1(x$1);
      if ($2 === "LT") { return Data$dOrdering.LT; }
      if ($2 === "GT") { return Data$dOrdering.GT; }
      if ($2 === "EQ") { return $3; }
      $runtime.fail();
    };
  }
};
const newtypeComparison = {Coercible0: () => {}};
const monoidComparison = {mempty: v => v1 => Data$dOrdering.EQ, Semigroup0: () => semigroupComparison};
const defaultComparison = dictOrd => dictOrd.compare;
const contravariantComparison = {cmap: f => v => x => y => v(f(x))(f(y))};
export {Comparison, contravariantComparison, defaultComparison, monoidComparison, newtypeComparison, semigroupComparison};
