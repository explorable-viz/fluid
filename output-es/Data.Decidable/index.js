import * as Data$dDecide from "../Data.Decide/index.js";
import * as Data$dDivisible from "../Data.Divisible/index.js";
const identity = x => x;
const lose = dict => dict.lose;
const lost = dictDecidable => dictDecidable.lose(identity);
const decidablePredicate = {
  lose: f => a => {
    const spin = spin$a0$copy => {
      let spin$a0 = spin$a0$copy, spin$c = true, spin$r;
      while (spin$c) {
        const v = spin$a0;
        spin$a0 = v;
      }
      return spin$r;
    };
    return spin(f(a));
  },
  Decide0: () => Data$dDecide.choosePredicate,
  Divisible1: () => Data$dDivisible.divisiblePredicate
};
const decidableOp = dictMonoid => {
  const chooseOp = Data$dDecide.chooseOp(dictMonoid.Semigroup0());
  const divisibleOp = Data$dDivisible.divisibleOp(dictMonoid);
  return {
    lose: f => a => {
      const spin = spin$a0$copy => {
        let spin$a0 = spin$a0$copy, spin$c = true, spin$r;
        while (spin$c) {
          const v = spin$a0;
          spin$a0 = v;
        }
        return spin$r;
      };
      return spin(f(a));
    },
    Decide0: () => chooseOp,
    Divisible1: () => divisibleOp
  };
};
const decidableEquivalence = {
  lose: f => a => {
    const spin = spin$a0$copy => {
      let spin$a0 = spin$a0$copy, spin$c = true, spin$r;
      while (spin$c) {
        const v = spin$a0;
        spin$a0 = v;
      }
      return spin$r;
    };
    return spin(f(a));
  },
  Decide0: () => Data$dDecide.chooseEquivalence,
  Divisible1: () => Data$dDivisible.divisibleEquivalence
};
const decidableComparison = {
  lose: f => a => v => {
    const spin = spin$a0$copy => {
      let spin$a0 = spin$a0$copy, spin$c = true, spin$r;
      while (spin$c) {
        const v$1 = spin$a0;
        spin$a0 = v$1;
      }
      return spin$r;
    };
    return spin(f(a));
  },
  Decide0: () => Data$dDecide.chooseComparison,
  Divisible1: () => Data$dDivisible.divisibleComparison
};
export {decidableComparison, decidableEquivalence, decidableOp, decidablePredicate, identity, lose, lost};
