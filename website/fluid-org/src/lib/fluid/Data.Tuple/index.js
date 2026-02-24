// | A data type and functions for working with ordered pairs.
import * as Data$dGeneric$dRep from "../Data.Generic.Rep/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
const $Tuple = (_1, _2) => ({tag: "Tuple", _1, _2});
const Tuple = value0 => value1 => $Tuple(value0, value1);
const uncurry = f => v => f(v._1)(v._2);
const swap = v => $Tuple(v._2, v._1);
const snd = v => v._2;
const showTuple = dictShow => dictShow1 => ({show: v => "(Tuple " + dictShow.show(v._1) + " " + dictShow1.show(v._2) + ")"});
const semiringTuple = dictSemiring => {
  const one = dictSemiring.one;
  const zero = dictSemiring.zero;
  return dictSemiring1 => (
    {
      add: v => v1 => $Tuple(dictSemiring.add(v._1)(v1._1), dictSemiring1.add(v._2)(v1._2)),
      one: $Tuple(one, dictSemiring1.one),
      mul: v => v1 => $Tuple(dictSemiring.mul(v._1)(v1._1), dictSemiring1.mul(v._2)(v1._2)),
      zero: $Tuple(zero, dictSemiring1.zero)
    }
  );
};
const semigroupoidTuple = {compose: v => v1 => $Tuple(v1._1, v._2)};
const semigroupTuple = dictSemigroup => dictSemigroup1 => ({append: v => v1 => $Tuple(dictSemigroup.append(v._1)(v1._1), dictSemigroup1.append(v._2)(v1._2))});
const ringTuple = dictRing => {
  const $0 = dictRing.Semiring0();
  const one = $0.one;
  const semiringTuple1 = (() => {
    const zero = $0.zero;
    return dictSemiring1 => (
      {
        add: v => v1 => $Tuple($0.add(v._1)(v1._1), dictSemiring1.add(v._2)(v1._2)),
        one: $Tuple(one, dictSemiring1.one),
        mul: v => v1 => $Tuple($0.mul(v._1)(v1._1), dictSemiring1.mul(v._2)(v1._2)),
        zero: $Tuple(zero, dictSemiring1.zero)
      }
    );
  })();
  return dictRing1 => {
    const semiringTuple2 = semiringTuple1(dictRing1.Semiring0());
    return {sub: v => v1 => $Tuple(dictRing.sub(v._1)(v1._1), dictRing1.sub(v._2)(v1._2)), Semiring0: () => semiringTuple2};
  };
};
const monoidTuple = dictMonoid => {
  const mempty = dictMonoid.mempty;
  const $0 = dictMonoid.Semigroup0();
  return dictMonoid1 => {
    const $1 = dictMonoid1.Semigroup0();
    const semigroupTuple2 = {append: v => v1 => $Tuple($0.append(v._1)(v1._1), $1.append(v._2)(v1._2))};
    return {mempty: $Tuple(mempty, dictMonoid1.mempty), Semigroup0: () => semigroupTuple2};
  };
};
const heytingAlgebraTuple = dictHeytingAlgebra => {
  const tt = dictHeytingAlgebra.tt;
  const ff = dictHeytingAlgebra.ff;
  return dictHeytingAlgebra1 => (
    {
      tt: $Tuple(tt, dictHeytingAlgebra1.tt),
      ff: $Tuple(ff, dictHeytingAlgebra1.ff),
      implies: v => v1 => $Tuple(dictHeytingAlgebra.implies(v._1)(v1._1), dictHeytingAlgebra1.implies(v._2)(v1._2)),
      conj: v => v1 => $Tuple(dictHeytingAlgebra.conj(v._1)(v1._1), dictHeytingAlgebra1.conj(v._2)(v1._2)),
      disj: v => v1 => $Tuple(dictHeytingAlgebra.disj(v._1)(v1._1), dictHeytingAlgebra1.disj(v._2)(v1._2)),
      not: v => $Tuple(dictHeytingAlgebra.not(v._1), dictHeytingAlgebra1.not(v._2))
    }
  );
};
const genericTuple = {to: x => $Tuple(x._1, x._2), from: x => Data$dGeneric$dRep.$Product(x._1, x._2)};
const functorTuple = {map: f => m => $Tuple(m._1, f(m._2))};
const invariantTuple = {imap: f => v => m => $Tuple(m._1, f(m._2))};
const fst = v => v._1;
const lazyTuple = dictLazy => dictLazy1 => ({defer: f => $Tuple(dictLazy.defer(v => f()._1), dictLazy1.defer(v => f()._2))});
const extendTuple = {extend: f => v => $Tuple(v._1, f(v)), Functor0: () => functorTuple};
const eqTuple = dictEq => dictEq1 => ({eq: x => y => dictEq.eq(x._1)(y._1) && dictEq1.eq(x._2)(y._2)});
const ordTuple = dictOrd => {
  const $0 = dictOrd.Eq0();
  return dictOrd1 => {
    const $1 = dictOrd1.Eq0();
    const eqTuple2 = {eq: x => y => $0.eq(x._1)(y._1) && $1.eq(x._2)(y._2)};
    return {
      compare: x => y => {
        const v = dictOrd.compare(x._1)(y._1);
        if (v === "LT") { return Data$dOrdering.LT; }
        if (v === "GT") { return Data$dOrdering.GT; }
        return dictOrd1.compare(x._2)(y._2);
      },
      Eq0: () => eqTuple2
    };
  };
};
const eq1Tuple = dictEq => ({eq1: dictEq1 => x => y => dictEq.eq(x._1)(y._1) && dictEq1.eq(x._2)(y._2)});
const ord1Tuple = dictOrd => {
  const $0 = dictOrd.Eq0();
  const eq1Tuple1 = {eq1: dictEq1 => x => y => $0.eq(x._1)(y._1) && dictEq1.eq(x._2)(y._2)};
  return {
    compare1: dictOrd1 => x => y => {
      const v = dictOrd.compare(x._1)(y._1);
      if (v === "LT") { return Data$dOrdering.LT; }
      if (v === "GT") { return Data$dOrdering.GT; }
      return dictOrd1.compare(x._2)(y._2);
    },
    Eq10: () => eq1Tuple1
  };
};
const curry = f => a => b => f($Tuple(a, b));
const comonadTuple = {extract: snd, Extend0: () => extendTuple};
const commutativeRingTuple = dictCommutativeRing => {
  const ringTuple1 = ringTuple(dictCommutativeRing.Ring0());
  return dictCommutativeRing1 => {
    const ringTuple2 = ringTuple1(dictCommutativeRing1.Ring0());
    return {Ring0: () => ringTuple2};
  };
};
const boundedTuple = dictBounded => {
  const top = dictBounded.top;
  const bottom = dictBounded.bottom;
  const $0 = dictBounded.Ord0();
  const $1 = $0.Eq0();
  return dictBounded1 => {
    const $2 = dictBounded1.Ord0();
    const $3 = $2.Eq0();
    const ordTuple2 = (() => {
      const eqTuple2 = {eq: x => y => $1.eq(x._1)(y._1) && $3.eq(x._2)(y._2)};
      return {
        compare: x => y => {
          const v = $0.compare(x._1)(y._1);
          if (v === "LT") { return Data$dOrdering.LT; }
          if (v === "GT") { return Data$dOrdering.GT; }
          return $2.compare(x._2)(y._2);
        },
        Eq0: () => eqTuple2
      };
    })();
    return {top: $Tuple(top, dictBounded1.top), bottom: $Tuple(bottom, dictBounded1.bottom), Ord0: () => ordTuple2};
  };
};
const booleanAlgebraTuple = dictBooleanAlgebra => {
  const heytingAlgebraTuple1 = heytingAlgebraTuple(dictBooleanAlgebra.HeytingAlgebra0());
  return dictBooleanAlgebra1 => {
    const heytingAlgebraTuple2 = heytingAlgebraTuple1(dictBooleanAlgebra1.HeytingAlgebra0());
    return {HeytingAlgebra0: () => heytingAlgebraTuple2};
  };
};
const applyTuple = dictSemigroup => ({apply: v => v1 => $Tuple(dictSemigroup.append(v._1)(v1._1), v._2(v1._2)), Functor0: () => functorTuple});
const bindTuple = dictSemigroup => {
  const applyTuple1 = applyTuple(dictSemigroup);
  return {
    bind: v => f => {
      const v1 = f(v._2);
      return $Tuple(dictSemigroup.append(v._1)(v1._1), v1._2);
    },
    Apply0: () => applyTuple1
  };
};
const applicativeTuple = dictMonoid => {
  const applyTuple1 = applyTuple(dictMonoid.Semigroup0());
  return {pure: Tuple(dictMonoid.mempty), Apply0: () => applyTuple1};
};
const monadTuple = dictMonoid => {
  const applicativeTuple1 = applicativeTuple(dictMonoid);
  const bindTuple1 = bindTuple(dictMonoid.Semigroup0());
  return {Applicative0: () => applicativeTuple1, Bind1: () => bindTuple1};
};
export {
  $Tuple,
  Tuple,
  
  
  
  
  
  
  
  
  
  
  
  fst,
  functorTuple,
  
  
  
  
  
  
  
  
  
  
  
  
  
  snd,
  swap,
  
};
