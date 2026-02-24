const Multiplicative = x => x;
const showMultiplicative = dictShow => ({show: v => "(Multiplicative " + dictShow.show(v) + ")"});
const semigroupMultiplicative = dictSemiring => ({append: v => v1 => dictSemiring.mul(v)(v1)});
const ordMultiplicative = dictOrd => dictOrd;
const monoidMultiplicative = dictSemiring => {
  const semigroupMultiplicative1 = {append: v => v1 => dictSemiring.mul(v)(v1)};
  return {mempty: dictSemiring.one, Semigroup0: () => semigroupMultiplicative1};
};
const functorMultiplicative = {map: f => m => f(m)};
const eqMultiplicative = dictEq => dictEq;
const eq1Multiplicative = {eq1: dictEq => dictEq.eq};
const ord1Multiplicative = {compare1: dictOrd => dictOrd.compare, Eq10: () => eq1Multiplicative};
const boundedMultiplicative = dictBounded => dictBounded;
const applyMultiplicative = {apply: v => v1 => v(v1), Functor0: () => functorMultiplicative};
const bindMultiplicative = {bind: v => f => f(v), Apply0: () => applyMultiplicative};
const applicativeMultiplicative = {pure: Multiplicative, Apply0: () => applyMultiplicative};
const monadMultiplicative = {Applicative0: () => applicativeMultiplicative, Bind1: () => bindMultiplicative};
export {
  Multiplicative,
  
  
  
  
  
  
  functorMultiplicative,
  
  
  
  
  
  
};
