const Conj = x => x;
const showConj = dictShow => ({show: v => "(Conj " + dictShow.show(v) + ")"});
const semiringConj = dictHeytingAlgebra => (
  {zero: dictHeytingAlgebra.tt, one: dictHeytingAlgebra.ff, add: v => v1 => dictHeytingAlgebra.conj(v)(v1), mul: v => v1 => dictHeytingAlgebra.disj(v)(v1)}
);
const semigroupConj = dictHeytingAlgebra => ({append: v => v1 => dictHeytingAlgebra.conj(v)(v1)});
const ordConj = dictOrd => dictOrd;
const monoidConj = dictHeytingAlgebra => {
  const semigroupConj1 = {append: v => v1 => dictHeytingAlgebra.conj(v)(v1)};
  return {mempty: dictHeytingAlgebra.tt, Semigroup0: () => semigroupConj1};
};
const functorConj = {map: f => m => f(m)};
const eqConj = dictEq => dictEq;
const eq1Conj = {eq1: dictEq => dictEq.eq};
const ord1Conj = {compare1: dictOrd => dictOrd.compare, Eq10: () => eq1Conj};
const boundedConj = dictBounded => dictBounded;
const applyConj = {apply: v => v1 => v(v1), Functor0: () => functorConj};
const bindConj = {bind: v => f => f(v), Apply0: () => applyConj};
const applicativeConj = {pure: Conj, Apply0: () => applyConj};
const monadConj = {Applicative0: () => applicativeConj, Bind1: () => bindConj};
export {Conj,       functorConj,       };
