const Disj = x => x;
const showDisj = dictShow => ({show: v => "(Disj " + dictShow.show(v) + ")"});
const semiringDisj = dictHeytingAlgebra => (
  {zero: dictHeytingAlgebra.ff, one: dictHeytingAlgebra.tt, add: v => v1 => dictHeytingAlgebra.disj(v)(v1), mul: v => v1 => dictHeytingAlgebra.conj(v)(v1)}
);
const semigroupDisj = dictHeytingAlgebra => ({append: v => v1 => dictHeytingAlgebra.disj(v)(v1)});
const ordDisj = dictOrd => dictOrd;
const monoidDisj = dictHeytingAlgebra => {
  const semigroupDisj1 = {append: v => v1 => dictHeytingAlgebra.disj(v)(v1)};
  return {mempty: dictHeytingAlgebra.ff, Semigroup0: () => semigroupDisj1};
};
const functorDisj = {map: f => m => f(m)};
const eqDisj = dictEq => dictEq;
const eq1Disj = {eq1: dictEq => dictEq.eq};
const ord1Disj = {compare1: dictOrd => dictOrd.compare, Eq10: () => eq1Disj};
const boundedDisj = dictBounded => dictBounded;
const applyDisj = {apply: v => v1 => v(v1), Functor0: () => functorDisj};
const bindDisj = {bind: v => f => f(v), Apply0: () => applyDisj};
const applicativeDisj = {pure: Disj, Apply0: () => applyDisj};
const monadDisj = {Applicative0: () => applicativeDisj, Bind1: () => bindDisj};
export {Disj,       functorDisj,       };
