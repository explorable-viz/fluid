const Dual = x => x;
const showDual = dictShow => ({show: v => "(Dual " + dictShow.show(v) + ")"});
const semigroupDual = dictSemigroup => ({append: v => v1 => dictSemigroup.append(v1)(v)});
const ordDual = dictOrd => dictOrd;
const monoidDual = dictMonoid => {
  const $0 = dictMonoid.Semigroup0();
  const semigroupDual1 = {append: v => v1 => $0.append(v1)(v)};
  return {mempty: dictMonoid.mempty, Semigroup0: () => semigroupDual1};
};
const functorDual = {map: f => m => f(m)};
const eqDual = dictEq => dictEq;
const eq1Dual = {eq1: dictEq => dictEq.eq};
const ord1Dual = {compare1: dictOrd => dictOrd.compare, Eq10: () => eq1Dual};
const boundedDual = dictBounded => dictBounded;
const applyDual = {apply: v => v1 => v(v1), Functor0: () => functorDual};
const bindDual = {bind: v => f => f(v), Apply0: () => applyDual};
const applicativeDual = {pure: Dual, Apply0: () => applyDual};
const monadDual = {Applicative0: () => applicativeDual, Bind1: () => bindDual};
export {Dual,       functorDual,      };
