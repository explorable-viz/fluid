const Additive = x => x;
const showAdditive = dictShow => ({show: v => "(Additive " + dictShow.show(v) + ")"});
const semigroupAdditive = dictSemiring => ({append: v => v1 => dictSemiring.add(v)(v1)});
const ordAdditive = dictOrd => dictOrd;
const monoidAdditive = dictSemiring => {
  const semigroupAdditive1 = {append: v => v1 => dictSemiring.add(v)(v1)};
  return {mempty: dictSemiring.zero, Semigroup0: () => semigroupAdditive1};
};
const functorAdditive = {map: f => m => f(m)};
const eqAdditive = dictEq => dictEq;
const eq1Additive = {eq1: dictEq => dictEq.eq};
const ord1Additive = {compare1: dictOrd => dictOrd.compare, Eq10: () => eq1Additive};
const boundedAdditive = dictBounded => dictBounded;
const applyAdditive = {apply: v => v1 => v(v1), Functor0: () => functorAdditive};
const bindAdditive = {bind: v => f => f(v), Apply0: () => applyAdditive};
const applicativeAdditive = {pure: Additive, Apply0: () => applyAdditive};
const monadAdditive = {Applicative0: () => applicativeAdditive, Bind1: () => bindAdditive};
export {
  Additive,
  
  
  
  
  
  
  functorAdditive,
  
  
  
  
  
  
};
