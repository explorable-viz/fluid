const Identity = x => x;
const showIdentity = dictShow => ({show: v => "(Identity " + dictShow.show(v) + ")"});
const semiringIdentity = dictSemiring => dictSemiring;
const semigroupIdentity = dictSemigroup => dictSemigroup;
const ringIdentity = dictRing => dictRing;
const ordIdentity = dictOrd => dictOrd;
const newtypeIdentity = {Coercible0: () => {}};
const monoidIdentity = dictMonoid => dictMonoid;
const lazyIdentity = dictLazy => dictLazy;
const heytingAlgebraIdentity = dictHeytingAlgebra => dictHeytingAlgebra;
const functorIdentity = {map: f => m => f(m)};
const invariantIdentity = {imap: f => v => m => f(m)};
const extendIdentity = {extend: f => m => f(m), Functor0: () => functorIdentity};
const euclideanRingIdentity = dictEuclideanRing => dictEuclideanRing;
const eqIdentity = dictEq => dictEq;
const eq1Identity = {eq1: dictEq => dictEq.eq};
const ord1Identity = {compare1: dictOrd => dictOrd.compare, Eq10: () => eq1Identity};
const comonadIdentity = {extract: v => v, Extend0: () => extendIdentity};
const commutativeRingIdentity = dictCommutativeRing => dictCommutativeRing;
const boundedIdentity = dictBounded => dictBounded;
const booleanAlgebraIdentity = dictBooleanAlgebra => dictBooleanAlgebra;
const applyIdentity = {apply: v => v1 => v(v1), Functor0: () => functorIdentity};
const bindIdentity = {bind: v => f => f(v), Apply0: () => applyIdentity};
const applicativeIdentity = {pure: Identity, Apply0: () => applyIdentity};
const monadIdentity = {Applicative0: () => applicativeIdentity, Bind1: () => bindIdentity};
const altIdentity = {alt: x => v => x, Functor0: () => functorIdentity};
export {
  Identity,
  
  
  
  
  
  
  
  
  
  
  
  
  functorIdentity,
  
  
  
  monadIdentity,
  
  
  
  
  
  
  
  
};
