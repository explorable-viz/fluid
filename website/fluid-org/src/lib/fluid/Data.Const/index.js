const Const = x => x;
const showConst = dictShow => ({show: v => "(Const " + dictShow.show(v) + ")"});
const semiringConst = dictSemiring => dictSemiring;
const semigroupoidConst = {compose: v => v1 => v1};
const semigroupConst = dictSemigroup => dictSemigroup;
const ringConst = dictRing => dictRing;
const ordConst = dictOrd => dictOrd;
const newtypeConst = {Coercible0: () => {}};
const monoidConst = dictMonoid => dictMonoid;
const heytingAlgebraConst = dictHeytingAlgebra => dictHeytingAlgebra;
const functorConst = {map: f => m => m};
const invariantConst = {imap: f => v => m => m};
const euclideanRingConst = dictEuclideanRing => dictEuclideanRing;
const eqConst = dictEq => dictEq;
const eq1Const = dictEq => {
  const eq = dictEq.eq;
  return {eq1: dictEq1 => eq};
};
const ord1Const = dictOrd => {
  const compare = dictOrd.compare;
  const eq = dictOrd.Eq0().eq;
  return {compare1: dictOrd1 => compare, Eq10: () => ({eq1: dictEq1 => eq})};
};
const commutativeRingConst = dictCommutativeRing => dictCommutativeRing;
const boundedConst = dictBounded => dictBounded;
const booleanAlgebraConst = dictBooleanAlgebra => dictBooleanAlgebra;
const applyConst = dictSemigroup => ({apply: v => v1 => dictSemigroup.append(v)(v1), Functor0: () => functorConst});
const applicativeConst = dictMonoid => {
  const mempty = dictMonoid.mempty;
  const $0 = dictMonoid.Semigroup0();
  const applyConst1 = {apply: v => v1 => $0.append(v)(v1), Functor0: () => functorConst};
  return {pure: v => mempty, Apply0: () => applyConst1};
};
export {
  Const,
  
  
  
  
  
  
  
  
  functorConst,
  
  
  
  
  
  
  
  
  
  
  
};
