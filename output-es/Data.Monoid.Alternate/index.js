const Alternate = x => x;
const showAlternate = dictShow => ({show: v => "(Alternate " + dictShow.show(v) + ")"});
const semigroupAlternate = dictAlt => ({append: v => v1 => dictAlt.alt(v)(v1)});
const plusAlternate = dictPlus => dictPlus;
const ordAlternate = dictOrd => dictOrd;
const ord1Alternate = dictOrd1 => dictOrd1;
const newtypeAlternate = {Coercible0: () => {}};
const monoidAlternate = dictPlus => {
  const $0 = dictPlus.Alt0();
  const semigroupAlternate1 = {append: v => v1 => $0.alt(v)(v1)};
  return {mempty: dictPlus.empty, Semigroup0: () => semigroupAlternate1};
};
const monadAlternate = dictMonad => dictMonad;
const functorAlternate = dictFunctor => dictFunctor;
const extendAlternate = dictExtend => dictExtend;
const eqAlternate = dictEq => dictEq;
const eq1Alternate = dictEq1 => dictEq1;
const comonadAlternate = dictComonad => dictComonad;
const boundedAlternate = dictBounded => dictBounded;
const bindAlternate = dictBind => dictBind;
const applyAlternate = dictApply => dictApply;
const applicativeAlternate = dictApplicative => dictApplicative;
const alternativeAlternate = dictAlternative => dictAlternative;
const altAlternate = dictAlt => dictAlt;
export {
  Alternate,
  altAlternate,
  alternativeAlternate,
  applicativeAlternate,
  applyAlternate,
  bindAlternate,
  boundedAlternate,
  comonadAlternate,
  eq1Alternate,
  eqAlternate,
  extendAlternate,
  functorAlternate,
  monadAlternate,
  monoidAlternate,
  newtypeAlternate,
  ord1Alternate,
  ordAlternate,
  plusAlternate,
  semigroupAlternate,
  showAlternate
};
