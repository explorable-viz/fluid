import * as Control$dSemigroupoid from "../Control.Semigroupoid/index.js";
const identity = x => x;
const Yoneda = x => x;
const runYoneda = v => k => v(k);
const lowerYoneda = v => v(identity);
const liftYoneda = dictFunctor => m => k => dictFunctor.map(k)(m);
const monadTransYoneda = {
  lift: dictMonad => {
    const $0 = dictMonad.Bind1().Apply0().Functor0();
    return m => k => $0.map(k)(m);
  }
};
const hoistYoneda = nat => v => x => nat(v(x));
const functorYoneda = {map: f => m => k => m(x => k(f(x)))};
const extendYoneda = dictExtend => {
  const $0 = dictExtend.Functor0();
  return {extend: f => v => k => dictExtend.extend(x => k(f(k$1 => $0.map(k$1)(x))))(v(identity)), Functor0: () => functorYoneda};
};
const eqYoneda = dictEq1 => dictEq => {
  const eq11 = dictEq1.eq1(dictEq);
  return {eq: x => y => eq11(x(identity))(y(identity))};
};
const ordYoneda = dictOrd1 => {
  const eqYoneda1 = eqYoneda(dictOrd1.Eq10());
  return dictOrd => {
    const compare11 = dictOrd1.compare1(dictOrd);
    const eqYoneda2 = eqYoneda1(dictOrd.Eq0());
    return {compare: x => y => compare11(x(identity))(y(identity)), Eq0: () => eqYoneda2};
  };
};
const eq1Yoneda = dictEq1 => ({eq1: dictEq => eqYoneda(dictEq1)(dictEq).eq});
const ord1Yoneda = dictOrd1 => {
  const ordYoneda1 = ordYoneda(dictOrd1);
  const $0 = dictOrd1.Eq10();
  const eq1Yoneda1 = {eq1: dictEq => eqYoneda($0)(dictEq).eq};
  return {compare1: dictOrd => ordYoneda1(dictOrd).compare, Eq10: () => eq1Yoneda1};
};
const comonadYoneda = dictComonad => {
  const extendYoneda1 = extendYoneda(dictComonad.Extend0());
  return {extract: x => dictComonad.extract(x(identity)), Extend0: () => extendYoneda1};
};
const applyYoneda = dictApply => ({apply: v => v1 => k => dictApply.apply(v(Control$dSemigroupoid.semigroupoidFn.compose(k)))(v1(identity)), Functor0: () => functorYoneda});
const bindYoneda = dictBind => {
  const applyYoneda1 = applyYoneda(dictBind.Apply0());
  return {bind: v => g => k => dictBind.bind(v(identity))(a => g(a)(k)), Apply0: () => applyYoneda1};
};
const applicativeYoneda = dictApplicative => {
  const Apply0 = dictApplicative.Apply0();
  const applyYoneda1 = applyYoneda(Apply0);
  return {
    pure: (() => {
      const $0 = Apply0.Functor0();
      return x => {
        const $1 = dictApplicative.pure(x);
        return k => $0.map(k)($1);
      };
    })(),
    Apply0: () => applyYoneda1
  };
};
const monadYoneda = dictMonad => {
  const applicativeYoneda1 = applicativeYoneda(dictMonad.Applicative0());
  const bindYoneda1 = bindYoneda(dictMonad.Bind1());
  return {Applicative0: () => applicativeYoneda1, Bind1: () => bindYoneda1};
};
export {
  Yoneda,
  applicativeYoneda,
  applyYoneda,
  bindYoneda,
  comonadYoneda,
  eq1Yoneda,
  eqYoneda,
  extendYoneda,
  functorYoneda,
  hoistYoneda,
  identity,
  liftYoneda,
  lowerYoneda,
  monadTransYoneda,
  monadYoneda,
  ord1Yoneda,
  ordYoneda,
  runYoneda
};
