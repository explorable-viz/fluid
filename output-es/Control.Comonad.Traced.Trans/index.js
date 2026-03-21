// | This module defines the cowriter comonad transformer, `TracedT`.
const TracedT = x => x;
const runTracedT = v => v;
const newtypeTracedT = {Coercible0: () => {}};
const functorTracedT = dictFunctor => ({map: f => v => dictFunctor.map(g => t => f(g(t)))(v)});
const extendTracedT = dictExtend => {
  const Functor0 = dictExtend.Functor0();
  const functorTracedT1 = {map: f => v => Functor0.map(g => t => f(g(t)))(v)};
  return dictSemigroup => ({extend: f => v => dictExtend.extend(w$p => t => f(Functor0.map(h => t$p => h(dictSemigroup.append(t)(t$p)))(w$p)))(v), Functor0: () => functorTracedT1});
};
const comonadTransTracedT = dictMonoid => {
  const mempty = dictMonoid.mempty;
  return {lower: dictComonad => v => dictComonad.Extend0().Functor0().map(f => f(mempty))(v)};
};
const comonadTracedT = dictComonad => {
  const $0 = dictComonad.Extend0();
  const Functor0 = $0.Functor0();
  const extendTracedT1 = (() => {
    const functorTracedT1 = {map: f => v => Functor0.map(g => t => f(g(t)))(v)};
    return dictSemigroup => ({extend: f => v => $0.extend(w$p => t => f(Functor0.map(h => t$p => h(dictSemigroup.append(t)(t$p)))(w$p)))(v), Functor0: () => functorTracedT1});
  })();
  return dictMonoid => {
    const mempty = dictMonoid.mempty;
    const extendTracedT2 = extendTracedT1(dictMonoid.Semigroup0());
    return {extract: v => dictComonad.extract(v)(mempty), Extend0: () => extendTracedT2};
  };
};
export {TracedT, comonadTracedT, comonadTransTracedT, extendTracedT, functorTracedT, newtypeTracedT, runTracedT};
