const Clown = x => x;
const showClown = dictShow => ({show: v => "(Clown " + dictShow.show(v) + ")"});
const profunctorClown = dictContravariant => ({dimap: f => v => v1 => dictContravariant.cmap(f)(v1)});
const ordClown = dictOrd => dictOrd;
const newtypeClown = {Coercible0: () => {}};
const hoistClown = f => v => f(v);
const functorClown = {map: v => v1 => v1};
const eqClown = dictEq => dictEq;
const bifunctorClown = dictFunctor => ({bimap: f => v => v1 => dictFunctor.map(f)(v1)});
const biapplyClown = dictApply => {
  const $0 = dictApply.Functor0();
  const bifunctorClown1 = {bimap: f => v => v1 => $0.map(f)(v1)};
  return {biapply: v => v1 => dictApply.apply(v)(v1), Bifunctor0: () => bifunctorClown1};
};
const biapplicativeClown = dictApplicative => {
  const $0 = dictApplicative.Apply0();
  const $1 = $0.Functor0();
  const biapplyClown1 = (() => {
    const bifunctorClown1 = {bimap: f => v => v1 => $1.map(f)(v1)};
    return {biapply: v => v1 => $0.apply(v)(v1), Bifunctor0: () => bifunctorClown1};
  })();
  return {bipure: a => v => dictApplicative.pure(a), Biapply0: () => biapplyClown1};
};
export {Clown,          };
