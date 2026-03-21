const Join = x => x;
const showJoin = dictShow => ({show: v => "(Join " + dictShow.show(v) + ")"});
const ordJoin = dictOrd => dictOrd;
const newtypeJoin = {Coercible0: () => {}};
const eqJoin = dictEq => dictEq;
const bifunctorJoin = dictBifunctor => ({map: f => v => dictBifunctor.bimap(f)(f)(v)});
const biapplyJoin = dictBiapply => {
  const $0 = dictBiapply.Bifunctor0();
  const bifunctorJoin1 = {map: f => v => $0.bimap(f)(f)(v)};
  return {apply: v => v1 => dictBiapply.biapply(v)(v1), Functor0: () => bifunctorJoin1};
};
const biapplicativeJoin = dictBiapplicative => {
  const $0 = dictBiapplicative.Biapply0();
  const $1 = $0.Bifunctor0();
  const biapplyJoin1 = (() => {
    const bifunctorJoin1 = {map: f => v => $1.bimap(f)(f)(v)};
    return {apply: v => v1 => $0.biapply(v)(v1), Functor0: () => bifunctorJoin1};
  })();
  return {pure: a => dictBiapplicative.bipure(a)(a), Apply0: () => biapplyJoin1};
};
export {Join, biapplicativeJoin, biapplyJoin, bifunctorJoin, eqJoin, newtypeJoin, ordJoin, showJoin};
