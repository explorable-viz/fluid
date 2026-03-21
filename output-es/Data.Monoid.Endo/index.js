const Endo = x => x;
const showEndo = dictShow => ({show: v => "(Endo " + dictShow.show(v) + ")"});
const semigroupEndo = dictSemigroupoid => ({append: v => v1 => dictSemigroupoid.compose(v)(v1)});
const ordEndo = dictOrd => dictOrd;
const monoidEndo = dictCategory => {
  const $0 = dictCategory.Semigroupoid0();
  const semigroupEndo1 = {append: v => v1 => $0.compose(v)(v1)};
  return {mempty: dictCategory.identity, Semigroup0: () => semigroupEndo1};
};
const eqEndo = dictEq => dictEq;
const boundedEndo = dictBounded => dictBounded;
export {Endo, boundedEndo, eqEndo, monoidEndo, ordEndo, semigroupEndo, showEndo};
