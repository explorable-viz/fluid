import * as Unsafe$dCoerce from "../Unsafe.Coerce/index.js";
const identity = x => x;
const profunctorFn = {dimap: a2b => c2d => b2c => x => c2d(b2c(a2b(x)))};
const dimap = dict => dict.dimap;
const lcmap = dictProfunctor => a2b => dictProfunctor.dimap(a2b)(identity);
const rmap = dictProfunctor => b2c => dictProfunctor.dimap(identity)(b2c);
const unwrapIso = dictProfunctor => () => dictProfunctor.dimap(Unsafe$dCoerce.unsafeCoerce)(Unsafe$dCoerce.unsafeCoerce);
const wrapIso = dictProfunctor => () => v => dictProfunctor.dimap(Unsafe$dCoerce.unsafeCoerce)(Unsafe$dCoerce.unsafeCoerce);
const arr = dictCategory => {
  const identity1 = dictCategory.identity;
  return dictProfunctor => f => dictProfunctor.dimap(identity)(f)(identity1);
};
export {  identity,  profunctorFn,   };
