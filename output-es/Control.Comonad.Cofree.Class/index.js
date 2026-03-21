import * as Control$dComonad$dCofree from "../Control.Comonad.Cofree/index.js";
import * as Control$dComonad$dEnv$dTrans from "../Control.Comonad.Env.Trans/index.js";
import * as Control$dComonad$dStore$dTrans from "../Control.Comonad.Store.Trans/index.js";
import * as Control$dComonad$dTraced$dTrans from "../Control.Comonad.Traced.Trans/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const unwrapCofree = dict => dict.unwrapCofree;
const comonadCofreeTracedT = dictFunctor => dictComonadCofree => {
  const Functor0 = dictComonadCofree.Functor0();
  const comonadTracedT = Control$dComonad$dTraced$dTrans.comonadTracedT(dictComonadCofree.Comonad1());
  return dictMonoid => {
    const comonadTracedT1 = comonadTracedT(dictMonoid);
    return {unwrapCofree: v => Functor0.map(Control$dComonad$dTraced$dTrans.TracedT)(dictComonadCofree.unwrapCofree(v)), Functor0: () => Functor0, Comonad1: () => comonadTracedT1};
  };
};
const comonadCofreeStoreT = dictFunctor => dictComonadCofree => {
  const Functor0 = dictComonadCofree.Functor0();
  const comonadStoreT = Control$dComonad$dStore$dTrans.comonadStoreT(dictComonadCofree.Comonad1());
  return {
    unwrapCofree: v => {
      const $0 = v._2;
      return Functor0.map(x => Data$dTuple.$Tuple(x, $0))(dictComonadCofree.unwrapCofree(v._1));
    },
    Functor0: () => Functor0,
    Comonad1: () => comonadStoreT
  };
};
const comonadCofreeEnvT = dictFunctor => dictComonadCofree => {
  const Functor0 = dictComonadCofree.Functor0();
  const comonadEnvT = Control$dComonad$dEnv$dTrans.comonadEnvT(dictComonadCofree.Comonad1());
  return {
    unwrapCofree: v => {
      const $0 = v._1;
      return Functor0.map(x => Data$dTuple.$Tuple($0, x))(dictComonadCofree.unwrapCofree(v._2));
    },
    Functor0: () => Functor0,
    Comonad1: () => comonadEnvT
  };
};
const comonadCofreeCofree = dictFunctor => {
  const extendCofree1 = Control$dComonad$dCofree.extendCofree(dictFunctor);
  return {unwrapCofree: Control$dComonad$dCofree.tail, Functor0: () => dictFunctor, Comonad1: () => ({extract: Control$dComonad$dCofree.head, Extend0: () => extendCofree1})};
};
export {comonadCofreeCofree, comonadCofreeEnvT, comonadCofreeStoreT, comonadCofreeTracedT, unwrapCofree};
