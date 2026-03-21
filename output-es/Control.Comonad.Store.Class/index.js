// | This module defines the `ComonadStore` type class and its instances.
import * as Control$dComonad$dEnv$dTrans from "../Control.Comonad.Env.Trans/index.js";
import * as Control$dComonad$dStore$dTrans from "../Control.Comonad.Store.Trans/index.js";
import * as Control$dComonad$dTraced$dTrans from "../Control.Comonad.Traced.Trans/index.js";
import * as Control$dExtend from "../Control.Extend/index.js";
const pos = dict => dict.pos;
const peek = dict => dict.peek;
const peeks = dictComonadStore => f => x => dictComonadStore.peek(f(dictComonadStore.pos(x)))(x);
const seeks = dictComonadStore => {
  const duplicate = dictComonadStore.Comonad0().Extend0().extend(Control$dExtend.identity);
  return f => x => {
    const $0 = duplicate(x);
    return dictComonadStore.peek(f(dictComonadStore.pos($0)))($0);
  };
};
const seek = dictComonadStore => {
  const duplicate = dictComonadStore.Comonad0().Extend0().extend(Control$dExtend.identity);
  return s => {
    const $0 = dictComonadStore.peek(s);
    return x => $0(duplicate(x));
  };
};
const experiment = dictComonadStore => dictFunctor => f => x => dictFunctor.map(a => dictComonadStore.peek(a)(x))(f(dictComonadStore.pos(x)));
const comonadStoreTracedT = dictComonadStore => {
  const Comonad0 = dictComonadStore.Comonad0();
  const comonadTracedT = Control$dComonad$dTraced$dTrans.comonadTracedT(Comonad0);
  return dictMonoid => {
    const mempty = dictMonoid.mempty;
    const lower1 = v => Comonad0.Extend0().Functor0().map(f => f(mempty))(v);
    const comonadTracedT1 = comonadTracedT(dictMonoid);
    return {
      pos: x => dictComonadStore.pos(lower1(x)),
      peek: s => {
        const $0 = dictComonadStore.peek(s);
        return x => $0(lower1(x));
      },
      Comonad0: () => comonadTracedT1
    };
  };
};
const comonadStoreStoreT = dictComonad => {
  const comonadStoreT = Control$dComonad$dStore$dTrans.comonadStoreT(dictComonad);
  return {pos: v => v._2, peek: s => v => dictComonad.extract(v._1)(s), Comonad0: () => comonadStoreT};
};
const comonadStoreEnvT = dictComonadStore => {
  const comonadEnvT = Control$dComonad$dEnv$dTrans.comonadEnvT(dictComonadStore.Comonad0());
  return {
    pos: x => dictComonadStore.pos(x._2),
    peek: s => {
      const $0 = dictComonadStore.peek(s);
      return x => $0(x._2);
    },
    Comonad0: () => comonadEnvT
  };
};
export {comonadStoreEnvT, comonadStoreStoreT, comonadStoreTracedT, experiment, peek, peeks, pos, seek, seeks};
