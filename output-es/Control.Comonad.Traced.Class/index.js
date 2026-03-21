// | This module defines the `ComonadTraced` type class and its instances.
import * as Control$dComonad$dTraced$dTrans from "../Control.Comonad.Traced.Trans/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const track = dict => dict.track;
const tracks = dictComonadTraced => f => w => dictComonadTraced.track(f(dictComonadTraced.Comonad0().extract(w)))(w);
const listens = dictFunctor => f => v => dictFunctor.map(g => t => Data$dTuple.$Tuple(g(t), f(t)))(v);
const listen = dictFunctor => v => dictFunctor.map(f => t => Data$dTuple.$Tuple(f(t), t))(v);
const comonadTracedTracedT = dictComonad => {
  const comonadTracedT = Control$dComonad$dTraced$dTrans.comonadTracedT(dictComonad);
  return dictMonoid => {
    const comonadTracedT1 = comonadTracedT(dictMonoid);
    return {track: t => v => dictComonad.extract(v)(t), Comonad0: () => comonadTracedT1};
  };
};
const censor = dictFunctor => f => v => dictFunctor.map(v1 => x => v1(f(x)))(v);
export {censor, comonadTracedTracedT, listen, listens, track, tracks};
