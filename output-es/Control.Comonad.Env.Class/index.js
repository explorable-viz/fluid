// | This module defines the `ComonadEnv` type class and its instances.
import * as Control$dComonad$dEnv$dTrans from "../Control.Comonad.Env.Trans/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const local = dict => dict.local;
const comonadAskTuple = {ask: Data$dTuple.fst, Comonad0: () => Data$dTuple.comonadTuple};
const comonadEnvTuple = {local: f => v => Data$dTuple.$Tuple(f(v._1), v._2), ComonadAsk0: () => comonadAskTuple};
const comonadAskEnvT = dictComonad => {
  const comonadEnvT = Control$dComonad$dEnv$dTrans.comonadEnvT(dictComonad);
  return {ask: v => v._1, Comonad0: () => comonadEnvT};
};
const comonadEnvEnvT = dictComonad => {
  const comonadEnvT = Control$dComonad$dEnv$dTrans.comonadEnvT(dictComonad);
  return {local: f => v => Data$dTuple.$Tuple(f(v._1), v._2), ComonadAsk0: () => ({ask: v => v._1, Comonad0: () => comonadEnvT})};
};
const ask = dict => dict.ask;
const asks = dictComonadAsk => f => x => f(dictComonadAsk.ask(x));
export {ask, asks, comonadAskEnvT, comonadAskTuple, comonadEnvEnvT, comonadEnvTuple, local};
