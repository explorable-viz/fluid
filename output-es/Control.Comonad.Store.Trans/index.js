// | This module defines the store comonad transformer, `StoreT`.
import * as Data$dTuple from "../Data.Tuple/index.js";
const StoreT = x => x;
const runStoreT = v => v;
const newtypeStoreT = {Coercible0: () => {}};
const functorStoreT = dictFunctor => ({map: f => v => Data$dTuple.$Tuple(dictFunctor.map(h => x => f(h(x)))(v._1), v._2)});
const extendStoreT = dictExtend => {
  const $0 = dictExtend.Functor0();
  const functorStoreT1 = {map: f => v => Data$dTuple.$Tuple($0.map(h => x => f(h(x)))(v._1), v._2)};
  return {extend: f => v => Data$dTuple.$Tuple(dictExtend.extend(w$p => s$p => f(Data$dTuple.$Tuple(w$p, s$p)))(v._1), v._2), Functor0: () => functorStoreT1};
};
const comonadTransStoreT = {
  lower: dictComonad => v => {
    const $0 = v._2;
    return dictComonad.Extend0().Functor0().map(v1 => v1($0))(v._1);
  }
};
const comonadStoreT = dictComonad => {
  const $0 = dictComonad.Extend0();
  const $1 = $0.Functor0();
  const extendStoreT1 = (() => {
    const functorStoreT1 = {map: f => v => Data$dTuple.$Tuple($1.map(h => x => f(h(x)))(v._1), v._2)};
    return {extend: f => v => Data$dTuple.$Tuple($0.extend(w$p => s$p => f(Data$dTuple.$Tuple(w$p, s$p)))(v._1), v._2), Functor0: () => functorStoreT1};
  })();
  return {extract: v => dictComonad.extract(v._1)(v._2), Extend0: () => extendStoreT1};
};
export {StoreT, comonadStoreT, comonadTransStoreT, extendStoreT, functorStoreT, newtypeStoreT, runStoreT};
