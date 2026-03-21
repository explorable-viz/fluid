import * as Control$dBind from "../Control.Bind/index.js";
import * as Control$dMonad$dExcept$dTrans from "../Control.Monad.Except.Trans/index.js";
import * as Control$dMonad$dFree from "../Control.Monad.Free/index.js";
import * as Control$dMonad$dMaybe$dTrans from "../Control.Monad.Maybe.Trans/index.js";
import * as Control$dMonad$dReader$dTrans from "../Control.Monad.Reader.Trans/index.js";
import * as Control$dMonad$dState$dTrans from "../Control.Monad.State.Trans/index.js";
import * as Control$dMonad$dWriter$dTrans from "../Control.Monad.Writer.Trans/index.js";
import * as Data$dCatList from "../Data.CatList/index.js";
import * as Data$dCatQueue from "../Data.CatQueue/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
const wrapFree = dict => dict.wrapFree;
const monadFreeWriterT = dictFunctor => dictMonadFree => {
  const Monad0 = dictMonadFree.Monad0();
  return dictMonoid => {
    const monadWriterT = Control$dMonad$dWriter$dTrans.monadWriterT(dictMonoid)(Monad0);
    return {wrapFree: f => dictMonadFree.wrapFree(dictFunctor.map(Control$dMonad$dWriter$dTrans.runWriterT)(f)), Monad0: () => monadWriterT};
  };
};
const monadFreeStateT = dictFunctor => dictMonadFree => {
  const $0 = dictMonadFree.Monad0();
  const monadStateT = {Applicative0: () => Control$dMonad$dState$dTrans.applicativeStateT($0), Bind1: () => Control$dMonad$dState$dTrans.bindStateT($0)};
  return {wrapFree: f => s => dictMonadFree.wrapFree(dictFunctor.map(st => st(s))(f)), Monad0: () => monadStateT};
};
const monadFreeReaderT = dictFunctor => dictMonadFree => {
  const monadReaderT = Control$dMonad$dReader$dTrans.monadReaderT(dictMonadFree.Monad0());
  return {wrapFree: f => r => dictMonadFree.wrapFree(dictFunctor.map(rt => rt(r))(f)), Monad0: () => monadReaderT};
};
const monadFreeMaybeT = dictFunctor => dictMonadFree => {
  const $0 = dictMonadFree.Monad0();
  const monadMaybeT = {Applicative0: () => Control$dMonad$dMaybe$dTrans.applicativeMaybeT($0), Bind1: () => Control$dMonad$dMaybe$dTrans.bindMaybeT($0)};
  return {wrapFree: f => dictMonadFree.wrapFree(dictFunctor.map(Control$dMonad$dMaybe$dTrans.runMaybeT)(f)), Monad0: () => monadMaybeT};
};
const monadFreeFree = {
  wrapFree: x => Control$dMonad$dFree.$Free(
    Control$dMonad$dFree.$FreeView("Bind", x, x$1 => Control$dMonad$dFree.$Free(Control$dMonad$dFree.$FreeView("Return", x$1), Data$dCatList.CatNil)),
    Data$dCatList.$CatList("CatCons", Control$dBind.identity, Data$dCatQueue.$CatQueue(Data$dList$dTypes.Nil, Data$dList$dTypes.Nil))
  ),
  Monad0: () => Control$dMonad$dFree.freeMonad
};
const monadFreeExceptT = dictFunctor => dictMonadFree => {
  const $0 = dictMonadFree.Monad0();
  const monadExceptT = {Applicative0: () => Control$dMonad$dExcept$dTrans.applicativeExceptT($0), Bind1: () => Control$dMonad$dExcept$dTrans.bindExceptT($0)};
  return {wrapFree: f => dictMonadFree.wrapFree(dictFunctor.map(Control$dMonad$dExcept$dTrans.runExceptT)(f)), Monad0: () => monadExceptT};
};
export {monadFreeExceptT, monadFreeFree, monadFreeMaybeT, monadFreeReaderT, monadFreeStateT, monadFreeWriterT, wrapFree};
