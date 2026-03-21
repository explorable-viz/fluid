// | This module defines the `MonadReader` type class and its instances.
import * as Control$dMonad from "../Control.Monad/index.js";
const monadAskFun = {ask: x => x, Monad0: () => Control$dMonad.monadFn};
const monadReaderFun = {local: f => g => x => g(f(x)), MonadAsk0: () => monadAskFun};
const local = dict => dict.local;
const ask = dict => dict.ask;
const asks = dictMonadAsk => {
  const ask1 = dictMonadAsk.ask;
  return f => dictMonadAsk.Monad0().Bind1().Apply0().Functor0().map(f)(ask1);
};
export {ask, asks, local, monadAskFun, monadReaderFun};
