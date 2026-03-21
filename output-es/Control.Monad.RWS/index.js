// | This module defines the `RWS` monad.
import * as Control$dMonad$dRWS$dTrans from "../Control.Monad.RWS.Trans/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const withRWS = Control$dMonad$dRWS$dTrans.withRWST;
const rws = f => r => s => f(r)(s);
const runRWS = m => r => s => m(r)(s);
const mapRWS = f => v => r => s => f(v(r)(s));
const execRWS = m => r => s => {
  const $0 = m(r)(s);
  return Data$dTuple.$Tuple($0._1, $0._3);
};
const evalRWS = m => r => s => {
  const $0 = m(r)(s);
  return Data$dTuple.$Tuple($0._2, $0._3);
};
export {evalRWS, execRWS, mapRWS, runRWS, rws, withRWS};
