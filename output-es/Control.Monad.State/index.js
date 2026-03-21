// | This module defines the `State` monad.
import * as Control$dMonad$dState$dTrans from "../Control.Monad.State.Trans/index.js";
const withState = Control$dMonad$dState$dTrans.withStateT;
const runState = v => x => v(x);
const mapState = f => v => x => f(v(x));
const execState = v => s => v(s)._2;
const evalState = v => s => v(s)._1;
export {evalState, execState, mapState, runState, withState};
