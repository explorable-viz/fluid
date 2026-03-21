// | This module defines the `Env` comonad.
import * as Control$dComonad$dEnv$dTrans from "../Control.Comonad.Env.Trans/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const withEnv = Control$dComonad$dEnv$dTrans.withEnvT;
const runEnv = v => Data$dTuple.$Tuple(v._1, v._2);
const mapEnv = f => v => Data$dTuple.$Tuple(v._1, f(v._2));
const env = e => a => Data$dTuple.$Tuple(e, a);
export {env, mapEnv, runEnv, withEnv};
