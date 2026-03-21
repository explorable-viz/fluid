// | This module defines the `Store` comonad.
import * as Data$dTuple from "../Data.Tuple/index.js";
const store = f => x => Data$dTuple.$Tuple(f, x);
const runStore = v => Data$dTuple.$Tuple(v._1, v._2);
export {runStore, store};
