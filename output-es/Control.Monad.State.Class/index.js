// | This module defines the `MonadState` type class and its instances.
import * as Data$dTuple from "../Data.Tuple/index.js";
const state = dict => dict.state;
const put = dictMonadState => s => dictMonadState.state(v => Data$dTuple.$Tuple(undefined, s));
const modify_ = dictMonadState => f => dictMonadState.state(s => Data$dTuple.$Tuple(undefined, f(s)));
const modify = dictMonadState => f => dictMonadState.state(s => {
  const s$p = f(s);
  return Data$dTuple.$Tuple(s$p, s$p);
});
const gets = dictMonadState => f => dictMonadState.state(s => Data$dTuple.$Tuple(f(s), s));
const $$get = dictMonadState => dictMonadState.state(s => Data$dTuple.$Tuple(s, s));
export {$$get as get, gets, modify, modify_, put, state};
