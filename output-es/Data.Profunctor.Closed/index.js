import * as Control$dSemigroupoid from "../Control.Semigroupoid/index.js";
import * as Data$dProfunctor from "../Data.Profunctor/index.js";
const closedFunction = /* #__PURE__ */ (() => ({closed: Control$dSemigroupoid.semigroupoidFn.compose, Profunctor0: () => Data$dProfunctor.profunctorFn}))();
const closed = dict => dict.closed;
export {closed, closedFunction};
