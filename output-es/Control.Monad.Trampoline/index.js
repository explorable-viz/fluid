// | A _trampoline_ monad, which can be used at the bottom of
// | a monad transformer stack to avoid stack overflows in large
// | monadic computations.
import * as Control$dMonad$dFree from "../Control.Monad.Free/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
const runTrampoline = /* #__PURE__ */ Control$dMonad$dFree.runFree(Data$dFunctor.functorFn)(v => v());
const done = /* #__PURE__ */ (() => Control$dMonad$dFree.freeApplicative.pure)();
const delay = Control$dMonad$dFree.liftF;
export {delay, done, runTrampoline};
