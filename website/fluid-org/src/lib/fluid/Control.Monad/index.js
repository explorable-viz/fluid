import * as $runtime from "../runtime.js";
import * as Control$dApplicative from "../Control.Applicative/index.js";
import * as Control$dBind from "../Control.Bind/index.js";
const whenM = dictMonad => {
  const $0 = dictMonad.Applicative0();
  return mb => m => dictMonad.Bind1().bind(mb)(b => {
    if (b) { return m; }
    return $0.pure();
  });
};
const unlessM = dictMonad => {
  const $0 = dictMonad.Applicative0();
  return mb => m => dictMonad.Bind1().bind(mb)(b => {
    if (!b) { return m; }
    if (b) { return $0.pure(); }
    $runtime.fail();
  });
};
const monadProxy = {Applicative0: () => Control$dApplicative.applicativeProxy, Bind1: () => Control$dBind.bindProxy};
const monadFn = {Applicative0: () => Control$dApplicative.applicativeFn, Bind1: () => Control$dBind.bindFn};
const monadArray = {Applicative0: () => Control$dApplicative.applicativeArray, Bind1: () => Control$dBind.bindArray};
const liftM1 = dictMonad => f => a => dictMonad.Bind1().bind(a)(a$p => dictMonad.Applicative0().pure(f(a$p)));
const ap = dictMonad => {
  const $0 = dictMonad.Bind1();
  return f => a => $0.bind(f)(f$p => $0.bind(a)(a$p => dictMonad.Applicative0().pure(f$p(a$p))));
};
export {  monadArray, monadFn,   };
