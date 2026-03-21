// | This module defines the `Cont`inuation monad.
const withCont = f => v => k => v(f(x => k(x)));
const runCont = cc => k => cc(x => k(x));
const mapCont = f => v => k => f(v(k));
const cont = f => c => f(x => c(x));
export {cont, mapCont, runCont, withCont};
