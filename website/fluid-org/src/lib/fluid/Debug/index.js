import {_debugger, _spy, _trace, _traceTime} from "./foreign.js";
const warn = () => ({});
const traceTime = () => $0 => $1 => _traceTime($0, $1);
const trace = () => a => k => _trace(a, k);
const traceM = () => dictMonad => {
  const $0 = dictMonad.Applicative0();
  return s => dictMonad.Bind1().bind($0.pure())(() => _trace(s, v => $0.pure()));
};
const spy = () => tag => a => _spy(tag, a);
const spyWith = () => msg => f => a => a;
const $$debugger = () => f => _debugger(f);
;
export * from "./foreign.js";
