import * as $runtime from "../runtime.js";
const lazyUnit = {defer: v => {}};
const lazyFn = {defer: f => x => f()(x)};
const defer = dict => dict.defer;
const fix = dictLazy => f => {
  const go$lazy = $runtime.binding(() => dictLazy.defer(v => f(go$lazy())));
  const go = go$lazy();
  return go;
};
export {defer, fix, lazyFn, lazyUnit};
