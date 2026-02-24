import * as Control$dApply from "../Control.Apply/index.js";
import * as Type$dProxy from "../Type.Proxy/index.js";
import {arrayBind} from "./foreign.js";
const identity = x => x;
const discard = dict => dict.discard;
const bindProxy = {bind: v => v1 => Type$dProxy.Proxy, Apply0: () => Control$dApply.applyProxy};
const bindFn = {bind: m => f => x => f(m(x))(x), Apply0: () => Control$dApply.applyFn};
const bindArray = {bind: arrayBind, Apply0: () => Control$dApply.applyArray};
const bind = dict => dict.bind;
const bindFlipped = dictBind => b => a => dictBind.bind(a)(b);
const composeKleisliFlipped = dictBind => f => g => a => dictBind.bind(g(a))(f);
const composeKleisli = dictBind => f => g => a => dictBind.bind(f(a))(g);
const discardProxy = {discard: dictBind => dictBind.bind};
const discardUnit = {discard: dictBind => dictBind.bind};
const ifM = dictBind => cond => t => f => dictBind.bind(cond)(cond$p => {
  if (cond$p) { return t; }
  return f;
});
const join = dictBind => m => dictBind.bind(m)(identity);
export { bindArray,  bindFn, bindProxy,      identity,  };
export * from "./foreign.js";
