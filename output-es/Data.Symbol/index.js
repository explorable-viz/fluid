import * as Type$dProxy from "../Type.Proxy/index.js";
import {unsafeCoerce} from "./foreign.js";
const reifySymbol = s => f => unsafeCoerce(dictIsSymbol => f(dictIsSymbol))({reflectSymbol: v => s})(Type$dProxy.Proxy);
const reflectSymbol = dict => dict.reflectSymbol;
export {reflectSymbol, reifySymbol};
export * from "./foreign.js";
