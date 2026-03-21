import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNullable from "../Data.Nullable/index.js";
import {forImpl, keyForImpl, showSymbolImpl} from "./foreign.js";
const showSymbol = s => showSymbolImpl(s);
const showJsSymbol = {show: x => showSymbolImpl(x)};
const keyFor = s => () => {
  const a$p = keyForImpl(s);
  return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
};
const $$for = s => () => forImpl(s);
export {$$for as for, keyFor, showJsSymbol, showSymbol};
export * from "./foreign.js";
