import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNullable from "../Data.Nullable/index.js";
import {_nextElementSibling, _previousElementSibling} from "./foreign.js";
const previousElementSibling = x => {
  const $0 = _previousElementSibling(x);
  return () => {
    const a$p = $0();
    return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
  };
};
const nextElementSibling = x => {
  const $0 = _nextElementSibling(x);
  return () => {
    const a$p = $0();
    return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
  };
};
export {nextElementSibling, previousElementSibling};
export * from "./foreign.js";
