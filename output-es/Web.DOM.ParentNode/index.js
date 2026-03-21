import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNullable from "../Data.Nullable/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import {_firstElementChild, _lastElementChild, _querySelector, childElementCount, children, querySelectorAll} from "./foreign.js";
const QuerySelector = x => x;
const querySelector = qs => {
  const $0 = _querySelector(qs);
  return x => {
    const $1 = $0(x);
    return () => {
      const a$p = $1();
      return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
    };
  };
};
const ordQuerySelector = Data$dOrd.ordString;
const newtypeQuerySelector = {Coercible0: () => {}};
const lastElementChild = x => {
  const $0 = _lastElementChild(x);
  return () => {
    const a$p = $0();
    return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
  };
};
const firstElementChild = x => {
  const $0 = _firstElementChild(x);
  return () => {
    const a$p = $0();
    return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
  };
};
const eqQuerySelector = Data$dEq.eqString;
export {QuerySelector, eqQuerySelector, firstElementChild, lastElementChild, newtypeQuerySelector, ordQuerySelector, querySelector};
export * from "./foreign.js";
