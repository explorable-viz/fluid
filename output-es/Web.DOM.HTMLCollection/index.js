import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNullable from "../Data.Nullable/index.js";
import {_item, _namedItem, length, toArray} from "./foreign.js";
const namedItem = id => {
  const $0 = _namedItem(id);
  return x => {
    const $1 = $0(x);
    return () => {
      const a$p = $1();
      return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
    };
  };
};
const item = i => {
  const $0 = _item(i);
  return x => {
    const $1 = $0(x);
    return () => {
      const a$p = $1();
      return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
    };
  };
};
export {item, namedItem};
export * from "./foreign.js";
