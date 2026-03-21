// | This module defines a data type and various functions for interacting
// | with the `Storage` interface of the Web Storage API.
// | For example:
// |
// | ```purescript
// | import Prelude
// | import Effect (Effect)
// | import Effect.Console (log, logShow)
// | import Web.HTML (window)
// | import Web.HTML.Window (localStorage)
// | import Web.Storage.Storage (clear, getItem, removeItem, setItem)
// |
// | main :: Effect Unit
// | main = do
// |   w <- window
// |   s <- localStorage w
// |   setItem "this-is-my-key" "Here is my value." s
// |   v <- getItem "this-is-my-key" s
// |   logShow v
// |
// |   removeItem "this-is-my-key" s
// |   v' <- getItem "this-is-my-key" s
// |   log "It is gone!"
// |   logShow v'
// |
// |   clear s
// | ```
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNullable from "../Data.Nullable/index.js";
import {_getItem, _key, clear, length, removeItem, setItem} from "./foreign.js";
const key = i => {
  const $0 = _key(i);
  return x => {
    const $1 = $0(x);
    return () => {
      const a$p = $1();
      return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
    };
  };
};
const getItem = s => {
  const $0 = _getItem(s);
  return x => {
    const $1 = $0(x);
    return () => {
      const a$p = $1();
      return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
    };
  };
};
export {getItem, key};
export * from "./foreign.js";
