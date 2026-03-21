// | Utilities for working with partial functions.
// | See the README for more documentation.
import * as Partial from "../Partial/index.js";
import {_unsafePartial} from "./foreign.js";
const unsafePartial = _unsafePartial;
const unsafeCrashWith = msg => Partial._crashWith(msg);
export {unsafeCrashWith, unsafePartial};
export * from "./foreign.js";
