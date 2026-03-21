// | Helper functions for working with mutable objects using the `ST` effect.
// |
// | This module can be used when performance is important and mutation is a
// | local effect.
import * as Data$dMaybe from "../Data.Maybe/index.js";
import {delete as $$delete, new as $$new, peekImpl, poke} from "./foreign.js";
const peek = /* #__PURE__ */ peekImpl(Data$dMaybe.Just)(Data$dMaybe.Nothing);
export {peek};
export * from "./foreign.js";
