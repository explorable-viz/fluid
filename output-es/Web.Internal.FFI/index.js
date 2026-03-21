import * as Data$dMaybe from "../Data.Maybe/index.js";
import {_unsafeReadProtoTagged} from "./foreign.js";
const unsafeReadProtoTagged = name => value => _unsafeReadProtoTagged(Data$dMaybe.Nothing, Data$dMaybe.Just, name, value);
export {unsafeReadProtoTagged};
export * from "./foreign.js";
