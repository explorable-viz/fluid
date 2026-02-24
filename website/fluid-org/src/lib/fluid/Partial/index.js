// | Some partial helper functions. See the README for more documentation.
import {_crashWith} from "./foreign.js";
const crashWith = () => _crashWith;
const crash = () => _crashWith("Partial.crash: partial function");
;
export * from "./foreign.js";
