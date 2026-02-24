// | The functions in this module are highly unsafe as they treat records like
// | stringly-keyed maps and can coerce the row of labels that a record has.
// |
// | These function are intended for situations where there is some other way of
// | proving things about the structure of the record - for example, when using
// | `RowToList`. **They should never be used for general record manipulation.**
import {unsafeDelete, unsafeGet, unsafeHas, unsafeSet} from "./foreign.js";
export {};
export * from "./foreign.js";
