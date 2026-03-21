import * as Data$dVoid from "../Data.Void/index.js";
import * as Record$dUnsafe from "../Record.Unsafe/index.js";
import * as Type$dProxy from "../Type.Proxy/index.js";
import {concatArray, concatString} from "./foreign.js";
const semigroupVoid = {append: v => Data$dVoid.absurd};
const semigroupUnit = {append: v => v1 => {}};
const semigroupString = {append: concatString};
const semigroupRecordNil = {appendRecord: v => v1 => v2 => ({})};
const semigroupProxy = {append: v => v1 => Type$dProxy.Proxy};
const semigroupArray = {append: concatArray};
const appendRecord = dict => dict.appendRecord;
const semigroupRecord = () => dictSemigroupRecord => ({append: dictSemigroupRecord.appendRecord(Type$dProxy.Proxy)});
const append = dict => dict.append;
const semigroupFn = dictSemigroup => ({append: f => g => x => dictSemigroup.append(f(x))(g(x))});
const semigroupRecordCons = dictIsSymbol => () => dictSemigroupRecord => dictSemigroup => (
  {
    appendRecord: v => ra => rb => {
      const key = dictIsSymbol.reflectSymbol(Type$dProxy.Proxy);
      const $$get = Record$dUnsafe.unsafeGet(key);
      return Record$dUnsafe.unsafeSet(key)(dictSemigroup.append($$get(ra))($$get(rb)))(dictSemigroupRecord.appendRecord(Type$dProxy.Proxy)(ra)(rb));
    }
  }
);
export {  semigroupArray,     semigroupRecordNil, semigroupString, semigroupUnit, };
export * from "./foreign.js";
