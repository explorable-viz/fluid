import * as Record$dUnsafe from "../Record.Unsafe/index.js";
import * as Type$dProxy from "../Type.Proxy/index.js";
import {intAdd, intMul, numAdd, numMul} from "./foreign.js";
const zeroRecord = dict => dict.zeroRecord;
const zero = dict => dict.zero;
const semiringUnit = {add: v => v1 => {}, zero: undefined, mul: v => v1 => {}, one: undefined};
const semiringRecordNil = {addRecord: v => v1 => v2 => ({}), mulRecord: v => v1 => v2 => ({}), oneRecord: v => v1 => ({}), zeroRecord: v => v1 => ({})};
const semiringProxy = {add: v => v1 => Type$dProxy.Proxy, mul: v => v1 => Type$dProxy.Proxy, one: Type$dProxy.Proxy, zero: Type$dProxy.Proxy};
const semiringNumber = {add: numAdd, zero: 0.0, mul: numMul, one: 1.0};
const semiringInt = {add: intAdd, zero: 0, mul: intMul, one: 1};
const oneRecord = dict => dict.oneRecord;
const one = dict => dict.one;
const mulRecord = dict => dict.mulRecord;
const mul = dict => dict.mul;
const addRecord = dict => dict.addRecord;
const semiringRecord = () => dictSemiringRecord => (
  {
    add: dictSemiringRecord.addRecord(Type$dProxy.Proxy),
    mul: dictSemiringRecord.mulRecord(Type$dProxy.Proxy),
    one: dictSemiringRecord.oneRecord(Type$dProxy.Proxy)(Type$dProxy.Proxy),
    zero: dictSemiringRecord.zeroRecord(Type$dProxy.Proxy)(Type$dProxy.Proxy)
  }
);
const add = dict => dict.add;
const semiringFn = dictSemiring => {
  const zero1 = dictSemiring.zero;
  const one1 = dictSemiring.one;
  return {add: f => g => x => dictSemiring.add(f(x))(g(x)), zero: v => zero1, mul: f => g => x => dictSemiring.mul(f(x))(g(x)), one: v => one1};
};
const semiringRecordCons = dictIsSymbol => () => dictSemiringRecord => dictSemiring => {
  const one1 = dictSemiring.one;
  const zero1 = dictSemiring.zero;
  return {
    addRecord: v => ra => rb => {
      const key = dictIsSymbol.reflectSymbol(Type$dProxy.Proxy);
      const $$get = Record$dUnsafe.unsafeGet(key);
      return Record$dUnsafe.unsafeSet(key)(dictSemiring.add($$get(ra))($$get(rb)))(dictSemiringRecord.addRecord(Type$dProxy.Proxy)(ra)(rb));
    },
    mulRecord: v => ra => rb => {
      const key = dictIsSymbol.reflectSymbol(Type$dProxy.Proxy);
      const $$get = Record$dUnsafe.unsafeGet(key);
      return Record$dUnsafe.unsafeSet(key)(dictSemiring.mul($$get(ra))($$get(rb)))(dictSemiringRecord.mulRecord(Type$dProxy.Proxy)(ra)(rb));
    },
    oneRecord: v => v1 => Record$dUnsafe.unsafeSet(dictIsSymbol.reflectSymbol(Type$dProxy.Proxy))(one1)(dictSemiringRecord.oneRecord(Type$dProxy.Proxy)(Type$dProxy.Proxy)),
    zeroRecord: v => v1 => Record$dUnsafe.unsafeSet(dictIsSymbol.reflectSymbol(Type$dProxy.Proxy))(zero1)(dictSemiringRecord.zeroRecord(Type$dProxy.Proxy)(Type$dProxy.Proxy))
  };
};
export {
  
  
  
  
  
  
  
  semiringInt,
  semiringNumber,
  semiringProxy,
  
  semiringRecordCons,
  semiringRecordNil,
  semiringUnit,
  
  
};
export * from "./foreign.js";
