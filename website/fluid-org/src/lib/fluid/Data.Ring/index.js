import * as Data$dSemiring from "../Data.Semiring/index.js";
import * as Record$dUnsafe from "../Record.Unsafe/index.js";
import * as Type$dProxy from "../Type.Proxy/index.js";
import {intSub, numSub} from "./foreign.js";
const subRecord = dict => dict.subRecord;
const sub = dict => dict.sub;
const ringUnit = {sub: v => v1 => {}, Semiring0: () => Data$dSemiring.semiringUnit};
const ringRecordNil = {subRecord: v => v1 => v2 => ({}), SemiringRecord0: () => Data$dSemiring.semiringRecordNil};
const ringRecordCons = dictIsSymbol => () => dictRingRecord => {
  const semiringRecordCons1 = Data$dSemiring.semiringRecordCons(dictIsSymbol)()(dictRingRecord.SemiringRecord0());
  return dictRing => {
    const semiringRecordCons2 = semiringRecordCons1(dictRing.Semiring0());
    return {
      subRecord: v => ra => rb => {
        const key = dictIsSymbol.reflectSymbol(Type$dProxy.Proxy);
        const $$get = Record$dUnsafe.unsafeGet(key);
        return Record$dUnsafe.unsafeSet(key)(dictRing.sub($$get(ra))($$get(rb)))(dictRingRecord.subRecord(Type$dProxy.Proxy)(ra)(rb));
      },
      SemiringRecord0: () => semiringRecordCons2
    };
  };
};
const ringRecord = () => dictRingRecord => {
  const $0 = dictRingRecord.SemiringRecord0();
  const semiringRecord1 = {
    add: $0.addRecord(Type$dProxy.Proxy),
    mul: $0.mulRecord(Type$dProxy.Proxy),
    one: $0.oneRecord(Type$dProxy.Proxy)(Type$dProxy.Proxy),
    zero: $0.zeroRecord(Type$dProxy.Proxy)(Type$dProxy.Proxy)
  };
  return {sub: dictRingRecord.subRecord(Type$dProxy.Proxy), Semiring0: () => semiringRecord1};
};
const ringProxy = {sub: v => v1 => Type$dProxy.Proxy, Semiring0: () => Data$dSemiring.semiringProxy};
const ringNumber = {sub: numSub, Semiring0: () => Data$dSemiring.semiringNumber};
const ringInt = {sub: intSub, Semiring0: () => Data$dSemiring.semiringInt};
const ringFn = dictRing => {
  const $0 = dictRing.Semiring0();
  const zero1 = $0.zero;
  const semiringFn = (() => {
    const one1 = $0.one;
    return {add: f => g => x => $0.add(f(x))(g(x)), zero: v => zero1, mul: f => g => x => $0.mul(f(x))(g(x)), one: v => one1};
  })();
  return {sub: f => g => x => dictRing.sub(f(x))(g(x)), Semiring0: () => semiringFn};
};
const negate = dictRing => {
  const zero = dictRing.Semiring0().zero;
  return a => dictRing.sub(zero)(a);
};
export {  ringInt, ringNumber, ringProxy,  ringRecordCons, ringRecordNil, ringUnit,  };
export * from "./foreign.js";
