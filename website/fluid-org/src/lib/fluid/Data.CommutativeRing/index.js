import * as Data$dRing from "../Data.Ring/index.js";
import * as Type$dProxy from "../Type.Proxy/index.js";
const commutativeRingUnit = {Ring0: () => Data$dRing.ringUnit};
const commutativeRingRecordNil = {RingRecord0: () => Data$dRing.ringRecordNil};
const commutativeRingRecordCons = dictIsSymbol => () => dictCommutativeRingRecord => {
  const ringRecordCons1 = Data$dRing.ringRecordCons(dictIsSymbol)()(dictCommutativeRingRecord.RingRecord0());
  return dictCommutativeRing => {
    const ringRecordCons2 = ringRecordCons1(dictCommutativeRing.Ring0());
    return {RingRecord0: () => ringRecordCons2};
  };
};
const commutativeRingRecord = () => dictCommutativeRingRecord => {
  const $0 = dictCommutativeRingRecord.RingRecord0();
  const $1 = $0.SemiringRecord0();
  const ringRecord1 = (() => {
    const semiringRecord1 = {
      add: $1.addRecord(Type$dProxy.Proxy),
      mul: $1.mulRecord(Type$dProxy.Proxy),
      one: $1.oneRecord(Type$dProxy.Proxy)(Type$dProxy.Proxy),
      zero: $1.zeroRecord(Type$dProxy.Proxy)(Type$dProxy.Proxy)
    };
    return {sub: $0.subRecord(Type$dProxy.Proxy), Semiring0: () => semiringRecord1};
  })();
  return {Ring0: () => ringRecord1};
};
const commutativeRingProxy = {Ring0: () => Data$dRing.ringProxy};
const commutativeRingNumber = {Ring0: () => Data$dRing.ringNumber};
const commutativeRingInt = {Ring0: () => Data$dRing.ringInt};
const commutativeRingFn = dictCommutativeRing => {
  const $0 = dictCommutativeRing.Ring0();
  const $1 = $0.Semiring0();
  const ringFn = (() => {
    const zero1 = $1.zero;
    const one1 = $1.one;
    const semiringFn = {add: f => g => x => $1.add(f(x))(g(x)), zero: v => zero1, mul: f => g => x => $1.mul(f(x))(g(x)), one: v => one1};
    return {sub: f => g => x => $0.sub(f(x))(g(x)), Semiring0: () => semiringFn};
  })();
  return {Ring0: () => ringFn};
};
export {
  
  commutativeRingInt,
  commutativeRingNumber,
  
  
  
  
  
};
