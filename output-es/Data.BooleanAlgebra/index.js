import * as Data$dHeytingAlgebra from "../Data.HeytingAlgebra/index.js";
import * as Type$dProxy from "../Type.Proxy/index.js";
const booleanAlgebraUnit = {HeytingAlgebra0: () => Data$dHeytingAlgebra.heytingAlgebraUnit};
const booleanAlgebraRecordNil = {HeytingAlgebraRecord0: () => Data$dHeytingAlgebra.heytingAlgebraRecordNil};
const booleanAlgebraRecordCons = dictIsSymbol => () => dictBooleanAlgebraRecord => {
  const heytingAlgebraRecordCons1 = Data$dHeytingAlgebra.heytingAlgebraRecordCons(dictIsSymbol)()(dictBooleanAlgebraRecord.HeytingAlgebraRecord0());
  return dictBooleanAlgebra => {
    const heytingAlgebraRecordCons2 = heytingAlgebraRecordCons1(dictBooleanAlgebra.HeytingAlgebra0());
    return {HeytingAlgebraRecord0: () => heytingAlgebraRecordCons2};
  };
};
const booleanAlgebraRecord = () => dictBooleanAlgebraRecord => {
  const $0 = dictBooleanAlgebraRecord.HeytingAlgebraRecord0();
  const heytingAlgebraRecord1 = {
    ff: $0.ffRecord(Type$dProxy.Proxy)(Type$dProxy.Proxy),
    tt: $0.ttRecord(Type$dProxy.Proxy)(Type$dProxy.Proxy),
    conj: $0.conjRecord(Type$dProxy.Proxy),
    disj: $0.disjRecord(Type$dProxy.Proxy),
    implies: $0.impliesRecord(Type$dProxy.Proxy),
    not: $0.notRecord(Type$dProxy.Proxy)
  };
  return {HeytingAlgebra0: () => heytingAlgebraRecord1};
};
const booleanAlgebraProxy = {HeytingAlgebra0: () => Data$dHeytingAlgebra.heytingAlgebraProxy};
const booleanAlgebraFn = dictBooleanAlgebra => {
  const $0 = dictBooleanAlgebra.HeytingAlgebra0();
  const ff1 = $0.ff;
  const heytingAlgebraFunction = (() => {
    const tt1 = $0.tt;
    return {
      ff: v => ff1,
      tt: v => tt1,
      implies: f => g => a => $0.implies(f(a))(g(a)),
      conj: f => g => a => $0.conj(f(a))(g(a)),
      disj: f => g => a => $0.disj(f(a))(g(a)),
      not: f => a => $0.not(f(a))
    };
  })();
  return {HeytingAlgebra0: () => heytingAlgebraFunction};
};
const booleanAlgebraBoolean = {HeytingAlgebra0: () => Data$dHeytingAlgebra.heytingAlgebraBoolean};
export {booleanAlgebraBoolean, booleanAlgebraFn, booleanAlgebraProxy, booleanAlgebraRecord, booleanAlgebraRecordCons, booleanAlgebraRecordNil, booleanAlgebraUnit};
