import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Record$dUnsafe from "../Record.Unsafe/index.js";
import * as Type$dProxy from "../Type.Proxy/index.js";
import {bottomChar, bottomInt, bottomNumber, topChar, topInt, topNumber} from "./foreign.js";
const topRecord = dict => dict.topRecord;
const top = dict => dict.top;
const boundedUnit = {top: undefined, bottom: undefined, Ord0: () => Data$dOrd.ordUnit};
const boundedRecordNil = {topRecord: v => v1 => ({}), bottomRecord: v => v1 => ({}), OrdRecord0: () => Data$dOrd.ordRecordNil};
const boundedProxy = {bottom: Type$dProxy.Proxy, top: Type$dProxy.Proxy, Ord0: () => Data$dOrd.ordProxy};
const boundedOrdering = {top: Data$dOrdering.GT, bottom: Data$dOrdering.LT, Ord0: () => Data$dOrd.ordOrdering};
const boundedNumber = {top: Infinity, bottom: -Infinity, Ord0: () => Data$dOrd.ordNumber};
const boundedInt = {top: 2147483647, bottom: -2147483648, Ord0: () => Data$dOrd.ordInt};
const boundedChar = {top: "￿", bottom: "\u0000", Ord0: () => Data$dOrd.ordChar};
const boundedBoolean = {top: true, bottom: false, Ord0: () => Data$dOrd.ordBoolean};
const bottomRecord = dict => dict.bottomRecord;
const boundedRecord = () => dictBoundedRecord => {
  const $0 = dictBoundedRecord.OrdRecord0();
  const eqRec1 = {eq: $0.EqRecord0().eqRecord(Type$dProxy.Proxy)};
  const ordRecord1 = {compare: $0.compareRecord(Type$dProxy.Proxy), Eq0: () => eqRec1};
  return {
    top: dictBoundedRecord.topRecord(Type$dProxy.Proxy)(Type$dProxy.Proxy),
    bottom: dictBoundedRecord.bottomRecord(Type$dProxy.Proxy)(Type$dProxy.Proxy),
    Ord0: () => ordRecord1
  };
};
const bottom = dict => dict.bottom;
const boundedRecordCons = dictIsSymbol => dictBounded => {
  const top1 = dictBounded.top;
  const bottom1 = dictBounded.bottom;
  const Ord0 = dictBounded.Ord0();
  return () => () => dictBoundedRecord => {
    const $0 = dictBoundedRecord.OrdRecord0();
    const $1 = $0.EqRecord0();
    const $2 = Ord0.Eq0();
    const ordRecordCons = (() => {
      const eqRowCons2 = {
        eqRecord: v => ra => rb => {
          const $$get = Record$dUnsafe.unsafeGet(dictIsSymbol.reflectSymbol(Type$dProxy.Proxy));
          return $2.eq($$get(ra))($$get(rb)) && $1.eqRecord(Type$dProxy.Proxy)(ra)(rb);
        }
      };
      return {
        compareRecord: v => ra => rb => {
          const key = dictIsSymbol.reflectSymbol(Type$dProxy.Proxy);
          const left = Ord0.compare(Record$dUnsafe.unsafeGet(key)(ra))(Record$dUnsafe.unsafeGet(key)(rb));
          if (left === "LT" || left === "GT" || left !== "EQ") { return left; }
          return $0.compareRecord(Type$dProxy.Proxy)(ra)(rb);
        },
        EqRecord0: () => eqRowCons2
      };
    })();
    return {
      topRecord: v => rowProxy => Record$dUnsafe.unsafeSet(dictIsSymbol.reflectSymbol(Type$dProxy.Proxy))(top1)(dictBoundedRecord.topRecord(Type$dProxy.Proxy)(rowProxy)),
      bottomRecord: v => rowProxy => Record$dUnsafe.unsafeSet(dictIsSymbol.reflectSymbol(Type$dProxy.Proxy))(bottom1)(dictBoundedRecord.bottomRecord(Type$dProxy.Proxy)(rowProxy)),
      OrdRecord0: () => ordRecordCons
    };
  };
};
export {
  
  
  boundedBoolean,
  boundedChar,
  
  
  boundedOrdering,
  
  
  
  
  boundedUnit,
  
  
};
export * from "./foreign.js";
