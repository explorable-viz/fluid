import * as Record$dUnsafe from "../Record.Unsafe/index.js";
import * as Type$dProxy from "../Type.Proxy/index.js";
import {eqArrayImpl, eqBooleanImpl, eqCharImpl, eqIntImpl, eqNumberImpl, eqStringImpl} from "./foreign.js";
const eqVoid = {eq: v => v1 => true};
const eqUnit = {eq: v => v1 => true};
const eqString = {eq: eqStringImpl};
const eqRowNil = {eqRecord: v => v1 => v2 => true};
const eqRecord = dict => dict.eqRecord;
const eqRec = () => dictEqRecord => ({eq: dictEqRecord.eqRecord(Type$dProxy.Proxy)});
const eqProxy = {eq: v => v1 => true};
const eqNumber = {eq: eqNumberImpl};
const eqInt = {eq: eqIntImpl};
const eqChar = {eq: eqCharImpl};
const eqBoolean = {eq: eqBooleanImpl};
const eq1 = dict => dict.eq1;
const eq = dict => dict.eq;
const eqArray = dictEq => ({eq: eqArrayImpl(dictEq.eq)});
const eq1Array = {eq1: dictEq => eqArrayImpl(dictEq.eq)};
const eqRowCons = dictEqRecord => () => dictIsSymbol => dictEq => (
  {
    eqRecord: v => ra => rb => {
      const $$get = Record$dUnsafe.unsafeGet(dictIsSymbol.reflectSymbol(Type$dProxy.Proxy));
      return dictEq.eq($$get(ra))($$get(rb)) && dictEqRecord.eqRecord(Type$dProxy.Proxy)(ra)(rb);
    }
  }
);
const notEq = dictEq => x => y => !dictEq.eq(x)(y);
const notEq1 = dictEq1 => dictEq => {
  const eq12 = dictEq1.eq1(dictEq);
  return x => y => !eq12(x)(y);
};
export {  eq1Array,  eqBoolean, eqChar, eqInt, eqNumber, eqProxy,   eqRowCons, eqRowNil, eqString, eqUnit, eqVoid,  };
export * from "./foreign.js";
