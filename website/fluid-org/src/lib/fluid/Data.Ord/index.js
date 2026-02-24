import * as $runtime from "../runtime.js";
import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Record$dUnsafe from "../Record.Unsafe/index.js";
import * as Type$dProxy from "../Type.Proxy/index.js";
import {ordArrayImpl, ordBooleanImpl, ordCharImpl, ordIntImpl, ordNumberImpl, ordStringImpl} from "./foreign.js";
const ordVoid = {compare: v => v1 => Data$dOrdering.EQ, Eq0: () => Data$dEq.eqVoid};
const ordUnit = {compare: v => v1 => Data$dOrdering.EQ, Eq0: () => Data$dEq.eqUnit};
const ordString = {compare: /* #__PURE__ */ ordStringImpl(Data$dOrdering.LT)(Data$dOrdering.EQ)(Data$dOrdering.GT), Eq0: () => Data$dEq.eqString};
const ordRecordNil = {compareRecord: v => v1 => v2 => Data$dOrdering.EQ, EqRecord0: () => Data$dEq.eqRowNil};
const ordProxy = {compare: v => v1 => Data$dOrdering.EQ, Eq0: () => Data$dEq.eqProxy};
const ordOrdering = {
  compare: v => v1 => {
    if (v === "LT") {
      if (v1 === "LT") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (v === "EQ") {
      if (v1 === "EQ") { return Data$dOrdering.EQ; }
      if (v1 === "LT") { return Data$dOrdering.GT; }
      if (v1 === "GT") { return Data$dOrdering.LT; }
      $runtime.fail();
    }
    if (v === "GT") {
      if (v1 === "GT") { return Data$dOrdering.EQ; }
      return Data$dOrdering.GT;
    }
    $runtime.fail();
  },
  Eq0: () => Data$dOrdering.eqOrdering
};
const ordNumber = {compare: /* #__PURE__ */ ordNumberImpl(Data$dOrdering.LT)(Data$dOrdering.EQ)(Data$dOrdering.GT), Eq0: () => Data$dEq.eqNumber};
const ordInt = {compare: /* #__PURE__ */ ordIntImpl(Data$dOrdering.LT)(Data$dOrdering.EQ)(Data$dOrdering.GT), Eq0: () => Data$dEq.eqInt};
const ordChar = {compare: /* #__PURE__ */ ordCharImpl(Data$dOrdering.LT)(Data$dOrdering.EQ)(Data$dOrdering.GT), Eq0: () => Data$dEq.eqChar};
const ordBoolean = {compare: /* #__PURE__ */ ordBooleanImpl(Data$dOrdering.LT)(Data$dOrdering.EQ)(Data$dOrdering.GT), Eq0: () => Data$dEq.eqBoolean};
const compareRecord = dict => dict.compareRecord;
const ordRecord = () => dictOrdRecord => {
  const eqRec1 = {eq: dictOrdRecord.EqRecord0().eqRecord(Type$dProxy.Proxy)};
  return {compare: dictOrdRecord.compareRecord(Type$dProxy.Proxy), Eq0: () => eqRec1};
};
const compare1 = dict => dict.compare1;
const compare = dict => dict.compare;
const comparing = dictOrd => f => x => y => dictOrd.compare(f(x))(f(y));
const greaterThan = dictOrd => a1 => a2 => dictOrd.compare(a1)(a2) === "GT";
const greaterThanOrEq = dictOrd => a1 => a2 => dictOrd.compare(a1)(a2) !== "LT";
const lessThan = dictOrd => a1 => a2 => dictOrd.compare(a1)(a2) === "LT";
const signum = dictOrd => dictRing => {
  const Semiring0 = dictRing.Semiring0();
  const zero = Semiring0.zero;
  const zero$1 = dictRing.Semiring0().zero;
  const one = Semiring0.one;
  return x => {
    if (dictOrd.compare(x)(zero) === "LT") { return dictRing.sub(zero$1)(one); }
    if (dictOrd.compare(x)(zero) === "GT") { return one; }
    return x;
  };
};
const lessThanOrEq = dictOrd => a1 => a2 => dictOrd.compare(a1)(a2) !== "GT";
const max = dictOrd => x => y => {
  const v = dictOrd.compare(x)(y);
  if (v === "LT") { return y; }
  if (v === "EQ") { return x; }
  if (v === "GT") { return x; }
  $runtime.fail();
};
const min = dictOrd => x => y => {
  const v = dictOrd.compare(x)(y);
  if (v === "LT") { return x; }
  if (v === "EQ") { return x; }
  if (v === "GT") { return y; }
  $runtime.fail();
};
const ordArray = dictOrd => {
  const eqArray = {eq: Data$dEq.eqArrayImpl(dictOrd.Eq0().eq)};
  return {
    compare: xs => ys => ordInt.compare(0)(ordArrayImpl(x => y => {
      const v = dictOrd.compare(x)(y);
      if (v === "EQ") { return 0; }
      if (v === "LT") { return 1; }
      if (v === "GT") { return -1; }
      $runtime.fail();
    })(xs)(ys)),
    Eq0: () => eqArray
  };
};
const ord1Array = {compare1: dictOrd => ordArray(dictOrd).compare, Eq10: () => Data$dEq.eq1Array};
const ordRecordCons = dictOrdRecord => {
  const eqRowCons = Data$dEq.eqRowCons(dictOrdRecord.EqRecord0())();
  return () => dictIsSymbol => {
    const eqRowCons1 = eqRowCons(dictIsSymbol);
    return dictOrd => {
      const eqRowCons2 = eqRowCons1(dictOrd.Eq0());
      return {
        compareRecord: v => ra => rb => {
          const key = dictIsSymbol.reflectSymbol(Type$dProxy.Proxy);
          const left = dictOrd.compare(Record$dUnsafe.unsafeGet(key)(ra))(Record$dUnsafe.unsafeGet(key)(rb));
          if (left === "LT" || left === "GT" || left !== "EQ") { return left; }
          return dictOrdRecord.compareRecord(Type$dProxy.Proxy)(ra)(rb);
        },
        EqRecord0: () => eqRowCons2
      };
    };
  };
};
const clamp = dictOrd => low => hi => x => {
  const v = dictOrd.compare(low)(x);
  const $0 = (() => {
    if (v === "LT") { return x; }
    if (v === "EQ") { return low; }
    if (v === "GT") { return low; }
    $runtime.fail();
  })();
  const v$1 = dictOrd.compare(hi)($0);
  if (v$1 === "LT") { return hi; }
  if (v$1 === "EQ") { return hi; }
  if (v$1 === "GT") { return $0; }
  $runtime.fail();
};
const between = dictOrd => low => hi => x => {
  if (dictOrd.compare(x)(low) === "LT") { return false; }
  return dictOrd.compare(x)(hi) !== "GT";
};
const abs = dictOrd => dictRing => {
  const zero = dictRing.Semiring0().zero;
  const zero$1 = dictRing.Semiring0().zero;
  return x => {
    if (dictOrd.compare(x)(zero) !== "LT") { return x; }
    return dictRing.sub(zero$1)(x);
  };
};
export {
  
  
  
  
  
  
  
  
  
  
  
  
  
  ord1Array,
  ordArray,
  ordBoolean,
  ordChar,
  ordInt,
  ordNumber,
  ordOrdering,
  ordProxy,
  
  
  ordRecordNil,
  ordString,
  ordUnit,
  
  
};
export * from "./foreign.js";
