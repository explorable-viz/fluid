import * as $runtime from "../runtime.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Type$dProxy from "../Type.Proxy/index.js";
const reflectOrdering = dict => dict.reflectOrdering;
const isOrderingLT = {reflectOrdering: v => Data$dOrdering.LT};
const isOrderingGT = {reflectOrdering: v => Data$dOrdering.GT};
const isOrderingEQ = {reflectOrdering: v => Data$dOrdering.EQ};
const reifyOrdering = v => v1 => {
  if (v === "LT") { return v1(isOrderingLT)(Type$dProxy.Proxy); }
  if (v === "EQ") { return v1(isOrderingEQ)(Type$dProxy.Proxy); }
  if (v === "GT") { return v1(isOrderingGT)(Type$dProxy.Proxy); }
  $runtime.fail();
};
const invertOrderingLT = {};
const invertOrderingGT = {};
const invertOrderingEQ = {};
const invert = () => v => Type$dProxy.Proxy;
const equalsLTLT = {};
const equalsLTGT = {};
const equalsLTEQ = {};
const equalsGTLT = {};
const equalsGTGT = {};
const equalsGTEQ = {};
const equalsEQLT = {};
const equalsEQGT = {};
const equalsEQEQ = {};
const equals = () => v => v1 => Type$dProxy.Proxy;
const appendOrderingLT = {};
const appendOrderingGT = {};
const appendOrderingEQ = {};
const append = () => v => v1 => Type$dProxy.Proxy;
export {
  append,
  appendOrderingEQ,
  appendOrderingGT,
  appendOrderingLT,
  equals,
  equalsEQEQ,
  equalsEQGT,
  equalsEQLT,
  equalsGTEQ,
  equalsGTGT,
  equalsGTLT,
  equalsLTEQ,
  equalsLTGT,
  equalsLTLT,
  invert,
  invertOrderingEQ,
  invertOrderingGT,
  invertOrderingLT,
  isOrderingEQ,
  isOrderingGT,
  isOrderingLT,
  reflectOrdering,
  reifyOrdering
};
