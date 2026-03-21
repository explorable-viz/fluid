import * as Type$dProxy from "../Type.Proxy/index.js";
const reflectBoolean = dict => dict.reflectBoolean;
const orTrue = {};
const orFalse = {};
const or = () => v => v1 => Type$dProxy.Proxy;
const notTrue = {};
const notFalse = {};
const not = () => v => Type$dProxy.Proxy;
const isBooleanTrue = {reflectBoolean: v => true};
const isBooleanFalse = {reflectBoolean: v => false};
const reifyBoolean = v => v1 => {
  if (v) { return v1(isBooleanTrue)(Type$dProxy.Proxy); }
  return v1(isBooleanFalse)(Type$dProxy.Proxy);
};
const if_ = () => v => v1 => v2 => Type$dProxy.Proxy;
const ifTrue = {};
const ifFalse = {};
const andTrue = {};
const andFalse = {};
const and = () => v => v1 => Type$dProxy.Proxy;
export {and, andFalse, andTrue, ifFalse, ifTrue, if_, isBooleanFalse, isBooleanTrue, not, notFalse, notTrue, or, orFalse, orTrue, reflectBoolean, reifyBoolean};
