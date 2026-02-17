// | This module defines a data type and various functions for creating and
// | manipulating JSON values. The README contains additional documentation
// | for this module.
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
import {_caseJson, _compare, fromArray, fromBoolean, fromNumber, fromObject, fromString, jsonNull, stringify, stringifyWithIndent} from "./foreign.js";
const jsonZero = /* #__PURE__ */ fromNumber(0.0);
const jsonTrue = /* #__PURE__ */ fromBoolean(true);
const jsonSingletonObject = key => val => fromObject((() => {
  const $0 = {};
  $0[key] = val;
  return $0;
})());
const jsonSingletonArray = j => fromArray([j]);
const jsonFalse = /* #__PURE__ */ fromBoolean(false);
const jsonEmptyString = /* #__PURE__ */ fromString("");
const jsonEmptyObject = /* #__PURE__ */ fromObject(Foreign$dObject.empty);
const jsonEmptyArray = /* #__PURE__ */ fromArray([]);
const ordJson = {compare: a => b => _compare(Data$dOrdering.EQ, Data$dOrdering.GT, Data$dOrdering.LT, a, b), Eq0: () => eqJson};
const eqJson = {eq: j1 => j2 => _compare(Data$dOrdering.EQ, Data$dOrdering.GT, Data$dOrdering.LT, j1, j2) === "EQ"};
const caseJsonString = d => f => j => _caseJson(v => d, v => d, v => d, f, v => d, v => d, j);
const isString = /* #__PURE__ */ caseJsonString(false)(v => true);
const toString = /* #__PURE__ */ caseJsonString(Data$dMaybe.Nothing)(Data$dMaybe.Just);
const caseJsonObject = d => f => j => _caseJson(v => d, v => d, v => d, v => d, v => d, f, j);
const isObject = /* #__PURE__ */ caseJsonObject(false)(v => true);
const toObject = /* #__PURE__ */ caseJsonObject(Data$dMaybe.Nothing)(Data$dMaybe.Just);
const caseJsonNumber = d => f => j => _caseJson(v => d, v => d, f, v => d, v => d, v => d, j);
const isNumber = /* #__PURE__ */ caseJsonNumber(false)(v => true);
const toNumber = /* #__PURE__ */ caseJsonNumber(Data$dMaybe.Nothing)(Data$dMaybe.Just);
const caseJsonNull = d => f => j => _caseJson(f, v => d, v => d, v => d, v => d, v => d, j);
const isNull = /* #__PURE__ */ caseJsonNull(false)(v => true);
const toNull = /* #__PURE__ */ caseJsonNull(Data$dMaybe.Nothing)(Data$dMaybe.Just);
const caseJsonBoolean = d => f => j => _caseJson(v => d, f, v => d, v => d, v => d, v => d, j);
const isBoolean = /* #__PURE__ */ caseJsonBoolean(false)(v => true);
const toBoolean = /* #__PURE__ */ caseJsonBoolean(Data$dMaybe.Nothing)(Data$dMaybe.Just);
const caseJsonArray = d => f => j => _caseJson(v => d, v => d, v => d, v => d, f, v => d, j);
const isArray = /* #__PURE__ */ caseJsonArray(false)(v => true);
const toArray = /* #__PURE__ */ caseJsonArray(Data$dMaybe.Nothing)(Data$dMaybe.Just);
const caseJson = a => b => c => d => e => f => json => _caseJson(a, b, c, d, e, f, json);
export {
  caseJson,
  
  caseJsonBoolean,
  caseJsonNull,
  caseJsonNumber,
  
  caseJsonString,
  
  
  
  
  
  
  
  
  jsonEmptyObject,
  
  
  
  
  
  
  
  
  
  
  
  
  
};
export * from "./foreign.js";
