import * as $runtime from "../runtime.js";
const $BufferValueType = tag => tag;
const UInt8 = /* #__PURE__ */ $BufferValueType("UInt8");
const UInt16LE = /* #__PURE__ */ $BufferValueType("UInt16LE");
const UInt16BE = /* #__PURE__ */ $BufferValueType("UInt16BE");
const UInt32LE = /* #__PURE__ */ $BufferValueType("UInt32LE");
const UInt32BE = /* #__PURE__ */ $BufferValueType("UInt32BE");
const Int8 = /* #__PURE__ */ $BufferValueType("Int8");
const Int16LE = /* #__PURE__ */ $BufferValueType("Int16LE");
const Int16BE = /* #__PURE__ */ $BufferValueType("Int16BE");
const Int32LE = /* #__PURE__ */ $BufferValueType("Int32LE");
const Int32BE = /* #__PURE__ */ $BufferValueType("Int32BE");
const FloatLE = /* #__PURE__ */ $BufferValueType("FloatLE");
const FloatBE = /* #__PURE__ */ $BufferValueType("FloatBE");
const DoubleLE = /* #__PURE__ */ $BufferValueType("DoubleLE");
const DoubleBE = /* #__PURE__ */ $BufferValueType("DoubleBE");
const showBufferValueType = {
  show: v => {
    if (v === "UInt8") { return "UInt8"; }
    if (v === "UInt16LE") { return "UInt16LE"; }
    if (v === "UInt16BE") { return "UInt16BE"; }
    if (v === "UInt32LE") { return "UInt32LE"; }
    if (v === "UInt32BE") { return "UInt32BE"; }
    if (v === "Int8") { return "Int8"; }
    if (v === "Int16LE") { return "Int16LE"; }
    if (v === "Int16BE") { return "Int16BE"; }
    if (v === "Int32LE") { return "Int32LE"; }
    if (v === "Int32BE") { return "Int32BE"; }
    if (v === "FloatLE") { return "FloatLE"; }
    if (v === "FloatBE") { return "FloatBE"; }
    if (v === "DoubleLE") { return "DoubleLE"; }
    if (v === "DoubleBE") { return "DoubleBE"; }
    $runtime.fail();
  }
};
export {$BufferValueType, DoubleBE, DoubleLE, FloatBE, FloatLE, Int16BE, Int16LE, Int32BE, Int32LE, Int8, UInt16BE, UInt16LE, UInt32BE, UInt32LE, UInt8, showBufferValueType};
