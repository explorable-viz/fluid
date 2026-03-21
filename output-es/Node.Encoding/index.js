import * as $runtime from "../runtime.js";
import {byteLengthImpl} from "./foreign.js";
const $Encoding = tag => tag;
const ASCII = /* #__PURE__ */ $Encoding("ASCII");
const UTF8 = /* #__PURE__ */ $Encoding("UTF8");
const UTF16LE = /* #__PURE__ */ $Encoding("UTF16LE");
const UCS2 = /* #__PURE__ */ $Encoding("UCS2");
const Base64 = /* #__PURE__ */ $Encoding("Base64");
const Base64Url = /* #__PURE__ */ $Encoding("Base64Url");
const Latin1 = /* #__PURE__ */ $Encoding("Latin1");
const Binary = /* #__PURE__ */ $Encoding("Binary");
const Hex = /* #__PURE__ */ $Encoding("Hex");
const showEncoding = {
  show: v => {
    if (v === "ASCII") { return "ASCII"; }
    if (v === "UTF8") { return "UTF8"; }
    if (v === "UTF16LE") { return "UTF16LE"; }
    if (v === "UCS2") { return "UCS2"; }
    if (v === "Base64") { return "Base64"; }
    if (v === "Base64Url") { return "Base64Url"; }
    if (v === "Latin1") { return "Latin1"; }
    if (v === "Binary") { return "Binary"; }
    if (v === "Hex") { return "Hex"; }
    $runtime.fail();
  }
};
const encodingToNode = v => {
  if (v === "ASCII") { return "ascii"; }
  if (v === "UTF8") { return "utf8"; }
  if (v === "UTF16LE") { return "utf16le"; }
  if (v === "UCS2") { return "ucs2"; }
  if (v === "Base64") { return "base64"; }
  if (v === "Base64Url") { return "base64url"; }
  if (v === "Latin1") { return "latin1"; }
  if (v === "Binary") { return "binary"; }
  if (v === "Hex") { return "hex"; }
  $runtime.fail();
};
const byteLength = str => enc => byteLengthImpl(
  str,
  (() => {
    if (enc === "ASCII") { return "ascii"; }
    if (enc === "UTF8") { return "utf8"; }
    if (enc === "UTF16LE") { return "utf16le"; }
    if (enc === "UCS2") { return "ucs2"; }
    if (enc === "Base64") { return "base64"; }
    if (enc === "Base64Url") { return "base64url"; }
    if (enc === "Latin1") { return "latin1"; }
    if (enc === "Binary") { return "binary"; }
    if (enc === "Hex") { return "hex"; }
    $runtime.fail();
  })()
);
export {$Encoding, ASCII, Base64, Base64Url, Binary, Hex, Latin1, UCS2, UTF16LE, UTF8, byteLength, encodingToNode, showEncoding};
export * from "./foreign.js";
