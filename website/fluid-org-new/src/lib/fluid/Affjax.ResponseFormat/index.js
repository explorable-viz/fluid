import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
const $ResponseFormat = (tag, _1) => ({tag, _1});
const identity = x => x;
const $$ArrayBuffer = value0 => $ResponseFormat("ArrayBuffer", value0);
const Blob = value0 => $ResponseFormat("Blob", value0);
const Document = value0 => $ResponseFormat("Document", value0);
const Json = value0 => $ResponseFormat("Json", value0);
const $$String = value0 => $ResponseFormat("String", value0);
const Ignore = value0 => $ResponseFormat("Ignore", value0);
const toResponseType = v => {
  if (v.tag === "ArrayBuffer") { return "arraybuffer"; }
  if (v.tag === "Blob") { return "blob"; }
  if (v.tag === "Document") { return "document"; }
  if (v.tag === "Json") { return "text"; }
  if (v.tag === "String") { return "text"; }
  if (v.tag === "Ignore") { return ""; }
  $runtime.fail();
};
const toMediaType = v => {
  if (v.tag === "Json") { return Data$dMaybe.$Maybe("Just", "application/json"); }
  return Data$dMaybe.Nothing;
};
const string = /* #__PURE__ */ $ResponseFormat("String", identity);
const json = /* #__PURE__ */ $ResponseFormat("Json", identity);
const ignore = /* #__PURE__ */ $ResponseFormat("Ignore", identity);
const document = /* #__PURE__ */ $ResponseFormat("Document", identity);
const blob = /* #__PURE__ */ $ResponseFormat("Blob", identity);
const arrayBuffer = /* #__PURE__ */ $ResponseFormat("ArrayBuffer", identity);
export {
  $ResponseFormat,
  
  
  
  
  
  
  
  
  
  identity,
  
  
  
  
  
};
