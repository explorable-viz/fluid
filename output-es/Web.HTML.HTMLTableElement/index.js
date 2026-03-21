import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNullable from "../Data.Nullable/index.js";
import * as Unsafe$dCoerce from "../Unsafe.Coerce/index.js";
import * as Web$dInternal$dFFI from "../Web.Internal.FFI/index.js";
import {
  _caption,
  _setCaption,
  _setTFoot,
  _setTHead,
  _tFoot,
  _tHead,
  border,
  createCaption,
  createTBody,
  createTFoot,
  createTHead,
  deleteCaption,
  deleteRow,
  deleteTFoot,
  deleteTHead,
  insertRowAt,
  rows,
  setBorder,
  tBodies
} from "./foreign.js";
const toParentNode = Unsafe$dCoerce.unsafeCoerce;
const toNonDocumentTypeChildNode = Unsafe$dCoerce.unsafeCoerce;
const toNode = Unsafe$dCoerce.unsafeCoerce;
const toHTMLElement = Unsafe$dCoerce.unsafeCoerce;
const toEventTarget = Unsafe$dCoerce.unsafeCoerce;
const toElement = Unsafe$dCoerce.unsafeCoerce;
const toChildNode = Unsafe$dCoerce.unsafeCoerce;
const tHead = x => {
  const $0 = _tHead(x);
  return () => {
    const a$p = $0();
    return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
  };
};
const tFoot = x => {
  const $0 = _tFoot(x);
  return () => {
    const a$p = $0();
    return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
  };
};
const setTHead = x => _setTHead((() => {
  if (x.tag === "Nothing") { return Data$dNullable.null; }
  if (x.tag === "Just") { return Data$dNullable.notNull(x._1); }
  $runtime.fail();
})());
const setTFoot = x => _setTFoot((() => {
  if (x.tag === "Nothing") { return Data$dNullable.null; }
  if (x.tag === "Just") { return Data$dNullable.notNull(x._1); }
  $runtime.fail();
})());
const setCaption = x => _setCaption((() => {
  if (x.tag === "Nothing") { return Data$dNullable.null; }
  if (x.tag === "Just") { return Data$dNullable.notNull(x._1); }
  $runtime.fail();
})());
const insertRow$p = insertRowAt;
const insertRow = /* #__PURE__ */ insertRowAt(-1);
const fromParentNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLTableElement");
const fromNonDocumentTypeChildNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLTableElement");
const fromNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLTableElement");
const fromHTMLElement = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLTableElement");
const fromEventTarget = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLTableElement");
const fromElement = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLTableElement");
const fromChildNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLTableElement");
const caption = x => {
  const $0 = _caption(x);
  return () => {
    const a$p = $0();
    return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
  };
};
export {
  caption,
  fromChildNode,
  fromElement,
  fromEventTarget,
  fromHTMLElement,
  fromNode,
  fromNonDocumentTypeChildNode,
  fromParentNode,
  insertRow,
  insertRow$p,
  setCaption,
  setTFoot,
  setTHead,
  tFoot,
  tHead,
  toChildNode,
  toElement,
  toEventTarget,
  toHTMLElement,
  toNode,
  toNonDocumentTypeChildNode,
  toParentNode
};
export * from "./foreign.js";
