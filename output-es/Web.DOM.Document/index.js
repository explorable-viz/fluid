// | This module provides type definitions and implementations for the
// | `Document` interface, which is part of the W3C DOM API.
// |
// | The DOM API doesn't actually give you any way of getting hold of a
// | `Document` by itself. To do that, you will need to look at one of the
// | other APIs which build on the DOM API. For example, `window.document` is
// | part of the HTML5 API, and so the relevant binding can be found in
// | `Web.HTML.Window`, which is part of the `purescript-web-html` package.
import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNullable from "../Data.Nullable/index.js";
import * as Unsafe$dCoerce from "../Unsafe.Coerce/index.js";
import * as Web$dInternal$dFFI from "../Web.Internal.FFI/index.js";
import {
  _createElementNS,
  _doctype,
  _documentElement,
  _getElementsByTagNameNS,
  adoptNode,
  characterSet,
  compatMode,
  contentType,
  createComment,
  createDocumentFragment,
  createElement,
  createProcessingInstruction,
  createTextNode,
  documentURI,
  getElementsByClassName,
  getElementsByTagName,
  importNode,
  origin,
  url
} from "./foreign.js";
const toParentNode = Unsafe$dCoerce.unsafeCoerce;
const toNonElementParentNode = Unsafe$dCoerce.unsafeCoerce;
const toNode = Unsafe$dCoerce.unsafeCoerce;
const toEventTarget = Unsafe$dCoerce.unsafeCoerce;
const getElementsByTagNameNS = x => _getElementsByTagNameNS((() => {
  if (x.tag === "Nothing") { return Data$dNullable.null; }
  if (x.tag === "Just") { return Data$dNullable.notNull(x._1); }
  $runtime.fail();
})());
const fromParentNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("Document");
const fromNonElementParentNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("Document");
const fromNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("Document");
const fromEventTarget = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("Document");
const documentElement = x => {
  const $0 = _documentElement(x);
  return () => {
    const a$p = $0();
    return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
  };
};
const doctype = x => Data$dNullable.nullable(_doctype(x), Data$dMaybe.Nothing, Data$dMaybe.Just);
const createElementNS = x => _createElementNS((() => {
  if (x.tag === "Nothing") { return Data$dNullable.null; }
  if (x.tag === "Just") { return Data$dNullable.notNull(x._1); }
  $runtime.fail();
})());
export {
  createElementNS,
  doctype,
  documentElement,
  fromEventTarget,
  fromNode,
  fromNonElementParentNode,
  fromParentNode,
  getElementsByTagNameNS,
  toEventTarget,
  toNode,
  toNonElementParentNode,
  toParentNode
};
export * from "./foreign.js";
