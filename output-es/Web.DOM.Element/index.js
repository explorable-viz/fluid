import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNullable from "../Data.Nullable/index.js";
import * as Unsafe$dCoerce from "../Unsafe.Coerce/index.js";
import * as Web$dInternal$dFFI from "../Web.Internal.FFI/index.js";
import {
  _attachShadow,
  _closest,
  _getAttribute,
  _getElementsByTagNameNS,
  _namespaceURI,
  _prefix,
  classList,
  className,
  clientHeight,
  clientLeft,
  clientTop,
  clientWidth,
  getBoundingClientRect,
  getElementsByClassName,
  getElementsByTagName,
  hasAttribute,
  id,
  localName,
  matches,
  removeAttribute,
  scrollHeight,
  scrollLeft,
  scrollTop,
  scrollWidth,
  setAttribute,
  setClassName,
  setId,
  setScrollLeft,
  setScrollTop,
  tagName
} from "./foreign.js";
const toParentNode = Unsafe$dCoerce.unsafeCoerce;
const toNonDocumentTypeChildNode = Unsafe$dCoerce.unsafeCoerce;
const toNode = Unsafe$dCoerce.unsafeCoerce;
const toEventTarget = Unsafe$dCoerce.unsafeCoerce;
const toChildNode = Unsafe$dCoerce.unsafeCoerce;
const prefix = x => Data$dNullable.nullable(_prefix(x), Data$dMaybe.Nothing, Data$dMaybe.Just);
const namespaceURI = x => Data$dNullable.nullable(_namespaceURI(x), Data$dMaybe.Nothing, Data$dMaybe.Just);
const getElementsByTagNameNS = x => _getElementsByTagNameNS((() => {
  if (x.tag === "Nothing") { return Data$dNullable.null; }
  if (x.tag === "Just") { return Data$dNullable.notNull(x._1); }
  $runtime.fail();
})());
const getAttribute = attr => {
  const $0 = _getAttribute(attr);
  return x => {
    const $1 = $0(x);
    return () => {
      const a$p = $1();
      return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
    };
  };
};
const fromParentNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("Element");
const fromNonDocumentTypeChildNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("Element");
const fromNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("Element");
const fromEventTarget = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("Element");
const fromChildNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("Element");
const closest = qs => {
  const $0 = _closest(qs);
  return x => {
    const $1 = $0(x);
    return () => {
      const a$p = $1();
      return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
    };
  };
};
const attachShadow = x => _attachShadow({
  mode: (() => {
    if (x.mode === "Open") { return "open"; }
    if (x.mode === "Closed") { return "closed"; }
    $runtime.fail();
  })(),
  delegatesFocus: x.delegatesFocus
});
export {
  attachShadow,
  closest,
  fromChildNode,
  fromEventTarget,
  fromNode,
  fromNonDocumentTypeChildNode,
  fromParentNode,
  getAttribute,
  getElementsByTagNameNS,
  namespaceURI,
  prefix,
  toChildNode,
  toEventTarget,
  toNode,
  toNonDocumentTypeChildNode,
  toParentNode
};
export * from "./foreign.js";
