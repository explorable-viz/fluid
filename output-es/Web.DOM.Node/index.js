import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNullable from "../Data.Nullable/index.js";
import * as Unsafe$dCoerce from "../Unsafe.Coerce/index.js";
import * as Web$dDOM$dNodeType from "../Web.DOM.NodeType/index.js";
import * as Web$dInternal$dFFI from "../Web.Internal.FFI/index.js";
import {
  _firstChild,
  _lastChild,
  _lookupNamespaceURI,
  _lookupPrefix,
  _nextSibling,
  _nodeValue,
  _ownerDocument,
  _parentElement,
  _parentNode,
  _previousSibling,
  appendChild,
  baseURI,
  childNodes,
  clone,
  compareDocumentPositionBits,
  contains,
  deepClone,
  hasChildNodes,
  insertBefore,
  isDefaultNamespace,
  isEqualNode,
  nodeName,
  nodeTypeIndex,
  normalize,
  removeChild,
  replaceChild,
  setNodeValue,
  setTextContent,
  textContent
} from "./foreign.js";
const toEventTarget = Unsafe$dCoerce.unsafeCoerce;
const previousSibling = x => {
  const $0 = _previousSibling(x);
  return () => {
    const a$p = $0();
    return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
  };
};
const parentNode = x => {
  const $0 = _parentNode(x);
  return () => {
    const a$p = $0();
    return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
  };
};
const parentElement = x => {
  const $0 = _parentElement(x);
  return () => {
    const a$p = $0();
    return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
  };
};
const ownerDocument = x => {
  const $0 = _ownerDocument(x);
  return () => {
    const a$p = $0();
    return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
  };
};
const nodeValue = x => {
  const $0 = _nodeValue(x);
  return () => {
    const a$p = $0();
    return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
  };
};
const nodeType = () => x => {
  const $0 = nodeTypeIndex(x);
  if ($0 === 1) { return Web$dDOM$dNodeType.ElementNode; }
  if ($0 === 2) { return Web$dDOM$dNodeType.AttributeNode; }
  if ($0 === 3) { return Web$dDOM$dNodeType.TextNode; }
  if ($0 === 4) { return Web$dDOM$dNodeType.CDATASectionNode; }
  if ($0 === 5) { return Web$dDOM$dNodeType.EntityReferenceNode; }
  if ($0 === 6) { return Web$dDOM$dNodeType.EntityNode; }
  if ($0 === 7) { return Web$dDOM$dNodeType.ProcessingInstructionNode; }
  if ($0 === 8) { return Web$dDOM$dNodeType.CommentNode; }
  if ($0 === 9) { return Web$dDOM$dNodeType.DocumentNode; }
  if ($0 === 10) { return Web$dDOM$dNodeType.DocumentTypeNode; }
  if ($0 === 11) { return Web$dDOM$dNodeType.DocumentFragmentNode; }
  if ($0 === 12) { return Web$dDOM$dNodeType.NotationNode; }
  $runtime.fail();
};
const nextSibling = x => {
  const $0 = _nextSibling(x);
  return () => {
    const a$p = $0();
    return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
  };
};
const lookupPrefix = p => {
  const $0 = _lookupPrefix(p);
  return x => {
    const $1 = $0(x);
    return () => {
      const a$p = $1();
      return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
    };
  };
};
const lookupNamespaceURI = ns => {
  const $0 = _lookupNamespaceURI(ns);
  return x => {
    const $1 = $0(x);
    return () => {
      const a$p = $1();
      return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
    };
  };
};
const lastChild = x => {
  const $0 = _lastChild(x);
  return () => {
    const a$p = $0();
    return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
  };
};
const fromEventTarget = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("Node");
const firstChild = x => {
  const $0 = _firstChild(x);
  return () => {
    const a$p = $0();
    return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
  };
};
export {
  firstChild,
  fromEventTarget,
  lastChild,
  lookupNamespaceURI,
  lookupPrefix,
  nextSibling,
  nodeType,
  nodeValue,
  ownerDocument,
  parentElement,
  parentNode,
  previousSibling,
  toEventTarget
};
export * from "./foreign.js";
