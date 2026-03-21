import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNullable from "../Data.Nullable/index.js";
import * as Unsafe$dCoerce from "../Unsafe.Coerce/index.js";
import * as Web$dHTML$dHTMLDocument$dReadyState from "../Web.HTML.HTMLDocument.ReadyState/index.js";
import * as Web$dHTML$dHTMLDocument$dVisibilityState from "../Web.HTML.HTMLDocument.VisibilityState/index.js";
import * as Web$dInternal$dFFI from "../Web.Internal.FFI/index.js";
import {_activeElement, _body, _currentScript, _documentElement, _head, _readyState, _referrer, _setTitle, _title, _visibilityState} from "./foreign.js";
const visibilityState = doc => () => {
  const a$p = _visibilityState(doc);
  if (a$p === "visible") { return Web$dHTML$dHTMLDocument$dVisibilityState.Visible; }
  if (a$p === "hidden") { return Web$dHTML$dHTMLDocument$dVisibilityState.Hidden; }
  return Web$dHTML$dHTMLDocument$dVisibilityState.Visible;
};
const toParentNode = Unsafe$dCoerce.unsafeCoerce;
const toNonElementParentNode = Unsafe$dCoerce.unsafeCoerce;
const toNode = Unsafe$dCoerce.unsafeCoerce;
const toEventTarget = Unsafe$dCoerce.unsafeCoerce;
const toDocument = Unsafe$dCoerce.unsafeCoerce;
const title = doc => () => _title(doc);
const setTitle = newTitle => doc => () => _setTitle(newTitle, doc);
const referrer = doc => () => _referrer(doc);
const readyState = doc => () => {
  const a$p = _readyState(doc);
  if (a$p === "loading") { return Web$dHTML$dHTMLDocument$dReadyState.Loading; }
  if (a$p === "interactive") { return Web$dHTML$dHTMLDocument$dReadyState.Interactive; }
  if (a$p === "complete") { return Web$dHTML$dHTMLDocument$dReadyState.Complete; }
  return Web$dHTML$dHTMLDocument$dReadyState.Loading;
};
const head = doc => () => {
  const a$p = _head(doc);
  return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
};
const fromParentNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLDocument");
const fromNonElementParentNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLDocument");
const fromNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLDocument");
const fromEventTarget = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLDocument");
const fromDocument = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLDocument");
const documentElement = doc => () => {
  const a$p = _documentElement(doc);
  return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
};
const currentScript = doc => () => {
  const a$p = _currentScript(doc);
  return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
};
const body = doc => () => {
  const a$p = _body(doc);
  return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
};
const activeElement = doc => () => {
  const a$p = _activeElement(doc);
  return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
};
export {
  activeElement,
  body,
  currentScript,
  documentElement,
  fromDocument,
  fromEventTarget,
  fromNode,
  fromNonElementParentNode,
  fromParentNode,
  head,
  readyState,
  referrer,
  setTitle,
  title,
  toDocument,
  toEventTarget,
  toNode,
  toNonElementParentNode,
  toParentNode,
  visibilityState
};
export * from "./foreign.js";
