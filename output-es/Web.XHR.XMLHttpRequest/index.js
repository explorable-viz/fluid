import * as $runtime from "../runtime.js";
import * as Data$dHTTP$dMethod from "../Data.HTTP.Method/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNullable from "../Data.Nullable/index.js";
import * as Effect$dUncurried from "../Effect.Uncurried/index.js";
import * as Unsafe$dCoerce from "../Unsafe.Coerce/index.js";
import * as Web$dXHR$dReadyState from "../Web.XHR.ReadyState/index.js";
import {_abort, _getAllResponseHeaders, _getProperty, _getResponseHeader, _open, _overrideMimeType, _send, _setProperty, _setRequestHeader, _xmlHttpRequest} from "./foreign.js";
const xmlHttpRequest = /* #__PURE__ */ Effect$dUncurried.runEffectFn1(_xmlHttpRequest);
const withCredentials = xhr => () => _getProperty("withCredentials", xhr);
const upload = xhr => () => _getProperty("upload", xhr);
const toEventTarget = Unsafe$dCoerce.unsafeCoerce;
const timeout = xhr => () => _getProperty("timeout", xhr);
const statusText = xhr => () => _getProperty("statusText", xhr);
const status = xhr => () => _getProperty("status", xhr);
const setWithCredentials = wc => xhr => () => _setProperty("withCredentials", wc, xhr);
const setTimeout = ms => xhr => () => _setProperty("timeout", ms, xhr);
const setRequestHeader = header => value => xhr => () => _setRequestHeader(header, value, xhr);
const sendString = payload => xhr => () => _send(payload, xhr);
const sendFormData = payload => xhr => () => _send(payload, xhr);
const sendDocument = payload => xhr => () => _send(payload, xhr);
const sendBlob = payload => xhr => () => _send(payload, xhr);
const sendArrayView = payload => xhr => () => _send(payload, xhr);
const send = xhr => () => _send(Data$dNullable.null, xhr);
const responseURL = xhr => () => _getProperty("responseURL", xhr);
const response = xhr => () => {
  const a$p = _getProperty("response", xhr);
  return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
};
const readyState = xhr => () => {
  const a$p = _getProperty("readyState", xhr);
  if (a$p === 0) { return Web$dXHR$dReadyState.Unsent; }
  if (a$p === 1) { return Web$dXHR$dReadyState.Opened; }
  if (a$p === 2) { return Web$dXHR$dReadyState.HeadersReceived; }
  if (a$p === 3) { return Web$dXHR$dReadyState.Loading; }
  if (a$p === 4) { return Web$dXHR$dReadyState.Done; }
  return Web$dXHR$dReadyState.Unsent;
};
const overrideMimeType = ty => req => () => _overrideMimeType(ty, req);
const open$p = options => xhr => {
  const $0 = Data$dHTTP$dMethod.print(options.method);
  const $1 = options.url;
  const $2 = (() => {
    if (options.username.tag === "Nothing") { return Data$dNullable.null; }
    if (options.username.tag === "Just") { return Data$dNullable.notNull(options.username._1); }
    $runtime.fail();
  })();
  const $3 = (() => {
    if (options.password.tag === "Nothing") { return Data$dNullable.null; }
    if (options.password.tag === "Just") { return Data$dNullable.notNull(options.password._1); }
    $runtime.fail();
  })();
  return () => _open($0, $1, $2, $3, xhr);
};
const open = method => url => xhr => {
  const $0 = Data$dHTTP$dMethod.print(method);
  return () => _open($0, url, Data$dNullable.null, Data$dNullable.null, xhr);
};
const getResponseHeader = header => xhr => () => {
  const a$p = _getResponseHeader(header, xhr);
  return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
};
const getAllResponseHeaders = xhr => () => {
  const a$p = _getAllResponseHeaders(xhr);
  return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
};
const abort = /* #__PURE__ */ Effect$dUncurried.runEffectFn1(_abort);
export {
  abort,
  getAllResponseHeaders,
  getResponseHeader,
  open,
  open$p,
  overrideMimeType,
  readyState,
  response,
  responseURL,
  send,
  sendArrayView,
  sendBlob,
  sendDocument,
  sendFormData,
  sendString,
  setRequestHeader,
  setTimeout,
  setWithCredentials,
  status,
  statusText,
  timeout,
  toEventTarget,
  upload,
  withCredentials,
  xmlHttpRequest
};
export * from "./foreign.js";
