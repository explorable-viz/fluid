import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNullable from "../Data.Nullable/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Unsafe$dCoerce from "../Unsafe.Coerce/index.js";
import * as Web$dInternal$dFFI from "../Web.Internal.FFI/index.js";
import {
  _open,
  _opener,
  _prompt,
  alert,
  cancelAnimationFrame,
  cancelIdleCallback,
  close,
  confirm,
  document,
  history,
  innerHeight,
  innerWidth,
  localStorage,
  location,
  moveBy,
  moveTo,
  navigator,
  outerHeight,
  outerWidth,
  parent,
  print,
  requestAnimationFrame,
  requestIdleCallback,
  resizeBy,
  resizeTo,
  screenX,
  screenY,
  scroll,
  scrollBy,
  scrollX,
  scrollY,
  sessionStorage
} from "./foreign.js";
const toEventTarget = Unsafe$dCoerce.unsafeCoerce;
const promptDefault = msg => defaultText => window => {
  const $0 = _prompt(msg)(defaultText)(window);
  return () => {
    const a$p = $0();
    return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
  };
};
const prompt = msg => window => {
  const $0 = _prompt(msg)("")(window);
  return () => {
    const a$p = $0();
    return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
  };
};
const opener = window => {
  const $0 = _opener(window);
  return () => {
    const a$p = $0();
    return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
  };
};
const open = url$p => name => features => window => {
  const $0 = _open(url$p)(name)(features)(window);
  return () => {
    const a$p = $0();
    return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
  };
};
const fromEventTarget = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("Window");
const eqRequestIdleCallbackId = {eq: x => y => x === y};
const ordRequestIdleCallbackId = {compare: x => y => Data$dOrd.ordInt.compare(x)(y), Eq0: () => eqRequestIdleCallbackId};
const eqRequestAnimationFrameId = {eq: x => y => x === y};
const ordRequestAnimationFrameId = {compare: x => y => Data$dOrd.ordInt.compare(x)(y), Eq0: () => eqRequestAnimationFrameId};
export {
  eqRequestAnimationFrameId,
  eqRequestIdleCallbackId,
  fromEventTarget,
  open,
  opener,
  ordRequestAnimationFrameId,
  ordRequestIdleCallbackId,
  prompt,
  promptDefault,
  toEventTarget
};
export * from "./foreign.js";
