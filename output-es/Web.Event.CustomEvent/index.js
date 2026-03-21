import * as $runtime from "../runtime.js";
import * as Data$dNullable from "../Data.Nullable/index.js";
import * as Unsafe$dCoerce from "../Unsafe.Coerce/index.js";
import * as Web$dInternal$dFFI from "../Web.Internal.FFI/index.js";
import {new as $$new, detail, newOptionsImpl} from "./foreign.js";
const toEvent = Unsafe$dCoerce.unsafeCoerce;
const newWithOptions = ty => v => newOptionsImpl(ty)({
  detail: (() => {
    if (v.detail.tag === "Nothing") { return Data$dNullable.null; }
    if (v.detail.tag === "Just") { return Data$dNullable.notNull(v.detail._1); }
    $runtime.fail();
  })(),
  bubbles: v.bubbles,
  cancelable: v.cancelable,
  composed: v.composed
});
const new$p = ty => det => newWithOptions(ty)({detail: det, bubbles: false, cancelable: false, composed: false});
const fromEvent = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("CustomEvent");
export {fromEvent, new$p, newWithOptions, toEvent};
export * from "./foreign.js";
