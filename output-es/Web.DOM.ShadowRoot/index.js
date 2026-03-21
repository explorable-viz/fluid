import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Unsafe$dCoerce from "../Unsafe.Coerce/index.js";
import {_mode, host} from "./foreign.js";
const $ShadowRootMode = tag => tag;
const Open = /* #__PURE__ */ $ShadowRootMode("Open");
const Closed = /* #__PURE__ */ $ShadowRootMode("Closed");
const toNode = Unsafe$dCoerce.unsafeCoerce;
const showShadowRootMode = {
  show: v => {
    if (v === "Open") { return "open"; }
    if (v === "Closed") { return "closed"; }
    $runtime.fail();
  }
};
const mode = x => {
  const $0 = _mode(x);
  if ($0 === "open") { return Data$dMaybe.$Maybe("Just", Open); }
  if ($0 === "closed") { return Data$dMaybe.$Maybe("Just", Closed); }
  return Data$dMaybe.Nothing;
};
export {$ShadowRootMode, Closed, Open, mode, showShadowRootMode, toNode};
export * from "./foreign.js";
