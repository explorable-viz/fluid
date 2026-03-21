import * as $runtime from "../runtime.js";
import * as Unsafe$dCoerce from "../Unsafe.Coerce/index.js";
import * as Web$dFile$dFileReader$dReadyState from "../Web.File.FileReader.ReadyState/index.js";
import * as Web$dInternal$dFFI from "../Web.Internal.FFI/index.js";
import {abort, error, fileReader, readAsArrayBuffer, readAsDataURL, readAsText, readyStateImpl, result} from "./foreign.js";
const toEventTarget = Unsafe$dCoerce.unsafeCoerce;
const readyState = fr => {
  const $0 = readyStateImpl(fr);
  return () => {
    const rs = $0();
    if (rs === 0) { return Web$dFile$dFileReader$dReadyState.EMPTY; }
    if (rs === 1) { return Web$dFile$dFileReader$dReadyState.LOADING; }
    if (rs === 2) { return Web$dFile$dFileReader$dReadyState.DONE; }
    $runtime.fail();
  };
};
const fromEventTarget = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("FileReader");
export {fromEventTarget, readyState, toEventTarget};
export * from "./foreign.js";
