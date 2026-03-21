import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNullable from "../Data.Nullable/index.js";
import * as Effect$dUncurried from "../Effect.Uncurried/index.js";
import * as Unsafe$dCoerce from "../Unsafe.Coerce/index.js";
import * as Web$dHTML$dHTMLImageElement$dCORSMode from "../Web.HTML.HTMLImageElement.CORSMode/index.js";
import * as Web$dHTML$dHTMLImageElement$dDecodingHint from "../Web.HTML.HTMLImageElement.DecodingHint/index.js";
import * as Web$dHTML$dHTMLImageElement$dLaziness from "../Web.HTML.HTMLImageElement.Laziness/index.js";
import * as Web$dInternal$dFFI from "../Web.Internal.FFI/index.js";
import {
  _crossOrigin,
  _decoding,
  _loading,
  _setCrossOrigin,
  _setDecoding,
  _setLoading,
  alt,
  complete,
  create,
  createWithDimensions,
  currentSrc,
  height,
  isMap,
  naturalHeight,
  naturalWidth,
  referrerPolicy,
  setAlt,
  setHeight,
  setIsMap,
  setReferrerPolicy,
  setSizes,
  setSrc,
  setSrcset,
  setUseMap,
  setWidth,
  sizes,
  src,
  srcset,
  useMap,
  width
} from "./foreign.js";
const toParentNode = Unsafe$dCoerce.unsafeCoerce;
const toNonDocumentTypeChildNode = Unsafe$dCoerce.unsafeCoerce;
const toNode = Unsafe$dCoerce.unsafeCoerce;
const toHTMLElement = Unsafe$dCoerce.unsafeCoerce;
const toEventTarget = Unsafe$dCoerce.unsafeCoerce;
const toElement = Unsafe$dCoerce.unsafeCoerce;
const toChildNode = Unsafe$dCoerce.unsafeCoerce;
const setLoading = laziness => Effect$dUncurried.runEffectFn2(_setLoading)((() => {
  if (laziness === "Eager") { return "eager"; }
  if (laziness === "Lazy") { return "lazy"; }
  $runtime.fail();
})());
const setDecoding = hint => Effect$dUncurried.runEffectFn2(_setDecoding)((() => {
  if (hint === "Sync") { return "sync"; }
  if (hint === "Async") { return "async"; }
  if (hint === "Auto") { return "auto"; }
  $runtime.fail();
})());
const setCrossOrigin = mode => Effect$dUncurried.runEffectFn2(_setCrossOrigin)((() => {
  if (mode === "Anonymous") { return "anonymous"; }
  if (mode === "UseCredentials") { return "use-credentials"; }
  $runtime.fail();
})());
const loading = /* #__PURE__ */ (() => {
  const $0 = Effect$dUncurried.runEffectFn1(_loading);
  return x => {
    const $1 = $0(x);
    return () => {
      const a$p = $1();
      if (a$p === "") { return Web$dHTML$dHTMLImageElement$dLaziness.Eager; }
      if (a$p === "eager") { return Web$dHTML$dHTMLImageElement$dLaziness.Eager; }
      if (a$p === "lazy") { return Web$dHTML$dHTMLImageElement$dLaziness.Lazy; }
      return Web$dHTML$dHTMLImageElement$dLaziness.Eager;
    };
  };
})();
const fromParentNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLImageElement");
const fromNonDocumentTypeChildNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLImageElement");
const fromNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLImageElement");
const fromHTMLElement = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLImageElement");
const fromEventTarget = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLImageElement");
const fromElement = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLImageElement");
const fromChildNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLImageElement");
const decoding = /* #__PURE__ */ (() => {
  const $0 = Effect$dUncurried.runEffectFn1(_decoding);
  return x => {
    const $1 = $0(x);
    return () => {
      const a$p = $1();
      if (a$p === "") { return Web$dHTML$dHTMLImageElement$dDecodingHint.Auto; }
      if (a$p === "sync") { return Web$dHTML$dHTMLImageElement$dDecodingHint.Sync; }
      if (a$p === "async") { return Web$dHTML$dHTMLImageElement$dDecodingHint.Async; }
      return Web$dHTML$dHTMLImageElement$dDecodingHint.Auto;
    };
  };
})();
const crossOrigin = /* #__PURE__ */ (() => {
  const $0 = Effect$dUncurried.runEffectFn1(_crossOrigin);
  return x => {
    const $1 = $0(x);
    return () => {
      const a$p = $1();
      const $2 = Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
      if ($2.tag === "Just") {
        if ($2._1 === "") { return Data$dMaybe.$Maybe("Just", Web$dHTML$dHTMLImageElement$dCORSMode.Anonymous); }
        if ($2._1 === "anonymous") { return Data$dMaybe.$Maybe("Just", Web$dHTML$dHTMLImageElement$dCORSMode.Anonymous); }
        if ($2._1 === "use-credentials") { return Data$dMaybe.$Maybe("Just", Web$dHTML$dHTMLImageElement$dCORSMode.UseCredentials); }
        return Data$dMaybe.Nothing;
      }
      if ($2.tag === "Nothing") { return Data$dMaybe.Nothing; }
      $runtime.fail();
    };
  };
})();
const create$p = createWithDimensions;
export {
  create$p,
  crossOrigin,
  decoding,
  fromChildNode,
  fromElement,
  fromEventTarget,
  fromHTMLElement,
  fromNode,
  fromNonDocumentTypeChildNode,
  fromParentNode,
  loading,
  setCrossOrigin,
  setDecoding,
  setLoading,
  toChildNode,
  toElement,
  toEventTarget,
  toHTMLElement,
  toNode,
  toNonDocumentTypeChildNode,
  toParentNode
};
export * from "./foreign.js";
