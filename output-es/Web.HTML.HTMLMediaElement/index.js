import * as Unsafe$dCoerce from "../Unsafe.Coerce/index.js";
import * as Web$dHTML$dHTMLMediaElement$dCanPlayType from "../Web.HTML.HTMLMediaElement.CanPlayType/index.js";
import * as Web$dHTML$dHTMLMediaElement$dNetworkState from "../Web.HTML.HTMLMediaElement.NetworkState/index.js";
import * as Web$dHTML$dHTMLMediaElement$dReadyState from "../Web.HTML.HTMLMediaElement.ReadyState/index.js";
import * as Web$dInternal$dFFI from "../Web.Internal.FFI/index.js";
import {
  _canPlayType,
  _networkState,
  _readyState,
  autoplay,
  controls,
  crossOrigin,
  currentSrc,
  currentTime,
  defaultMuted,
  defaultPlaybackRate,
  duration,
  ended,
  getStartDate,
  load,
  loop,
  mediaGroup,
  muted,
  pause,
  paused,
  play,
  playbackRate,
  preload,
  seeking,
  setAutoplay,
  setControls,
  setCrossOrigin,
  setCurrentTime,
  setDefaultMuted,
  setDefaultPlaybackRate,
  setLoop,
  setMediaGroup,
  setMuted,
  setPlaybackRate,
  setPreload,
  setSrc,
  setVolume,
  src,
  volume
} from "./foreign.js";
const toParentNode = Unsafe$dCoerce.unsafeCoerce;
const toNonDocumentTypeChildNode = Unsafe$dCoerce.unsafeCoerce;
const toNode = Unsafe$dCoerce.unsafeCoerce;
const toHTMLElement = Unsafe$dCoerce.unsafeCoerce;
const toEventTarget = Unsafe$dCoerce.unsafeCoerce;
const toElement = Unsafe$dCoerce.unsafeCoerce;
const toChildNode = Unsafe$dCoerce.unsafeCoerce;
const readyState = el => () => {
  const a$p = _readyState(el);
  if (a$p === 0) { return Web$dHTML$dHTMLMediaElement$dReadyState.HaveNothing; }
  if (a$p === 1) { return Web$dHTML$dHTMLMediaElement$dReadyState.HaveMetadata; }
  if (a$p === 2) { return Web$dHTML$dHTMLMediaElement$dReadyState.HaveCurrentData; }
  if (a$p === 3) { return Web$dHTML$dHTMLMediaElement$dReadyState.HaveFutureData; }
  if (a$p === 4) { return Web$dHTML$dHTMLMediaElement$dReadyState.HaveEnoughData; }
  return Web$dHTML$dHTMLMediaElement$dReadyState.HaveNothing;
};
const networkState = el => () => {
  const a$p = _networkState(el);
  if (a$p === 0) { return Web$dHTML$dHTMLMediaElement$dNetworkState.Empty; }
  if (a$p === 1) { return Web$dHTML$dHTMLMediaElement$dNetworkState.Idle; }
  if (a$p === 2) { return Web$dHTML$dHTMLMediaElement$dNetworkState.Loading; }
  if (a$p === 3) { return Web$dHTML$dHTMLMediaElement$dNetworkState.NoSource; }
  return Web$dHTML$dHTMLMediaElement$dNetworkState.Empty;
};
const fromParentNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLMediaElement");
const fromNonDocumentTypeChildNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLMediaElement");
const fromNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLMediaElement");
const fromHTMLElement = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLMediaElement");
const fromEventTarget = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLMediaElement");
const fromElement = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLMediaElement");
const fromChildNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLMediaElement");
const canPlayType = ty => el => () => {
  const a$p = _canPlayType(ty, el);
  if (a$p === "") { return Web$dHTML$dHTMLMediaElement$dCanPlayType.Unspecified; }
  if (a$p === "maybe") { return Web$dHTML$dHTMLMediaElement$dCanPlayType.Maybe; }
  if (a$p === "probably") { return Web$dHTML$dHTMLMediaElement$dCanPlayType.Probably; }
  return Web$dHTML$dHTMLMediaElement$dCanPlayType.Unspecified;
};
export {
  canPlayType,
  fromChildNode,
  fromElement,
  fromEventTarget,
  fromHTMLElement,
  fromNode,
  fromNonDocumentTypeChildNode,
  fromParentNode,
  networkState,
  readyState,
  toChildNode,
  toElement,
  toEventTarget,
  toHTMLElement,
  toNode,
  toNonDocumentTypeChildNode,
  toParentNode
};
export * from "./foreign.js";
