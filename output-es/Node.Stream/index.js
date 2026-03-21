// | This module provides a low-level wrapper for the [Node Stream API (v18 LTS)](https://nodejs.org/docs/latest-v18.x/api/stream.html).
import * as $runtime from "../runtime.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNullable from "../Data.Nullable/index.js";
import * as Effect$dException from "../Effect.Exception/index.js";
import * as Effect$dUncurried from "../Effect.Uncurried/index.js";
import * as Node$dBuffer$dImmutable from "../Node.Buffer.Immutable/index.js";
import * as Node$dEventEmitter from "../Node.EventEmitter/index.js";
import * as Unsafe$dCoerce from "../Unsafe.Coerce/index.js";
import {
  allowHalfOpenImpl,
  closedImpl,
  corkImpl,
  destroyErrorImpl,
  destroyImpl,
  destroyedImpl,
  endCbImpl,
  endImpl,
  erroredImpl,
  isPausedImpl,
  newPassThrough,
  pauseImpl,
  pipeCbImpl,
  pipeImpl,
  pipelineImpl,
  readChunkImpl,
  readImpl,
  readSizeImpl,
  readableEndedImpl,
  readableFlowingImpl,
  readableFromBufImpl,
  readableFromStrImpl,
  readableHighWaterMarkImpl,
  readableImpl,
  readableLengthImpl,
  resumeImpl,
  setDefaultEncodingImpl,
  setEncodingImpl,
  uncorkImpl,
  unpipeAllImpl,
  unpipeImpl,
  writeCbImpl,
  writeImpl,
  writeStringCbImpl,
  writeStringImpl,
  writeableCorkedImpl,
  writeableEndedImpl,
  writeableFinishedImpl,
  writeableHighWaterMarkImpl,
  writeableImpl,
  writeableLengthImpl,
  writeableNeedDrainImpl
} from "./foreign.js";
const identity = x => x;
const writeableNeedDrain = w => () => writeableNeedDrainImpl(w);
const writeableLength = w => () => writeableLengthImpl(w);
const writeableHighWaterMark = w => () => writeableHighWaterMarkImpl(w);
const writeableFinished = w => () => writeableFinishedImpl(w);
const writeableEnded = w => () => writeableEndedImpl(w);
const writeableCorked = w => () => writeableCorkedImpl(w);
const writeable = w => () => writeableImpl(w);
const writeString$p = w => enc => str => cb => {
  const $0 = (() => {
    if (enc === "ASCII") { return "ascii"; }
    if (enc === "UTF8") { return "utf8"; }
    if (enc === "UTF16LE") { return "utf16le"; }
    if (enc === "UCS2") { return "ucs2"; }
    if (enc === "Base64") { return "base64"; }
    if (enc === "Base64Url") { return "base64url"; }
    if (enc === "Latin1") { return "latin1"; }
    if (enc === "Binary") { return "binary"; }
    if (enc === "Hex") { return "hex"; }
    $runtime.fail();
  })();
  return () => writeStringCbImpl(w, str, $0, err => cb(Data$dNullable.nullable(err, Data$dMaybe.Nothing, Data$dMaybe.Just))());
};
const writeString = w => enc => str => {
  const $0 = (() => {
    if (enc === "ASCII") { return "ascii"; }
    if (enc === "UTF8") { return "utf8"; }
    if (enc === "UTF16LE") { return "utf16le"; }
    if (enc === "UCS2") { return "ucs2"; }
    if (enc === "Base64") { return "base64"; }
    if (enc === "Base64Url") { return "base64url"; }
    if (enc === "Latin1") { return "latin1"; }
    if (enc === "Binary") { return "binary"; }
    if (enc === "Hex") { return "hex"; }
    $runtime.fail();
  })();
  return () => writeStringImpl(w, str, $0);
};
const write$p = w => b => cb => () => writeCbImpl(w, b, err => cb(Data$dNullable.nullable(err, Data$dMaybe.Nothing, Data$dMaybe.Just))());
const write = w => b => () => writeImpl(w, b);
const unpipeH = /* #__PURE__ */ Node$dEventEmitter.$EventHandle("unpipe", Effect$dUncurried.mkEffectFn1);
const unpipeAll = r => () => unpipeAllImpl(r);
const unpipe = r => w => () => unpipeImpl(r, w);
const uncork = w => () => uncorkImpl(w);
const toEventEmitter = Unsafe$dCoerce.unsafeCoerce;
const setEncoding = r => enc => {
  const $0 = (() => {
    if (enc === "ASCII") { return "ASCII"; }
    if (enc === "UTF8") { return "UTF8"; }
    if (enc === "UTF16LE") { return "UTF16LE"; }
    if (enc === "UCS2") { return "UCS2"; }
    if (enc === "Base64") { return "Base64"; }
    if (enc === "Base64Url") { return "Base64Url"; }
    if (enc === "Latin1") { return "Latin1"; }
    if (enc === "Binary") { return "Binary"; }
    if (enc === "Hex") { return "Hex"; }
    $runtime.fail();
  })();
  return () => setEncodingImpl(r, $0);
};
const setDefaultEncoding = r => enc => {
  const $0 = (() => {
    if (enc === "ASCII") { return "ASCII"; }
    if (enc === "UTF8") { return "UTF8"; }
    if (enc === "UTF16LE") { return "UTF16LE"; }
    if (enc === "UCS2") { return "UCS2"; }
    if (enc === "Base64") { return "Base64"; }
    if (enc === "Base64Url") { return "Base64Url"; }
    if (enc === "Latin1") { return "Latin1"; }
    if (enc === "Binary") { return "Binary"; }
    if (enc === "Hex") { return "Hex"; }
    $runtime.fail();
  })();
  return () => setDefaultEncodingImpl(r, $0);
};
const resumeH = /* #__PURE__ */ Node$dEventEmitter.$EventHandle("resume", identity);
const resume = r => () => resumeImpl(r);
const readableLength = r => () => readableLengthImpl(r);
const readableHighWaterMark = r => () => readableHighWaterMarkImpl(r);
const readableH = /* #__PURE__ */ Node$dEventEmitter.$EventHandle("readable", identity);
const readableFromString = str => enc => {
  const $0 = (() => {
    if (enc === "ASCII") { return "ascii"; }
    if (enc === "UTF8") { return "utf8"; }
    if (enc === "UTF16LE") { return "utf16le"; }
    if (enc === "UCS2") { return "ucs2"; }
    if (enc === "Base64") { return "base64"; }
    if (enc === "Base64Url") { return "base64url"; }
    if (enc === "Latin1") { return "latin1"; }
    if (enc === "Binary") { return "binary"; }
    if (enc === "Hex") { return "hex"; }
    $runtime.fail();
  })();
  return () => readableFromStrImpl(str, $0);
};
const readableFromBuffer = buf => () => readableFromBufImpl(buf);
const readableFlowing = r => () => readableFlowingImpl(r);
const readableEnded = r => () => readableEndedImpl(r);
const readable = r => () => readableImpl(r);
const readEither$p = r => size => () => {
  const chunk = readSizeImpl(r, size);
  const v = Data$dNullable.nullable(chunk, Data$dMaybe.Nothing, Data$dMaybe.Just);
  if (v.tag === "Nothing") { return Data$dMaybe.Nothing; }
  if (v.tag === "Just") {
    return readChunkImpl($0 => Data$dMaybe.$Maybe("Just", Data$dEither.$Either("Right", $0)), $0 => Data$dMaybe.$Maybe("Just", Data$dEither.$Either("Left", $0)), v._1);
  }
  $runtime.fail();
};
const readEither = r => () => {
  const chunk = readImpl(r);
  const v = Data$dNullable.nullable(chunk, Data$dMaybe.Nothing, Data$dMaybe.Just);
  if (v.tag === "Nothing") { return Data$dMaybe.Nothing; }
  if (v.tag === "Just") {
    return readChunkImpl($0 => Data$dMaybe.$Maybe("Just", Data$dEither.$Either("Right", $0)), $0 => Data$dMaybe.$Maybe("Just", Data$dEither.$Either("Left", $0)), v._1);
  }
  $runtime.fail();
};
const read$p = r => size => () => {
  const chunk = readSizeImpl(r, size);
  const v = Data$dNullable.nullable(chunk, Data$dMaybe.Nothing, Data$dMaybe.Just);
  if (v.tag === "Nothing") { return Data$dMaybe.Nothing; }
  if (v.tag === "Just") {
    return readChunkImpl(buf => Data$dMaybe.$Maybe("Just", buf), v1 => Effect$dException.throwException(Effect$dException.error("Stream encoding should not be set"))(), v._1);
  }
  $runtime.fail();
};
const readString$p = r => size => enc => {
  const $0 = read$p(r)(size);
  return () => {
    const mbBuf = $0();
    if (mbBuf.tag === "Nothing") { return Data$dMaybe.Nothing; }
    if (mbBuf.tag === "Just") { return Data$dMaybe.$Maybe("Just", Node$dBuffer$dImmutable.toString(enc)(mbBuf._1)); }
    $runtime.fail();
  };
};
const read = r => () => {
  const chunk = readImpl(r);
  const v = Data$dNullable.nullable(chunk, Data$dMaybe.Nothing, Data$dMaybe.Just);
  if (v.tag === "Nothing") { return Data$dMaybe.Nothing; }
  if (v.tag === "Just") {
    return readChunkImpl(buf => Data$dMaybe.$Maybe("Just", buf), v1 => Effect$dException.throwException(Effect$dException.error("Stream encoding should not be set"))(), v._1);
  }
  $runtime.fail();
};
const readString = r => enc => {
  const $0 = read(r);
  return () => {
    const mbBuf = $0();
    if (mbBuf.tag === "Nothing") { return Data$dMaybe.Nothing; }
    if (mbBuf.tag === "Just") { return Data$dMaybe.$Maybe("Just", Node$dBuffer$dImmutable.toString(enc)(mbBuf._1)); }
    $runtime.fail();
  };
};
const pipeline = src => transforms => dest => cb => () => pipelineImpl(src, transforms, dest, err => cb(Data$dNullable.nullable(err, Data$dMaybe.Nothing, Data$dMaybe.Just))());
const pipeH = /* #__PURE__ */ Node$dEventEmitter.$EventHandle("pipe", Effect$dUncurried.mkEffectFn1);
const pipe$p = r => w => o => () => pipeCbImpl(r, w, o);
const pipe = r => w => () => pipeImpl(r, w);
const pauseH = /* #__PURE__ */ Node$dEventEmitter.$EventHandle("pause", identity);
const pause = r => () => pauseImpl(r);
const isPaused = r => () => isPausedImpl(r);
const finishH = /* #__PURE__ */ Node$dEventEmitter.$EventHandle("finish", identity);
const errored = rw => () => erroredImpl(rw);
const errorH = /* #__PURE__ */ Node$dEventEmitter.$EventHandle("error", Effect$dUncurried.mkEffectFn1);
const endH = /* #__PURE__ */ Node$dEventEmitter.$EventHandle("end", identity);
const end$p = w => cb => () => endCbImpl(w, err => cb(Data$dNullable.nullable(err, Data$dMaybe.Nothing, Data$dMaybe.Just))());
const end = w => () => endImpl(w);
const drainH = /* #__PURE__ */ Node$dEventEmitter.$EventHandle("drain", identity);
const destroyed = w => () => destroyedImpl(w);
const destroy$p = w => e => () => destroyErrorImpl(w, e);
const destroy = w => () => destroyImpl(w);
const dataHStr = /* #__PURE__ */ Node$dEventEmitter.$EventHandle(
  "data",
  cb => chunk => readChunkImpl(
    v => Effect$dException.throwException(Effect$dException.error("Got a Buffer, not String. Stream encoding must be set to get a String."))(),
    $0 => cb($0)(),
    chunk
  )
);
const dataHEither = /* #__PURE__ */ Node$dEventEmitter.$EventHandle(
  "data",
  cb => chunk => readChunkImpl(x => cb(Data$dEither.$Either("Left", x))(), x => cb(Data$dEither.$Either("Right", x))(), chunk)
);
const dataH = /* #__PURE__ */ Node$dEventEmitter.$EventHandle(
  "data",
  cb => chunk => readChunkImpl(
    $0 => cb($0)(),
    v => Effect$dException.throwException(Effect$dException.error("Got a String, not a Buffer. Stream encoding should not be set"))(),
    chunk
  )
);
const cork = s => () => corkImpl(s);
const closed = w => () => closedImpl(w);
const closeH = /* #__PURE__ */ Node$dEventEmitter.$EventHandle("close", identity);
const allowHalfOpen = d => () => allowHalfOpenImpl(d);
export {
  allowHalfOpen,
  closeH,
  closed,
  cork,
  dataH,
  dataHEither,
  dataHStr,
  destroy,
  destroy$p,
  destroyed,
  drainH,
  end,
  end$p,
  endH,
  errorH,
  errored,
  finishH,
  identity,
  isPaused,
  pause,
  pauseH,
  pipe,
  pipe$p,
  pipeH,
  pipeline,
  read,
  read$p,
  readEither,
  readEither$p,
  readString,
  readString$p,
  readable,
  readableEnded,
  readableFlowing,
  readableFromBuffer,
  readableFromString,
  readableH,
  readableHighWaterMark,
  readableLength,
  resume,
  resumeH,
  setDefaultEncoding,
  setEncoding,
  toEventEmitter,
  uncork,
  unpipe,
  unpipeAll,
  unpipeH,
  write,
  write$p,
  writeString,
  writeString$p,
  writeable,
  writeableCorked,
  writeableEnded,
  writeableFinished,
  writeableHighWaterMark,
  writeableLength,
  writeableNeedDrain
};
export * from "./foreign.js";
