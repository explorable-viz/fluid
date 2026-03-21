// | Immutable buffers and associated operations.
import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNullable from "../Data.Nullable/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Partial from "../Partial/index.js";
import {
  alloc,
  compareImpl,
  comparePartsImpl,
  concat,
  concatToLength,
  eqImpl,
  fromArray,
  fromArrayBuffer,
  fromStringImpl,
  getAtOffsetImpl,
  readImpl,
  readStringImpl,
  showImpl,
  size,
  sliceImpl,
  toArray,
  toArrayBuffer,
  toStringImpl,
  toStringSubImpl
} from "./foreign.js";
const toString$p = enc => start => end => buf => toStringSubImpl(enc, start, end, buf);
const toString = enc => buf => toStringImpl(
  (() => {
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
  })(),
  buf
);
const slice = start => end => buf => sliceImpl(start, end, buf);
const showBuffer = {show: showImpl};
const readString = enc => start => end => buf => readStringImpl(
  (() => {
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
  })(),
  start,
  end,
  buf
);
const read = ty => off => buf => readImpl(
  (() => {
    if (ty === "UInt8") { return "UInt8"; }
    if (ty === "UInt16LE") { return "UInt16LE"; }
    if (ty === "UInt16BE") { return "UInt16BE"; }
    if (ty === "UInt32LE") { return "UInt32LE"; }
    if (ty === "UInt32BE") { return "UInt32BE"; }
    if (ty === "Int8") { return "Int8"; }
    if (ty === "Int16LE") { return "Int16LE"; }
    if (ty === "Int16BE") { return "Int16BE"; }
    if (ty === "Int32LE") { return "Int32LE"; }
    if (ty === "Int32BE") { return "Int32BE"; }
    if (ty === "FloatLE") { return "FloatLE"; }
    if (ty === "FloatBE") { return "FloatBE"; }
    if (ty === "DoubleLE") { return "DoubleLE"; }
    if (ty === "DoubleBE") { return "DoubleBE"; }
    $runtime.fail();
  })(),
  off,
  buf
);
const getAtOffset = off => buf => Data$dNullable.nullable(getAtOffsetImpl(off, buf), Data$dMaybe.Nothing, Data$dMaybe.Just);
const fromString = str => enc => fromStringImpl(
  str,
  (() => {
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
  })()
);
const eqBuffer = {eq: a => b => eqImpl(a, b)};
const ordBuffer = {
  compare: a => b => {
    const v = compareImpl(a, b);
    if (v < 0) { return Data$dOrdering.LT; }
    if (v > 0) { return Data$dOrdering.GT; }
    return Data$dOrdering.EQ;
  },
  Eq0: () => eqBuffer
};
const create = alloc;
const concat$p = a => i => concatToLength(a, i);
const compareParts = src => target => targetStart => targetEnd => sourceStart => sourceEnd => () => {
  const a$p = comparePartsImpl(src, target, targetStart, targetEnd, sourceStart, sourceEnd);
  if (a$p === -1) { return Data$dOrdering.LT; }
  if (a$p === 0) { return Data$dOrdering.EQ; }
  if (a$p === 1) { return Data$dOrdering.GT; }
  return Partial._crashWith("Impossible: Invalid value: " + Data$dShow.showIntImpl(a$p));
};
export {compareParts, concat$p, create, eqBuffer, fromString, getAtOffset, ordBuffer, read, readString, showBuffer, slice, toString, toString$p};
export * from "./foreign.js";
