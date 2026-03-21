import { Buffer } from "node:buffer";

import { inspect } from "util";
export const showImpl = inspect;

export const eqImpl = (a, b) => a.equals(b);

export const compareImpl = (a, b) => a.compare(b);

export const comparePartsImpl = (src, target, targetStart, targetEnd, sourceStart, sourceEnd) => 
  src.compare(target, targetStart, targetEnd, sourceStart, sourceEnd);

export const alloc = (size) => Buffer.alloc(size);

export const fromArray = (octets) => Buffer.from(octets);

export const size = (buff) => buff.length;

export function toArray(buff) {
  var json = buff.toJSON();
  return json.data || json;
}

export const toArrayBuffer = (buff) =>
  buff.buffer.slice(buff.byteOffset, buff.byteOffset + buff.byteLength);

export const fromArrayBuffer = (ab) => Buffer.from(ab);

export const fromStringImpl = (str, encoding) => Buffer.from(str, encoding);

export const readImpl = (ty, offset, buf) => buf["read" + ty](offset);

export const readStringImpl = (enc, start, end, buff) => buff.toString(enc, start, end);

export const getAtOffsetImpl = (offset, buff) => buff[offset];

export const toStringImpl = (enc, buff) => buff.toString(enc);

export const toStringSubImpl = (enc, start, end, buff) => buff.toString(enc, start, end);

export const sliceImpl = (start, end, buff) => buff.slice(start, end);

export const concat = (buffs) => Buffer.concat(buffs);

export const concatToLength = (buffs, totalLength) => Buffer.concat(buffs, totalLength);
