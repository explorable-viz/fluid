import { Buffer, transcode } from "node:buffer";

export const allocUnsafeImpl = (size) => Buffer.allocUnsafe(size);
export const allocUnsafeSlowImpl = (size) => Buffer.allocUnsafeSlow(size);

export const freezeImpl = (a) => Buffer.from(a);
export const thawImpl = (a) => Buffer.from(a);

export const writeInternal = (ty, value, offset, buf) => buf["write" + ty](value, offset);

export const writeStringInternal = (encoding, offset, length, value, buff) =>
  buff.write(value, offset, length, encoding);

export const setAtOffsetImpl = (value, offset, buff) => {
  buff[offset] = value;
};

export const copyImpl = (srcStart, srcEnd, src, targStart, targ) =>
  src.copy(targ, targStart, srcStart, srcEnd);

export const fillImpl = (octet, start, end, buf) =>
  buf.fill(octet, start, end);

export const poolSize = () => Buffer.poolSize;

export const setPoolSizeImpl = (size) => { 
  Buffer.poolSize = size; 
};

export const swap16Impl = (buf) => buf.swap16();
export const swap32Impl = (buf) => buf.swap32();
export const swap64Impl = (buf) => buf.swap64();
export const transcodeImpl = (buf, from, to) => transcode(buf, from, to);
