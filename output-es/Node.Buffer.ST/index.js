import * as Control$dMonad$dST$dInternal from "../Control.Monad.ST.Internal/index.js";
import * as Node$dBuffer from "../Node.Buffer/index.js";
import * as Node$dBuffer$dImmutable from "../Node.Buffer.Immutable/index.js";
const writeString = Node$dBuffer.writeString;
const write = Node$dBuffer.write;
const unsafeThaw = Node$dBuffer.unsafeThaw;
const unsafeFreeze = Node$dBuffer.unsafeFreeze;
const transcode = Node$dBuffer.transcode;
const toString$p = Node$dBuffer.toString$p;
const toString = Node$dBuffer.toString;
const toArrayBuffer = Node$dBuffer.toArrayBuffer;
const toArray = Node$dBuffer.toArray;
const thaw = Node$dBuffer.thaw;
const swap64 = Node$dBuffer.swap64;
const swap32 = Node$dBuffer.swap32;
const swap16 = Node$dBuffer.swap16;
const slice = Node$dBuffer$dImmutable.slice;
const size = Node$dBuffer.size;
const setAtOffset = Node$dBuffer.setAtOffset;
const run = st => st();
const readString = Node$dBuffer.readString;
const read = Node$dBuffer.read;
const getAtOffset = Node$dBuffer.getAtOffset;
const fromString = Node$dBuffer.fromString;
const fromArrayBuffer = Node$dBuffer.fromArrayBuffer;
const fromArray = Node$dBuffer.fromArray;
const freeze = Node$dBuffer.freeze;
const fill = Node$dBuffer.fill;
const copy = Node$dBuffer.copy;
const concat$p = Node$dBuffer.concat$p;
const concat = Node$dBuffer.concat;
const compareParts = Node$dBuffer.compareParts;
const allocUnsafeSlow = Node$dBuffer.allocUnsafeSlow;
const allocUnsafe = Node$dBuffer.allocUnsafe;
const alloc = Node$dBuffer.alloc;
const create = Node$dBuffer.alloc;
const mutableBufferST = {
  create: Node$dBuffer.alloc,
  freeze: Node$dBuffer.freeze,
  unsafeFreeze: Node$dBuffer.unsafeFreeze,
  thaw: Node$dBuffer.thaw,
  unsafeThaw: Node$dBuffer.unsafeThaw,
  fromArray: Node$dBuffer.fromArray,
  fromString: Node$dBuffer.fromString,
  fromArrayBuffer: Node$dBuffer.fromArrayBuffer,
  toArrayBuffer: Node$dBuffer.toArrayBuffer,
  read: Node$dBuffer.read,
  readString: Node$dBuffer.readString,
  toString: Node$dBuffer.toString,
  write: Node$dBuffer.write,
  writeString: Node$dBuffer.writeString,
  toArray: Node$dBuffer.toArray,
  getAtOffset: Node$dBuffer.getAtOffset,
  setAtOffset: Node$dBuffer.setAtOffset,
  slice: Node$dBuffer$dImmutable.slice,
  size: Node$dBuffer.size,
  concat: Node$dBuffer.concat,
  "concat'": Node$dBuffer.concat$p,
  copy: Node$dBuffer.copy,
  fill: Node$dBuffer.fill,
  Monad0: () => Control$dMonad$dST$dInternal.monadST
};
export {
  alloc,
  allocUnsafe,
  allocUnsafeSlow,
  compareParts,
  concat,
  concat$p,
  copy,
  create,
  fill,
  freeze,
  fromArray,
  fromArrayBuffer,
  fromString,
  getAtOffset,
  mutableBufferST,
  read,
  readString,
  run,
  setAtOffset,
  size,
  slice,
  swap16,
  swap32,
  swap64,
  thaw,
  toArray,
  toArrayBuffer,
  toString,
  toString$p,
  transcode,
  unsafeFreeze,
  unsafeThaw,
  write,
  writeString
};
