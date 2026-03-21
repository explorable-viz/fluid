const writeString = dict => dict.writeString;
const write = dict => dict.write;
const unsafeThaw = dict => dict.unsafeThaw;
const unsafeFreeze = dict => dict.unsafeFreeze;
const toString = dict => dict.toString;
const toArrayBuffer = dict => dict.toArrayBuffer;
const toArray = dict => dict.toArray;
const thaw = dict => dict.thaw;
const slice = dict => dict.slice;
const size = dict => dict.size;
const setAtOffset = dict => dict.setAtOffset;
const readString = dict => dict.readString;
const read = dict => dict.read;
const getAtOffset = dict => dict.getAtOffset;
const fromString = dict => dict.fromString;
const fromArrayBuffer = dict => dict.fromArrayBuffer;
const fromArray = dict => dict.fromArray;
const freeze = dict => dict.freeze;
const fill = dict => dict.fill;
const create = dict => dict.create;
const copy = dict => dict.copy;
const concat$p = dict => dict["concat'"];
const concat = dict => dict.concat;
export {
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
  read,
  readString,
  setAtOffset,
  size,
  slice,
  thaw,
  toArray,
  toArrayBuffer,
  toString,
  unsafeFreeze,
  unsafeThaw,
  write,
  writeString
};
