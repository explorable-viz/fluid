import { Buffer } from "node:buffer";

export const byteLengthImpl = (str, enc) => Buffer.byteLength(str, enc);
