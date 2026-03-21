import {
  createReadStreamImpl,
  createReadStreamOptsImpl,
  createWriteStreamImpl,
  createWriteStreamOptsImpl,
  fdCreateReadStreamImpl,
  fdCreateReadStreamOptsImpl,
  fdCreateWriteStreamImpl,
  fdCreateWriteStreamOptsImpl
} from "./foreign.js";
const fdCreateWriteStream$p = () => f => opts => () => fdCreateWriteStreamOptsImpl(f, opts);
const fdCreateWriteStream = f => () => fdCreateWriteStreamImpl(f);
const fdCreateReadStream$p = () => f => opts => () => fdCreateReadStreamOptsImpl(f, opts);
const fdCreateReadStream = f => () => fdCreateReadStreamImpl(f);
const createWriteStream$p = () => f => opts => () => createWriteStreamOptsImpl(f, opts);
const createWriteStream = f => () => createWriteStreamImpl(f);
const createReadStream$p = () => path => opts => () => createReadStreamOptsImpl(path, opts);
const createReadStream = p => () => createReadStreamImpl(p);
export {createReadStream, createReadStream$p, createWriteStream, createWriteStream$p, fdCreateReadStream, fdCreateReadStream$p, fdCreateWriteStream, fdCreateWriteStream$p};
export * from "./foreign.js";
