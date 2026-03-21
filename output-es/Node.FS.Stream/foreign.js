import fs from "node:fs";

export const createReadStreamImpl = (path) => fs.createReadStream(path);
export const createReadStreamOptsImpl = (path, opts) => fs.createReadStream(path, opts);
export const fdCreateReadStreamImpl = (fd) => fs.createReadStream(null, { fd });
export const fdCreateReadStreamOptsImpl = (fd, opts) => fs.createReadStream(null, { ...opts, fd});

export const createWriteStreamImpl = (path) => fs.createWriteStream(path);
export const createWriteStreamOptsImpl = (path, opts) => fs.createWriteStream(path, opts);
export const fdCreateWriteStreamImpl = (fd) => fs.createWriteStream(null, { fd });
export const fdCreateWriteStreamOptsImpl = (fd, opts) => fs.createWriteStream(null, { ...opts, fd});
