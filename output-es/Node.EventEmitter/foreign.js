import EventEmitter from "node:events";

const newImpl = function () {
  return new EventEmitter();
}
export { newImpl as new };

// addEventListener - not implemented; alias to `on`
export const unsafeEmitFn = (emitter) => emitter.emit.bind(emitter);
export const eventNamesImpl = (emitter) => emitter.eventNames();
export const symbolOrStr = (left, right, sym) => typeof sym == "symbol" ? left(sym) : right(sym);
export const getMaxListenersImpl = (emitter) => emitter.getMaxListeners();
export const listenerCountImpl = (emitter, eventName) => emitter.listenerCount(eventName);
// listeners - not implemented; returned functions cannot be used in type-safe way.
export const unsafeOff = (emitter, eventName, cb) => emitter.off(eventName, cb);
export const unsafeOn = (emitter, eventName, cb) => emitter.on(eventName, cb);
export const unsafeOnce = (emitter, eventName, cb) => emitter.once(eventName, cb);
export const unsafePrependListener = (emitter, eventName, cb) => emitter.prependListener(eventName, cb);
export const unsafePrependOnceListener = (emitter, eventName, cb) => emitter.prependOnceListener(eventName, cb);
// removeAllListeners - not implemented; bad practice
// removeEventListener - not implemented; alias to `off`
export const setMaxListenersImpl = (emitter, max) => emitter.setMaxListeners(max);
// rawListeners - not implemented; returned functions cannot be used in type-safe way.

