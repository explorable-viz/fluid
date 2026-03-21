import * as Effect$dException from "../Effect.Exception/index.js";
const unsafeThrowException = x => Effect$dException.throwException(x)();
const unsafeThrow = x => Effect$dException.throwException(Effect$dException.error(x))();
export {unsafeThrow, unsafeThrowException};
