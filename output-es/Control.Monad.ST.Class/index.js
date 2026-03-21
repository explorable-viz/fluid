import * as Control$dMonad$dST$dInternal from "../Control.Monad.ST.Internal/index.js";
import * as Effect from "../Effect/index.js";
import * as Unsafe$dCoerce from "../Unsafe.Coerce/index.js";
const monadSTST = {liftST: x => x, Monad0: () => Control$dMonad$dST$dInternal.monadST};
const monadSTEffect = {liftST: Unsafe$dCoerce.unsafeCoerce, Monad0: () => Effect.monadEffect};
const liftST = dict => dict.liftST;
export {liftST, monadSTEffect, monadSTST};
