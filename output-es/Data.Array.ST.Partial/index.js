// | Partial functions for working with mutable arrays using the `ST` effect.
// |
// | This module is particularly helpful when performance is very important.
import * as Control$dMonad$dST$dUncurried from "../Control.Monad.ST.Uncurried/index.js";
import {peekImpl, pokeImpl} from "./foreign.js";
const poke = () => Control$dMonad$dST$dUncurried.runSTFn3(pokeImpl);
const peek = () => Control$dMonad$dST$dUncurried.runSTFn2(peekImpl);
export {peek, poke};
export * from "./foreign.js";
