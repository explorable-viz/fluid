// | Helper functions for working with mutable arrays using the `ST` effect.
// |
// | This module can be used when performance is important and mutation is a local effect.
import * as $runtime from "../runtime.js";
import * as Control$dMonad$dST$dUncurried from "../Control.Monad.ST.Uncurried/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import {
  new as $$new,
  freezeImpl,
  lengthImpl,
  peekImpl,
  pokeImpl,
  popImpl,
  pushAllImpl,
  shiftImpl,
  sortByImpl,
  spliceImpl,
  thawImpl,
  toAssocArrayImpl,
  unsafeFreezeImpl,
  unsafeThawImpl,
  unshiftAllImpl
} from "./foreign.js";
const unshiftAll = /* #__PURE__ */ Control$dMonad$dST$dUncurried.runSTFn2(unshiftAllImpl);
const unshift = a => Control$dMonad$dST$dUncurried.runSTFn2(unshiftAllImpl)([a]);
const unsafeThaw = /* #__PURE__ */ Control$dMonad$dST$dUncurried.runSTFn1(unsafeThawImpl);
const unsafeFreeze = /* #__PURE__ */ Control$dMonad$dST$dUncurried.runSTFn1(unsafeFreezeImpl);
const toAssocArray = /* #__PURE__ */ Control$dMonad$dST$dUncurried.runSTFn1(toAssocArrayImpl);
const thaw = /* #__PURE__ */ Control$dMonad$dST$dUncurried.runSTFn1(thawImpl);
const withArray = f => xs => () => {
  const result = [...xs];
  f(result)();
  return result;
};
const splice = /* #__PURE__ */ Control$dMonad$dST$dUncurried.runSTFn4(spliceImpl);
const sortBy = comp => Control$dMonad$dST$dUncurried.runSTFn3(sortByImpl)(comp)(v => {
  if (v === "GT") { return 1; }
  if (v === "EQ") { return 0; }
  if (v === "LT") { return -1; }
  $runtime.fail();
});
const sortWith = dictOrd => f => sortBy(x => y => dictOrd.compare(f(x))(f(y)));
const sort = dictOrd => sortBy(dictOrd.compare);
const shift = /* #__PURE__ */ Control$dMonad$dST$dUncurried.runSTFn3(shiftImpl)(Data$dMaybe.Just)(Data$dMaybe.Nothing);
const run = st => st();
const pushAll = /* #__PURE__ */ Control$dMonad$dST$dUncurried.runSTFn2(pushAllImpl);
const push = a => Control$dMonad$dST$dUncurried.runSTFn2(pushAllImpl)([a]);
const pop = /* #__PURE__ */ Control$dMonad$dST$dUncurried.runSTFn3(popImpl)(Data$dMaybe.Just)(Data$dMaybe.Nothing);
const poke = /* #__PURE__ */ Control$dMonad$dST$dUncurried.runSTFn3(pokeImpl);
const peek = /* #__PURE__ */ Control$dMonad$dST$dUncurried.runSTFn4(peekImpl)(Data$dMaybe.Just)(Data$dMaybe.Nothing);
const modify = i => f => xs => () => {
  const entry = peekImpl(Data$dMaybe.Just, Data$dMaybe.Nothing, i, xs);
  if (entry.tag === "Just") { return pokeImpl(i, f(entry._1), xs); }
  if (entry.tag === "Nothing") { return false; }
  $runtime.fail();
};
const length = /* #__PURE__ */ Control$dMonad$dST$dUncurried.runSTFn1(lengthImpl);
const freeze = /* #__PURE__ */ Control$dMonad$dST$dUncurried.runSTFn1(freezeImpl);
export {
  
  
  modify,
  
  
  
  push,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
};
export * from "./foreign.js";
