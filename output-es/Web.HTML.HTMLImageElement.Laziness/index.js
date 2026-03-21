import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
const $Laziness = tag => tag;
const Eager = /* #__PURE__ */ $Laziness("Eager");
const Lazy = /* #__PURE__ */ $Laziness("Lazy");
const showDecodingHint = {
  show: v => {
    if (v === "Eager") { return "Eager"; }
    if (v === "Lazy") { return "Lazy"; }
    $runtime.fail();
  }
};
const print = v => {
  if (v === "Eager") { return "eager"; }
  if (v === "Lazy") { return "lazy"; }
  $runtime.fail();
};
const parse = v => {
  if (v === "") { return Data$dMaybe.$Maybe("Just", Eager); }
  if (v === "eager") { return Data$dMaybe.$Maybe("Just", Eager); }
  if (v === "lazy") { return Data$dMaybe.$Maybe("Just", Lazy); }
  return Data$dMaybe.Nothing;
};
const eqDecodingHint = {
  eq: x => y => {
    if (x === "Eager") { return y === "Eager"; }
    return x === "Lazy" && y === "Lazy";
  }
};
const ordDecodingHint = {
  compare: x => y => {
    if (x === "Eager") {
      if (y === "Eager") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Eager") { return Data$dOrdering.GT; }
    if (x === "Lazy" && y === "Lazy") { return Data$dOrdering.EQ; }
    $runtime.fail();
  },
  Eq0: () => eqDecodingHint
};
export {$Laziness, Eager, Lazy, eqDecodingHint, ordDecodingHint, parse, print, showDecodingHint};
