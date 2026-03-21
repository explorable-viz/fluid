import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
const $CORSMode = tag => tag;
const Anonymous = /* #__PURE__ */ $CORSMode("Anonymous");
const UseCredentials = /* #__PURE__ */ $CORSMode("UseCredentials");
const showDecodingHint = {
  show: v => {
    if (v === "Anonymous") { return "Anonymous"; }
    if (v === "UseCredentials") { return "UseCredentials"; }
    $runtime.fail();
  }
};
const print = v => {
  if (v === "Anonymous") { return "anonymous"; }
  if (v === "UseCredentials") { return "use-credentials"; }
  $runtime.fail();
};
const parse = v => {
  if (v === "") { return Data$dMaybe.$Maybe("Just", Anonymous); }
  if (v === "anonymous") { return Data$dMaybe.$Maybe("Just", Anonymous); }
  if (v === "use-credentials") { return Data$dMaybe.$Maybe("Just", UseCredentials); }
  return Data$dMaybe.Nothing;
};
const eqCORSMode = {
  eq: x => y => {
    if (x === "Anonymous") { return y === "Anonymous"; }
    return x === "UseCredentials" && y === "UseCredentials";
  }
};
const ordCORSMode = {
  compare: x => y => {
    if (x === "Anonymous") {
      if (y === "Anonymous") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Anonymous") { return Data$dOrdering.GT; }
    if (x === "UseCredentials" && y === "UseCredentials") { return Data$dOrdering.EQ; }
    $runtime.fail();
  },
  Eq0: () => eqCORSMode
};
export {$CORSMode, Anonymous, UseCredentials, eqCORSMode, ordCORSMode, parse, print, showDecodingHint};
