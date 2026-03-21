import * as $runtime from "../runtime.js";
import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
import {assertImpl, checkThrows} from "./foreign.js";
const assert$p = assertImpl;
const assertEqual$p = dictEq => dictShow => userMessage => v => {
  const result = dictEq.eq(v.actual)(v.expected);
  const message = (userMessage === "" ? "Expected: " : userMessage + "\nExpected: ") + dictShow.show(v.expected) + "\nActual:   " + dictShow.show(v.actual);
  const $0 = Effect$dConsole.error(message);
  const $1 = (() => {
    if (!result) { return $0; }
    if (result) { return () => {}; }
    $runtime.fail();
  })();
  return () => {
    $1();
    return assertImpl(message)(result)();
  };
};
const assertEqual = dictEq => dictShow => assertEqual$p(dictEq)(dictShow)("");
const assertFalse = actual => assertEqual$p(Data$dEq.eqBoolean)(Data$dShow.showBoolean)("")({actual, expected: false});
const assertTrue = actual => assertEqual$p(Data$dEq.eqBoolean)(Data$dShow.showBoolean)("")({actual, expected: true});
const assertFalse$p = message => actual => assertEqual$p(Data$dEq.eqBoolean)(Data$dShow.showBoolean)(message)({actual, expected: false});
const assertTrue$p = message => actual => assertEqual$p(Data$dEq.eqBoolean)(Data$dShow.showBoolean)(message)({actual, expected: true});
const assertThrows$p = msg => fn => {
  const $0 = assertImpl(msg);
  const $1 = checkThrows(fn);
  return () => {
    const $2 = $1();
    return $0($2)();
  };
};
const assertThrows = /* #__PURE__ */ assertThrows$p("Assertion failed: An error should have been thrown");
const assert = /* #__PURE__ */ assertImpl("Assertion failed");
export {assert, assert$p, assertEqual, assertEqual$p, assertFalse, assertFalse$p, assertThrows, assertThrows$p, assertTrue, assertTrue$p};
export * from "./foreign.js";
